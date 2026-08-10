(in-package #:cl-postgresql-kit)

(defun %array-scalar-vector-p (element-oid value)
  (or (and (= element-oid 17)
           (or (typep value 'bytea-value)
               (and (vectorp value)
                    (every (lambda (octet)
                            (and (integerp octet) (<= 0 octet 255)))
                          value))))
      (and (member element-oid '(22 30) :test #'=)
           (vectorp value)
           (plusp (length value))
           (every (lambda (element)
                    (and (integerp element)
                         (if (= element-oid 22)
                             (<= -32768 element 32767)
                             (<= 0 element #xffffffff))))
                  value))))

(defun %array-sequence (value &optional element-oid)
  (cond ((and element-oid (%array-scalar-vector-p element-oid value)) nil)
        ((and (vectorp value) (not (stringp value))) value)
        ((and (consp value) (listp value)) (coerce value 'vector))
        (t nil)))

(defun %array-node-dimensions (value &optional element-oid)
  (let ((root (%array-sequence value element-oid)))
    (unless root
      (error 'parameter-error :parameter value
             :message "PostgreSQL array values must be vectors or non-empty lists")))
  (labels ((walk (node)
             (let ((sequence (%array-sequence node element-oid)))
               (if (null sequence)
                   #()
                   (let ((length (length sequence)))
                     (when (> length #x7fffffff)
                       (error 'parameter-error :parameter value
                              :message "PostgreSQL array dimension is too large"))
                     (if (zerop length)
                         (vector 0)
                         (let ((child (walk (aref sequence 0))))
                           (loop for index from 1 below length
                                 unless (equalp child (walk (aref sequence index)))
                                   do (error 'parameter-error :parameter value
                                             :message "PostgreSQL array dimensions are not rectangular"))
                           (when (>= (length child) *maximum-array-dimensions*)
                             (error 'parameter-error :parameter value
                                    :message "PostgreSQL array has too many dimensions"))
                           (concatenate 'vector (vector length) child))))))))
    (let ((dimensions (walk value)))
      (when (> (length dimensions) *maximum-array-dimensions*)
        (error 'parameter-error :parameter value
               :message "PostgreSQL array has too many dimensions"))
      dimensions)))

(defun %array-element-count (dimensions &key protocolp)
  (let ((count (if (zerop (length dimensions)) 0 1)))
    (loop for dimension across dimensions
          do (unless (and (integerp dimension) (<= 0 dimension #x7fffffff))
               (if protocolp
                   (error 'protocol-error :message "Invalid PostgreSQL array dimension"
                          :context :array :expected ">= 0" :actual dimension)
                   (error 'parameter-error :parameter dimensions
                          :message "PostgreSQL array dimensions must be non-negative 32-bit integers")))
             (setf count (* count dimension))
             (when (> count *maximum-array-elements*)
               (if protocolp
                   (error 'protocol-error :message "PostgreSQL array is too large"
                          :context :array :expected *maximum-array-elements* :actual count)
                   (error 'parameter-error :parameter dimensions
                          :message "PostgreSQL array is too large"))))
    count))

(defun %array-flatten (value &optional element-oid)
  (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
    (labels ((walk (node)
               (let ((sequence (%array-sequence node element-oid)))
                 (if sequence
                     (loop for child across sequence do (walk child))
                     (vector-push-extend node result)))))
      (walk value))
    (coerce result 'vector)))

(defun %array-shape (value element-oid)
  (let* ((wrapper (typep value 'postgres-array))
         (elements (if wrapper (postgres-array-elements value) value))
         (dimensions (%array-node-dimensions elements element-oid))
         (given-dimensions (and wrapper (postgres-array-dimensions value)))
         (given-lower-bounds (and wrapper (postgres-array-lower-bounds value))))
    (when (and wrapper (postgres-array-element-oid value)
               (/= (postgres-array-element-oid value) element-oid))
      (error 'parameter-error :parameter value
             :message "PostgreSQL array element OID does not match its codec"))
    (when (and given-dimensions (not (vectorp given-dimensions)))
      (setf given-dimensions (coerce given-dimensions 'vector)))
    (when (and given-lower-bounds (not (vectorp given-lower-bounds)))
      (setf given-lower-bounds (coerce given-lower-bounds 'vector)))
    (when (and given-dimensions
               (plusp (length given-dimensions))
               (not (equalp given-dimensions dimensions)))
      (error 'parameter-error :parameter value
             :message "PostgreSQL array dimensions do not match its elements"))
    (let ((lower-bounds
            (if (and given-lower-bounds (plusp (length given-lower-bounds)))
                given-lower-bounds
                (make-array (length dimensions) :initial-element 1))))
      (unless (= (length lower-bounds) (length dimensions))
        (error 'parameter-error :parameter value
               :message "PostgreSQL array lower bounds do not match its dimensions"))
      (unless (every (lambda (bound)
                       (and (integerp bound)
                            (<= -2147483648 bound 2147483647)))
                     lower-bounds)
        (error 'parameter-error :parameter value
               :message "PostgreSQL array lower bounds are invalid"))
      (let* ((count (%array-element-count dimensions))
             (flat (%array-flatten elements element-oid)))
        (unless (= count (length flat))
          (error 'parameter-error :parameter value
                 :message "PostgreSQL array element count does not match its dimensions"))
        (values elements dimensions lower-bounds flat)))))

(defun %parse-array-text (string)
  (let ((position 0)
        (length (length string)))
    (labels ((fail (message)
               (error 'protocol-error :message message :context :array
                      :actual position))
             (skip-space ()
               (loop while (and (< position length)
                                (find (char string position)
                                      '(#\Space #\Tab #\Newline #\Return #\Page)))
                     do (incf position)))
             (parse-quoted ()
               (unless (and (< position length)
                            (char= (char string position) #\"))
                 (fail "PostgreSQL array quoted value is malformed"))
               (incf position)
               (with-output-to-string (stream)
                 (loop
                   (when (>= position length)
                     (fail "PostgreSQL array quoted value is unterminated"))
                   (let ((character (char string position)))
                     (cond ((char= character #\\)
                            (incf position)
                            (when (>= position length)
                              (fail "PostgreSQL array escape is unterminated"))
                            (write-char (char string position) stream)
                            (incf position))
                           ((char= character #\")
                            (incf position)
                            (return))
                           (t
                            (write-char character stream)
                            (incf position)))))))
             (parse-unquoted ()
               (let ((start position))
                 (loop while (and (< position length)
                                  (not (member (char string position) '(#\, #\}))))
                       do (incf position))
                 (let ((value (string-trim
                               '(#\Space #\Tab #\Newline #\Return #\Page)
                               (subseq string start position))))
                   (when (zerop (length value))
                     (fail "PostgreSQL array contains an empty unquoted value"))
                   (if (string= value "NULL") +sql-null+ value))))
             (parse-item ()
               (skip-space)
               (when (>= position length)
                 (fail "PostgreSQL array is unterminated"))
               (case (char string position)
                 (#\{ (parse-array))
                 (#\" (parse-quoted))
                 (otherwise (parse-unquoted))))
             (parse-array ()
               (unless (and (< position length)
                            (char= (char string position) #\{))
                 (fail "PostgreSQL array must begin with an opening brace"))
               (incf position)
               (skip-space)
               (if (and (< position length)
                        (char= (char string position) #\}))
                   (progn
                     (incf position)
                     #())
                   (let ((items '()))
                     (loop
                       (push (parse-item) items)
                       (skip-space)
                       (cond ((>= position length)
                              (fail "PostgreSQL array is missing its closing brace"))
                             ((char= (char string position) #\,)
                              (incf position)
                              (skip-space))
                             ((char= (char string position) #\})
                              (incf position)
                              (return (coerce (nreverse items) 'vector)))
                             (t
                              (fail "PostgreSQL array has an invalid separator"))))))))
      (skip-space)
      (let ((value (parse-array)))
        (skip-space)
        (unless (= position length)
          (fail "PostgreSQL array has trailing data"))
        value))))

(defun %decode-array-node (registry element-oid node)
  (let ((sequence (%array-sequence node)))
    (if sequence
        (let ((result (make-array (length sequence))))
          (loop for index below (length sequence)
                do (setf (aref result index)
                         (%decode-array-node registry element-oid
                                             (aref sequence index))))
          result)
        (if (sql-null-p node)
            +sql-null+
            (decode-value registry element-oid (%encode-utf8 node) :format 0)))))

(defun %decode-array-text (registry element-oid octets)
  (let* ((parsed (%parse-array-text (%decode-utf8 octets)))
         (dimensions (%array-node-dimensions parsed))
         (elements (%decode-array-node registry element-oid parsed)))
    (make-postgres-array :elements elements
                         :dimensions dimensions
                         :lower-bounds (make-array (length dimensions)
                                                   :initial-element 1)
                         :element-oid element-oid)))

(defun %array-text-scalar (registry element-oid value)
  (if (sql-null-p value)
      "NULL"
      (let ((payload (encode-value registry element-oid value :format 0)))
        (let ((string (cond ((stringp payload) payload)
                            ((typep payload '(vector (unsigned-byte 8)))
                             (%decode-utf8 payload))
                            (t (princ-to-string payload)))))
          (with-output-to-string (stream)
            (write-char #\" stream)
            (loop for character across string
                  do (when (member character '(#\" #\\))
                       (write-char #\\ stream))
                     (write-char character stream))
            (write-char #\" stream))))))

(defun %write-array-text-node (stream registry element-oid node)
  (let ((sequence (%array-sequence node element-oid)))
    (if sequence
        (progn
          (write-char #\{ stream)
          (loop for index below (length sequence)
                do (when (plusp index) (write-char #\, stream))
                   (%write-array-text-node stream registry element-oid
                                           (aref sequence index)))
          (write-char #\} stream))
        (write-string (%array-text-scalar registry element-oid node) stream))))

(defun %encode-array-text (registry element-oid value)
  (multiple-value-bind (elements dimensions lower-bounds ignored)
      (%array-shape value element-oid)
    (declare (ignore dimensions lower-bounds ignored))
    (%encode-utf8
     (with-output-to-string (stream)
       (%write-array-text-node stream registry element-oid elements)))))

(defun %array-binary-payload (registry element-oid value)
  (let ((payload (encode-value registry element-oid value :format 1)))
    (cond ((typep payload '(vector (unsigned-byte 8))) payload)
          ((stringp payload) (%encode-utf8 payload))
          (t (error 'parameter-error :parameter value
                    :message "PostgreSQL binary array element encoder returned invalid data")))))

(defun %encode-array-binary (registry element-oid value)
  (multiple-value-bind (elements dimensions lower-bounds flat)
      (%array-shape value element-oid)
    (declare (ignore elements))
    (let ((builder (make-octet-builder)))
      (append-i32 builder (length dimensions))
      (append-i32 builder (if (some #'sql-null-p flat) 1 0))
      (append-i32 builder element-oid)
      (loop for dimension across dimensions do (append-i32 builder dimension))
      (loop for lower-bound across lower-bounds do (append-i32 builder lower-bound))
      (loop for element across flat
            do (if (sql-null-p element)
                   (append-i32 builder -1)
                   (let ((payload (%array-binary-payload registry element-oid element)))
                     (append-i32 builder (length payload))
                     (append-octets builder payload))))
      (builder-octets builder))))

(defun %decode-array-binary (registry element-oid octets)
  (let ((position 0)
        (length (length octets)))
    (multiple-value-bind (dimensions-count next) (%read-i32 octets position)
      (setf position next)
      (unless (<= 0 dimensions-count *maximum-array-dimensions*)
        (error 'protocol-error :message "PostgreSQL array dimension count is invalid"
               :context :array :expected (list 0 *maximum-array-dimensions*)
               :actual dimensions-count))
      (multiple-value-bind (has-null next) (%read-i32 octets position)
        (setf position next)
        (unless (member has-null '(0 1))
          (error 'protocol-error :message "PostgreSQL array null flag is invalid"
                 :context :array :expected '(0 1) :actual has-null))
        (multiple-value-bind (wire-element-oid next) (%read-i32 octets position)
          (setf position next)
          (unless (= wire-element-oid element-oid)
            (error 'protocol-error :message "PostgreSQL array element OID does not match its codec"
                   :context :array :expected element-oid :actual wire-element-oid))
          (let ((dimensions (make-array dimensions-count))
                (lower-bounds (make-array dimensions-count)))
            (loop for index below dimensions-count
                  do (multiple-value-bind (dimension next)
                         (%read-i32 octets position)
                       (setf position next (aref dimensions index) dimension)))
            (loop for index below dimensions-count
                  do (multiple-value-bind (lower-bound next)
                         (%read-i32 octets position)
                       (setf position next (aref lower-bounds index) lower-bound)))
            (let* ((count (%array-element-count dimensions :protocolp t))
                   (flat (make-array count)))
              (loop for index below count
                    do (multiple-value-bind (payload-length next)
                           (%read-i32 octets position)
                         (setf position next)
                         (cond ((= payload-length -1)
                                (unless (= has-null 1)
                                  (error 'protocol-error
                                         :message "PostgreSQL array contains an unexpected NULL"
                                         :context :array))
                                (setf (aref flat index) +sql-null+))
                               ((< payload-length -1)
                                (error 'protocol-error
                                       :message "PostgreSQL array element length is invalid"
                                       :context :array :actual payload-length))
                               (t
                                (when (> payload-length (- length position))
                                  (error 'protocol-error
                                         :message "PostgreSQL array element exceeds its payload"
                                         :context :array :expected (- length position)
                                         :actual payload-length))
                                (setf (aref flat index)
                                      (decode-value registry element-oid
                                                    (subseq octets position
                                                            (+ position payload-length))
                                                    :format 1))
                                (incf position payload-length)))))
              (unless (= position length)
                (error 'protocol-error :message "PostgreSQL array has trailing bytes"
                       :context :array :expected position :actual length))
              (labels ((nest (depth offset)
                         (if (= depth dimensions-count)
                             (values (aref flat offset) (1+ offset))
                             (let ((result (make-array (aref dimensions depth))))
                               (loop for index below (length result)
                                     do (multiple-value-bind (value next)
                                            (nest (1+ depth) offset)
                                          (setf (aref result index) value
                                                offset next)))
                               (values result offset)))))
                (let ((elements (if (zerop dimensions-count)
                                    #()
                                    (nest 0 0))))
                  (when (and (plusp dimensions-count)
                             (not (vectorp elements)))
                    (error 'protocol-error :message "PostgreSQL array nesting failed"
                           :context :array))
                  (make-postgres-array :elements elements
                                       :dimensions dimensions
                                       :lower-bounds lower-bounds
                                       :element-oid element-oid))))))))))

(defun %decode-vector-binary (registry element-oid octets)
  (multiple-value-bind (has-null ignored) (%read-i32 octets 4)
    (declare (ignore ignored))
    (let ((array (%decode-array-binary registry element-oid octets)))
      (unless (zerop has-null)
        (error 'protocol-error
               :message "PostgreSQL vector binary values cannot contain NULL"
               :context :type-decoder))
      (let ((dimensions (postgres-array-dimensions array))
            (lower-bounds (postgres-array-lower-bounds array))
            (elements (postgres-array-elements array)))
        (unless (and (= (length dimensions) 1)
                     (= (length lower-bounds) 1)
                     (zerop (aref lower-bounds 0)))
          (error 'protocol-error
                 :message "PostgreSQL vector binary values must be one-dimensional with lower bound zero"
                 :context :type-decoder))
        (when (some #'sql-null-p elements)
          (error 'protocol-error
                 :message "PostgreSQL vector binary values cannot contain NULL"
                 :context :type-decoder))
        elements))))

(defun %encode-vector-binary (registry element-oid value)
  (let* ((wrapper-p (typep value 'postgres-array))
         (elements (if wrapper-p
                       (postgres-array-elements value)
                       value))
         (normalized-elements
           (if (and (vectorp elements) (plusp (length elements)))
               (coerce elements 'list)
               elements))
         (array (if wrapper-p
                    (make-postgres-array
                     :elements normalized-elements
                     :dimensions (postgres-array-dimensions value)
                     :lower-bounds (postgres-array-lower-bounds value)
                     :element-oid (postgres-array-element-oid value))
                    (make-postgres-array :elements normalized-elements
                                         :lower-bounds #(0)
                                         :element-oid element-oid))))
    (multiple-value-bind (elements dimensions lower-bounds flat)
        (%array-shape array element-oid)
      (declare (ignore elements))
      (unless (and (= (length dimensions) 1)
                   (= (length lower-bounds) 1)
                   (zerop (aref lower-bounds 0)))
        (error 'parameter-error :parameter value
               :message "PostgreSQL vector binary values must be one-dimensional with lower bound zero"))
      (when (some #'sql-null-p flat)
        (error 'parameter-error :parameter value
               :message "PostgreSQL vector binary values cannot contain NULL"))
      (%encode-array-binary registry element-oid array))))

(defun decode-value (registry oid octets &key (format 0))
  (unless (member format '(0 1))
    (error 'parameter-error :parameter format
           :message "PostgreSQL format must be 0 (text) or 1 (binary)"))
  (let ((codec (find-type-codec registry oid)))
    (if (or (null octets) (sql-null-p octets))
        +sql-null+
        (let ((decoder (if (zerop format)
                           (and codec (type-codec-text-decoder codec))
                           (and codec (type-codec-binary-decoder codec)))))
          (if decoder
              (funcall decoder octets)
              octets)))))

(defun encode-value (registry oid value &key (format 0))
  (unless (member format '(0 1))
    (error 'parameter-error :parameter format
           :message "PostgreSQL format must be 0 (text) or 1 (binary)"))
  (when (sql-null-p value)
    (return-from encode-value nil))
  (let* ((codec (find-type-codec registry oid))
         (encoder (if (zerop format)
                      (and codec (type-codec-text-encoder codec))
                      (and codec (type-codec-binary-encoder codec)))))
    (cond (encoder (funcall encoder value))
          ((typep value '(vector (unsigned-byte 8))) value)
          (t (%encode-utf8 value)))))
