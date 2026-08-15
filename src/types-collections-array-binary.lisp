(in-package #:cl-postgresql-kit)

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
                       (setf position next
                             (aref dimensions index) dimension)))
            (loop for index below dimensions-count
                  do (multiple-value-bind (lower-bound next)
                         (%read-i32 octets position)
                       (setf position next
                             (aref lower-bounds index) lower-bound)))
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
          (if (and (vectorp elements)
                   (%sequence-non-empty-p elements))
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
