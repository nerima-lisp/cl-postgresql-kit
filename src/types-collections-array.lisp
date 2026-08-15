(in-package #:cl-postgresql-kit)

(defun %sequence-empty-p (sequence)
  (etypecase sequence
    (list (null sequence))
    (vector (if (array-has-fill-pointer-p sequence)
                (zerop (fill-pointer sequence))
                (zerop (array-total-size sequence))))))

(defun %sequence-non-empty-p (sequence)
  (not (%sequence-empty-p sequence)))

(defun %array-scalar-vector-p (element-oid value)
  (or (and (= element-oid 17)
           (or (typep value 'bytea-value)
               (and (vectorp value)
                    (every (lambda (octet)
                             (and (integerp octet) (<= 0 octet 255)))
                           value))))
      (and (member element-oid '(22 30) :test #'=)
           (vectorp value)
           (%sequence-non-empty-p value)
           (every (lambda (element)
                    (and (integerp element)
                         (if (= element-oid 22)
                             (<= -32768 element 32767)
                             (<= 0 element #xffffffff))))
                  value))))

(defun %array-sequence (value &optional element-oid)
  (cond ((and element-oid (%array-scalar-vector-p element-oid value)) nil)
        ((typep value '(and vector (not string))) value)
        ((and (consp value) (listp value)) (coerce value 'vector))
        (t nil)))

(defun %array-node-dimensions (value &optional element-oid)
  (let ((root (%array-sequence value element-oid)))
    (unless root
      (error 'parameter-error :parameter value
             :message "PostgreSQL array values must be vectors or non-empty lists")))
  (labels ((walk (node)
             (let ((sequence (%array-sequence node element-oid)))
               (if sequence
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
                           (concatenate 'vector (vector length) child))))
                   #()))))
    (let ((dimensions (walk value)))
      (when (> (length dimensions) *maximum-array-dimensions*)
        (error 'parameter-error :parameter value
               :message "PostgreSQL array has too many dimensions"))
      dimensions)))

(defun %array-element-count (dimensions &key protocolp)
  (let ((count (if (%sequence-non-empty-p dimensions) 1 0)))
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
               (%sequence-non-empty-p given-dimensions)
               (not (equalp given-dimensions dimensions)))
      (error 'parameter-error :parameter value
             :message "PostgreSQL array dimensions do not match its elements"))
    (let ((lower-bounds
            (if (and given-lower-bounds
                     (%sequence-non-empty-p given-lower-bounds))
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
