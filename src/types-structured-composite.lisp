(in-package #:cl-postgresql-kit)

(defun %composite-fields-for-encoding (value field-oids)
  (let* ((composite (and (postgres-composite-p value) value))
         (fields (if composite
                     (postgres-composite-fields composite)
                     value)))
    (unless (or (and (vectorp fields) (not (stringp fields)))
                (listp fields))
      (error 'parameter-error :parameter value
             :message "PostgreSQL composite values must contain a list or vector of fields"))
    (let ((fields (if (vectorp fields) (copy-seq fields) (coerce fields 'vector))))
      (when (/= (length fields) (length field-oids))
        (error 'parameter-error :parameter value
               :message "PostgreSQL composite field count does not match its type definition"))
      (when composite
        (let ((provided-oids (postgres-composite-field-oids composite)))
          (when (plusp (length provided-oids))
            (unless (and (= (length provided-oids) (length field-oids))
                         (loop for index below (length field-oids)
                               always (= (aref provided-oids index)
                                          (aref field-oids index))))
              (error 'parameter-error :parameter value
                     :message "PostgreSQL composite field OIDs do not match its type definition")))))
      fields)))

(defun %composite-field-text (registry field-oid value)
  (let ((payload (encode-value registry field-oid value :format 0)))
    (when (or (null payload) (sql-null-p payload))
      (error 'parameter-error :parameter value
             :message "PostgreSQL composite fields must not encode as SQL NULL"))
    (%type-encoded-text payload :composite)))

(defun %decode-composite-text (registry field-oids field-names octets)
  (let* ((raw-fields (%parse-composite-text (%type-encoded-text octets :composite)))
         (count (length field-oids)))
    (unless (= (length raw-fields) count)
      (error 'protocol-error :message "PostgreSQL composite field count does not match its type definition"
             :context :composite :expected count :actual (length raw-fields)))
    (let ((fields (make-array count)))
      (loop for index below count
            do (setf (aref fields index)
                     (if (sql-null-p (aref raw-fields index))
                         +sql-null+
                         (decode-value registry (aref field-oids index)
                                       (%encode-utf8 (aref raw-fields index))
                                       :format 0))))
      (make-postgres-composite :fields fields
                               :field-names field-names
                               :field-oids field-oids))))

(defun %encode-composite-text (registry field-oids value)
  (let ((fields (%composite-fields-for-encoding value field-oids)))
    (%encode-utf8
     (with-output-to-string (stream)
       (write-char #\( stream)
       (loop for index below (length fields)
             do (when (plusp index) (write-char #\, stream))
                (let ((field (aref fields index)))
                  (unless (sql-null-p field)
                    (%write-type-token
                     stream
                     (%composite-field-text registry (aref field-oids index)
                                             field)))))
       (write-char #\) stream)))))

(defun %decode-composite-binary (registry field-oids field-names octets)
  (let ((position 0)
        (length (length octets)))
    (multiple-value-bind (count next) (%read-i32 octets position)
      (setf position next)
      (unless (<= 0 count *maximum-array-elements*)
        (error 'protocol-error :message "PostgreSQL composite field count is invalid"
               :context :composite :expected (list 0 *maximum-array-elements*)
               :actual count))
      (unless (= count (length field-oids))
        (error 'protocol-error :message "PostgreSQL composite field count does not match its type definition"
               :context :composite :expected (length field-oids) :actual count))
      (let ((fields (make-array count)))
        (loop for index below count
              do (multiple-value-bind (payload-length next)
                     (%read-i32 octets position)
                   (setf position next)
                   (cond ((= payload-length -1)
                          (setf (aref fields index) +sql-null+))
                         ((< payload-length -1)
                          (error 'protocol-error
                                 :message "PostgreSQL composite field length is invalid"
                                 :context :composite :actual payload-length))
                         (t
                          (when (> payload-length (- length position))
                            (error 'protocol-error
                                   :message "PostgreSQL composite field exceeds its payload"
                                   :context :composite :expected (- length position)
                                   :actual payload-length))
                          (setf (aref fields index)
                                (decode-value registry (aref field-oids index)
                                              (subseq octets position
                                                      (+ position payload-length))
                                              :format 1))
                          (incf position payload-length)))))
        (unless (= position length)
          (error 'protocol-error :message "PostgreSQL composite has trailing bytes"
                 :context :composite :expected position :actual length))
        (make-postgres-composite :fields fields
                                 :field-names field-names
                                 :field-oids field-oids)))))

(defun %encode-composite-binary (registry field-oids value)
  (let ((fields (%composite-fields-for-encoding value field-oids))
        (builder (make-octet-builder)))
    (append-i32 builder (length fields))
    (loop for index below (length fields)
          do (let ((field (aref fields index)))
               (if (sql-null-p field)
                   (append-i32 builder -1)
                   (let ((payload (%type-encoded-octets
                                   (encode-value registry (aref field-oids index)
                                                 field :format 1)
                                   field)))
                     (append-i32 builder (length payload))
                     (append-octets builder payload)))))
    (builder-octets builder)))
