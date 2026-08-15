(in-package #:cl-postgresql-kit)

(defun register-array-type (registry &key oid name element-oid)
  (%ensure-type-definition-oid oid :oid)
  (%ensure-type-definition-name name :name)
  (%ensure-type-definition-oid element-oid :element-oid)
  (register-type registry :oid oid :name name
                 :text-decoder (lambda (value)
                                 (%decode-array-text registry element-oid value))
                 :text-encoder (lambda (value)
                                 (%encode-array-text registry element-oid value))
                 :binary-decoder (lambda (value)
                                   (%decode-array-binary registry element-oid value))
                 :binary-encoder (lambda (value)
                                   (%encode-array-binary registry element-oid value))))

(defun %ensure-enum-labels (labels)
  (let ((labels (%copy-type-definition-vector
                 labels :labels
                 "PostgreSQL enum labels must be a list or vector")))
    (loop for label across labels
          do (unless (and (stringp label) (not (find #\Null label)))
               (error 'parameter-error :parameter label
                      :message "PostgreSQL enum labels must be strings without NUL")))
    (loop for index below (length labels)
          do (when (find (aref labels index) labels :start (1+ index)
                         :test #'string=)
               (error 'parameter-error :parameter labels
                      :message "PostgreSQL enum labels must be unique")))
    labels))

(defun register-enum-type (registry &key oid name labels)
  (%ensure-type-definition-oid oid :oid)
  (%ensure-type-definition-name name :name)
  (let ((labels (%ensure-enum-labels labels)))
    (labels ((decode (value)
               (let ((label (%type-encoded-text value :enum)))
                 (unless (find label labels :test #'string=)
                   (error 'protocol-error
                          :message "PostgreSQL enum label is not registered"
                          :context :enum :expected labels :actual label))
                 label))
             (encode (value)
               (unless (and (stringp value)
                            (find value labels :test #'string=))
                 (error 'parameter-error :parameter value
                        :message "PostgreSQL enum value is not a registered label"))
               (%encode-utf8 value)))
      (register-type registry :oid oid :name name
                     :text-decoder #'decode :text-encoder #'encode
                     :binary-decoder #'decode :binary-encoder #'encode))))

(defun register-domain-type (registry &key oid name base-oid)
  (%ensure-type-definition-oid oid :oid)
  (%ensure-type-definition-name name :name)
  (%ensure-type-definition-oid base-oid :base-oid)
  (when (= oid base-oid)
    (error 'parameter-error :parameter base-oid
           :message "PostgreSQL domain base type must differ from its domain OID"))
  (register-type registry :oid oid :name name
                 :text-decoder (lambda (value)
                                 (decode-value registry base-oid value :format 0))
                 :text-encoder (lambda (value)
                                 (encode-value registry base-oid value :format 0))
                 :binary-decoder (lambda (value)
                                   (decode-value registry base-oid value :format 1))
                 :binary-encoder (lambda (value)
                                   (encode-value registry base-oid value :format 1))))

(defun register-range-type (registry &key oid name subtype-oid)
  (%ensure-type-definition-oid oid :oid)
  (%ensure-type-definition-name name :name)
  (%ensure-type-definition-oid subtype-oid :subtype-oid)
  (when (= oid subtype-oid)
    (error 'parameter-error :parameter subtype-oid
           :message "PostgreSQL range subtype must differ from its range OID"))
  (register-type registry :oid oid :name name
                 :text-decoder (lambda (value)
                                 (%decode-range-text registry subtype-oid value))
                 :text-encoder (lambda (value)
                                 (%encode-range-text registry subtype-oid value))
                 :binary-decoder (lambda (value)
                                   (%decode-range-binary registry subtype-oid value))
                 :binary-encoder (lambda (value)
                                   (%encode-range-binary registry subtype-oid value))))

(defun register-multirange-type (registry &key oid name subtype-oid)
  (%ensure-type-definition-oid oid :oid)
  (%ensure-type-definition-name name :name)
  (%ensure-type-definition-oid subtype-oid :subtype-oid)
  (when (= oid subtype-oid)
    (error 'parameter-error :parameter subtype-oid
           :message "PostgreSQL multirange subtype must differ from its multirange OID"))
  (register-type registry :oid oid :name name
                 :text-decoder (lambda (value)
                                 (%decode-multirange-text registry subtype-oid value))
                 :text-encoder (lambda (value)
                                 (%encode-multirange-text registry subtype-oid value))
                 :binary-decoder (lambda (value)
                                   (%decode-multirange-binary registry subtype-oid value))
                 :binary-encoder (lambda (value)
                                   (%encode-multirange-binary registry subtype-oid value))))

(defun %ensure-composite-field-names (field-names count)
  (let ((field-names (if field-names
                         (%copy-type-definition-vector
                          field-names :field-names
                          "PostgreSQL composite field names must be a list or vector")
                         #())))
    (unless (or (zerop (length field-names))
                (= (length field-names) count))
      (error 'parameter-error :parameter field-names
             :message "PostgreSQL composite field names must match field OIDs"))
    (loop for field-name across field-names
          do (%ensure-type-definition-name field-name :field-names))
    field-names))

(defun register-composite-type (registry &key oid name field-oids field-names)
  (%ensure-type-definition-oid oid :oid)
  (%ensure-type-definition-name name :name)
  (let* ((field-oids (%ensure-type-definition-oids
                      field-oids :field-oids
                      "PostgreSQL composite field OIDs must be a list or vector"))
         (field-names (%ensure-composite-field-names field-names
                                                      (length field-oids))))
    (when (find oid field-oids)
      (error 'parameter-error :parameter field-oids
             :message "PostgreSQL composite fields must not recursively use their own type OID"))
    (register-type registry :oid oid :name name
                   :text-decoder (lambda (value)
                                   (%decode-composite-text
                                    registry field-oids field-names value))
                   :text-encoder (lambda (value)
                                   (%encode-composite-text registry field-oids value))
                   :binary-decoder (lambda (value)
                                     (%decode-composite-binary
                                      registry field-oids field-names value))
                   :binary-encoder (lambda (value)
                                     (%encode-composite-binary
                                      registry field-oids value)))))
