(in-package #:cl-postgresql-kit)

(defparameter *maximum-type-registry-catalog-rows* 100000
  "Maximum number of rows accepted from one pg_catalog type query.")

(defun %type-registry-catalog-error (message &rest arguments)
  (error 'protocol-error
         :message (apply #'format nil message arguments)
         :context :type-registry))

(defun %type-registry-catalog-value (value field)
  (when (sql-null-p value)
    (%type-registry-catalog-error
     "The pg_catalog type metadata field ~A was unexpectedly NULL."
     field))
  value)

(defun %type-registry-catalog-oid (value field)
  (let ((value (%type-registry-catalog-value value field)))
    (unless (and (integerp value) (<= 0 value #xffffffff))
      (%type-registry-catalog-error
       "The pg_catalog type metadata field ~A is not an unsigned OID: ~S."
       field value))
    value))

(defun %type-registry-catalog-integer (value field)
  (let ((value (%type-registry-catalog-value value field)))
    (unless (integerp value)
      (%type-registry-catalog-error
       "The pg_catalog type metadata field ~A is not an integer: ~S."
       field value))
    value))

(defun %type-registry-catalog-name (value field)
  (let ((value (%type-registry-catalog-value value field)))
    (unless (and (stringp value) (plusp (length value))
                 (not (find #\Null value)))
      (%type-registry-catalog-error
       "The pg_catalog type metadata field ~A is not a valid name: ~S."
       field value))
    value))

(defun %type-registry-catalog-rows (connection sql column-count)
  (let ((result
          (query connection sql
                :max-result-rows *maximum-type-registry-catalog-rows*)))
    (unless (= column-count (length (result-columns result)))
      (%type-registry-catalog-error
       "The pg_catalog query returned ~D columns; expected ~D."
       (length (result-columns result)) column-count))
    (loop for index below (result-row-count result)
          collect (result-row result index))))

(defun %type-registry-enum-definitions (rows)
  (let ((definitions (make-hash-table :test #'eql))
        (order nil))
    (dolist (row rows)
      (unless (= (length row) 3)
        (%type-registry-catalog-error
         "The enum metadata row has ~D columns; expected 3."
         (length row)))
      (let* ((oid (%type-registry-catalog-oid (aref row 0) :enum-oid))
             (name (%type-registry-catalog-name (aref row 1) :enum-name))
             (label (%type-registry-catalog-name (aref row 2) :enum-label))
             (definition (gethash oid definitions)))
        (unless definition
          (setf definition (list :oid oid :name name :labels nil)
                (gethash oid definitions) definition)
          (push oid order))
        (unless (string= name (getf definition :name))
          (%type-registry-catalog-error
           "Enum OID ~D has inconsistent names: ~S and ~S."
           oid (getf definition :name) name))
        (setf (getf definition :labels)
              (cons label (getf definition :labels)))))
    (loop for oid in (nreverse order)
          for definition = (gethash oid definitions)
          for labels = (nreverse (getf definition :labels))
          do (setf (getf definition :labels) labels)
          collect definition)))

(defun %type-registry-simple-definitions (rows kind)
  (loop for row in rows
        do (unless (= (length row) 3)
             (%type-registry-catalog-error
              "The ~A metadata row has ~D columns; expected 3."
              kind (length row)))
        collect (list :oid (%type-registry-catalog-oid (aref row 0)
                                                       (list kind :oid))
                      :name (%type-registry-catalog-name (aref row 1)
                                                         (list kind :name))
                      :base-oid (%type-registry-catalog-oid
                                 (aref row 2) (list kind :base-oid)))))

(defun %type-registry-composite-definitions (rows)
  (let ((definitions (make-hash-table :test #'eql))
        (order nil))
    (dolist (row rows)
      (unless (= (length row) 5)
        (%type-registry-catalog-error
         "The composite metadata row has ~D columns; expected 5."
         (length row)))
      (let* ((oid (%type-registry-catalog-oid (aref row 0) :composite-oid))
             (name (%type-registry-catalog-name (aref row 1) :composite-name))
             (definition (gethash oid definitions))
             (attribute-number (aref row 2))
             (attribute-name (aref row 3))
             (attribute-oid (aref row 4)))
        (unless definition
          (setf definition (list :oid oid :name name :fields nil
                                 :last-attribute-number nil
                                 :empty-row-p nil)
                (gethash oid definitions) definition)
          (push oid order))
        (unless (string= name (getf definition :name))
          (%type-registry-catalog-error
           "Composite OID ~D has inconsistent names: ~S and ~S."
           oid (getf definition :name) name))
        (if (sql-null-p attribute-number)
            (progn
              (unless (and (sql-null-p attribute-name)
                           (sql-null-p attribute-oid))
                (%type-registry-catalog-error
                 "Composite OID ~D has a partially NULL attribute row."
                 oid))
              (when (getf definition :fields)
                (%type-registry-catalog-error
                 "Composite OID ~D mixes empty and non-empty attribute rows."
                 oid))
              (setf (getf definition :empty-row-p) t))
            (let ((attribute-number
                    (%type-registry-catalog-integer
                     attribute-number :attribute-number))
                  (attribute-name
                    (%type-registry-catalog-name
                     attribute-name :attribute-name))
                  (attribute-oid
                    (%type-registry-catalog-oid attribute-oid :attribute-oid)))
              (unless (plusp attribute-number)
                (%type-registry-catalog-error
                 "Composite OID ~D has an invalid attribute number: ~D."
                 oid attribute-number))
              (when (getf definition :empty-row-p)
                (%type-registry-catalog-error
                 "Composite OID ~D mixes empty and non-empty attribute rows."
                 oid))
              (when (and (getf definition :last-attribute-number)
                         (<= attribute-number
                             (getf definition :last-attribute-number)))
                (%type-registry-catalog-error
                 "Composite OID ~D has out-of-order or duplicate attributes."
                 oid))
              (setf (getf definition :last-attribute-number) attribute-number
                    (getf definition :fields)
                    (cons (list attribute-name attribute-oid)
                          (getf definition :fields)))))))
    (loop for oid in (nreverse order)
          for definition = (gethash oid definitions)
          for fields = (nreverse (getf definition :fields))
          collect (list :oid oid
                        :name (getf definition :name)
                        :field-names (map 'vector #'first fields)
                        :field-oids (map 'vector #'second fields)))))

(defun %type-registry-array-definitions (rows)
  (loop for row in rows
        do (unless (= (length row) 3)
             (%type-registry-catalog-error
              "The array metadata row has ~D columns; expected 3."
              (length row)))
        collect (list :oid (%type-registry-catalog-oid (aref row 0)
                                                       :array-oid)
                      :name (%type-registry-catalog-name (aref row 1)
                                                         :array-name)
                      :element-oid (%type-registry-catalog-oid
                                    (aref row 2) :array-element-oid))))

(defun %type-registry-server-version-at-least-p (connection minimum)
  (let ((value (gethash "server_version_num"
                        (connection-parameters connection))))
    (and value
         (handler-case
             (>= (if (integerp value)
                     value
                     (parse-integer value :junk-allowed nil))
                 minimum)
           (error () nil)))))

(defun load-type-registry (connection &key (registry (connection-type-registry connection)))
  "Load server-defined enum, domain, range, multirange, composite, and array types.

The catalog is queried explicitly so connecting does not require catalog
access or incur hidden queries.  All catalog result sets are read and
validated before REGISTRY is modified."
  (check-type connection connection)
  (check-type registry type-registry)
  (%require-open-connection connection)
  (let* ((enum-rows
             (%type-registry-catalog-rows
              connection
              "SELECT t.oid::int8, t.typname::text, e.enumlabel::text\n                 FROM pg_catalog.pg_type AS t\n                 JOIN pg_catalog.pg_enum AS e ON e.enumtypid = t.oid\n                WHERE t.typtype = 'e'\n                ORDER BY t.oid, e.enumsortorder"
              3))
           (domain-rows
             (%type-registry-catalog-rows
              connection
              "SELECT t.oid::int8, t.typname::text, t.typbasetype::int8\n                 FROM pg_catalog.pg_type AS t\n                WHERE t.typtype = 'd' AND t.typbasetype <> 0\n                ORDER BY t.oid"
              3))
           (range-rows
             (%type-registry-catalog-rows
              connection
              "SELECT t.oid::int8, t.typname::text, r.rngsubtype::int8\n                 FROM pg_catalog.pg_type AS t\n                 JOIN pg_catalog.pg_range AS r ON r.rngtypid = t.oid\n                WHERE t.typtype = 'r'\n                ORDER BY t.oid"
              3))
           (multirange-rows
             (when (%type-registry-server-version-at-least-p connection 140000)
               (%type-registry-catalog-rows
                connection
                "SELECT t.oid::int8, t.typname::text, r.rngsubtype::int8\n                   FROM pg_catalog.pg_type AS t\n                   JOIN pg_catalog.pg_range AS r\n                     ON r.rngmultitypid = t.oid\n                  WHERE t.typtype = 'm'\n                  ORDER BY t.oid"
                3)))
           (composite-rows
             (%type-registry-catalog-rows
              connection
              "SELECT t.oid::int8, t.typname::text, a.attnum::int4,\n                       a.attname::text, a.atttypid::int8\n                 FROM pg_catalog.pg_type AS t\n                 LEFT JOIN pg_catalog.pg_attribute AS a\n                   ON a.attrelid = t.typrelid\n                  AND a.attnum > 0\n                  AND NOT a.attisdropped\n                WHERE t.typtype = 'c' AND t.typrelid <> 0\n                ORDER BY t.oid, a.attnum"
              5))
           (array-rows
             (%type-registry-catalog-rows
              connection
              "SELECT t.oid::int8, t.typname::text, t.typelem::int8\n                 FROM pg_catalog.pg_type AS t\n                WHERE t.typtype = 'b'\n                  AND t.typelem <> 0\n                  AND t.typarray = 0\n                  AND t.typlen = -1\n                ORDER BY t.oid"
              3))
           (enum-definitions (%type-registry-enum-definitions enum-rows))
           (domain-definitions (%type-registry-simple-definitions
                                domain-rows :domain))
           (range-definitions (%type-registry-simple-definitions
                               range-rows :range))
           (multirange-definitions
             (%type-registry-simple-definitions
              multirange-rows :multirange))
           (composite-definitions
             (%type-registry-composite-definitions composite-rows))
           (array-definitions (%type-registry-array-definitions array-rows)))
      (cl-concurrent-kit:with-lock-held ((connection--lock connection))
        (dolist (definition enum-definitions)
          (register-enum-type registry
                              :oid (getf definition :oid)
                              :name (getf definition :name)
                              :labels (getf definition :labels)))
        (dolist (definition domain-definitions)
          (register-domain-type registry
                                :oid (getf definition :oid)
                                :name (getf definition :name)
                                :base-oid (getf definition :base-oid)))
        (dolist (definition range-definitions)
          (register-range-type registry
                               :oid (getf definition :oid)
                               :name (getf definition :name)
                               :subtype-oid (getf definition :base-oid)))
        (dolist (definition composite-definitions)
          (register-composite-type registry
                                   :oid (getf definition :oid)
                                   :name (getf definition :name)
                                   :field-oids (getf definition :field-oids)
                                   :field-names (getf definition :field-names)))
        (dolist (definition array-definitions)
          (register-array-type registry
                               :oid (getf definition :oid)
                               :name (getf definition :name)
                               :element-oid (getf definition :element-oid)))
        (dolist (definition multirange-definitions)
          (register-multirange-type registry
                                    :oid (getf definition :oid)
                                    :name (getf definition :name)
                                    :subtype-oid (getf definition :base-oid)))
        registry)))
