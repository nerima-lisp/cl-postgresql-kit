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

(defun %register-built-in-types (registry)
  (flet ((register (oid name text-decoder text-encoder &key binary-decoder binary-encoder)
           (register-type registry :oid oid :name name
                          :text-decoder text-decoder :text-encoder text-encoder
                          :binary-decoder binary-decoder :binary-encoder binary-encoder)))
    (register 16 "bool" #'%decode-boolean #'%encode-boolean
              :binary-decoder #'%decode-binary-boolean
              :binary-encoder #'%encode-binary-boolean)
    (register 17 "bytea" #'%decode-bytea-text #'%encode-bytea-text
              :binary-decoder #'identity :binary-encoder #'%crypto-octets)
    (register 18 "char" #'%decode-utf8 #'%encode-utf8
              :binary-decoder #'%decode-char-binary
              :binary-encoder #'%encode-char-binary)
    (register 19 "name" #'%decode-utf8 #'%encode-utf8
              :binary-decoder #'%decode-name-binary
              :binary-encoder #'%encode-name-binary)
    (register 20 "int8"
              (lambda (value) (%decode-integer value (- (ash 1 63)) (1- (ash 1 63))))
              (lambda (value) (%encode-integer value (- (ash 1 63)) (1- (ash 1 63))))
              :binary-decoder (lambda (value) (%decode-binary-signed value 8))
              :binary-encoder (lambda (value) (%encode-binary-signed value 8)))
    (register 21 "int2"
              (lambda (value) (%decode-integer value -32768 32767))
              (lambda (value) (%encode-integer value -32768 32767))
              :binary-decoder (lambda (value) (%decode-binary-signed value 2))
              :binary-encoder (lambda (value) (%encode-binary-signed value 2)))
    (register 23 "int4"
              (lambda (value) (%decode-integer value -2147483648 2147483647))
              (lambda (value) (%encode-integer value -2147483648 2147483647))
              :binary-decoder (lambda (value) (%decode-binary-signed value 4))
              :binary-encoder (lambda (value) (%encode-binary-signed value 4)))
    (register 22 "int2vector"
              (lambda (value)
                (%decode-space-separated-integers value -32768 32767))
              (lambda (value)
                (%encode-space-separated-integers value -32768 32767))
              :binary-decoder (lambda (value)
                                (%decode-vector-binary registry 21 value))
              :binary-encoder (lambda (value)
                                (%encode-vector-binary registry 21 value)))
    (register 24 "regproc" #'%decode-utf8 #'%encode-utf8
              :binary-decoder (lambda (value) (%decode-binary-unsigned value 4))
              :binary-encoder (lambda (value) (%encode-binary-unsigned value 4)))
    (register 27 "tid" #'%decode-utf8 #'%encode-utf8
              :binary-decoder #'%decode-tid-binary
              :binary-encoder #'%encode-tid-binary)
    (register 28 "xid"
              (lambda (value) (%decode-integer value 0 (1- (ash 1 32))))
              (lambda (value) (%encode-integer value 0 (1- (ash 1 32))))
              :binary-decoder (lambda (value) (%decode-binary-unsigned value 4))
              :binary-encoder (lambda (value) (%encode-binary-unsigned value 4)))
    (register 29 "cid"
              (lambda (value) (%decode-integer value 0 (1- (ash 1 32))))
              (lambda (value) (%encode-integer value 0 (1- (ash 1 32))))
              :binary-decoder (lambda (value) (%decode-binary-unsigned value 4))
              :binary-encoder (lambda (value) (%encode-binary-unsigned value 4)))
    (register 30 "oidvector"
              (lambda (value)
                (%decode-space-separated-integers value 0 (1- (ash 1 32))))
              (lambda (value)
                (%encode-space-separated-integers value 0 (1- (ash 1 32))))
              :binary-decoder (lambda (value)
                                (%decode-vector-binary registry 26 value))
              :binary-encoder (lambda (value)
                                (%encode-vector-binary registry 26 value)))
    (register 25 "text" #'%decode-utf8 #'%encode-utf8
              :binary-decoder #'%decode-utf8 :binary-encoder #'%encode-utf8)
    (register 26 "oid"
              (lambda (value) (%decode-integer value 0 (1- (ash 1 32))))
              (lambda (value) (%encode-integer value 0 (1- (ash 1 32))))
              :binary-decoder (lambda (value) (%decode-binary-unsigned value 4))
              :binary-encoder (lambda (value) (%encode-binary-unsigned value 4)))
    (register 700 "float4" #'%decode-float #'%encode-float
              :binary-decoder (lambda (value)
                                (%decode-binary-float value 32 8 23
                                                      'single-float))
              :binary-encoder (lambda (value)
                                (%encode-binary-float value 32 8 23
                                                      'single-float)))
    (register 701 "float8" #'%decode-float #'%encode-float
              :binary-decoder (lambda (value)
                                (%decode-binary-float value 64 11 52
                                                      'double-float))
              :binary-encoder (lambda (value)
                                (%encode-binary-float value 64 11 52
                                                      'double-float)))
    (register 1042 "bpchar" #'%decode-utf8 #'%encode-utf8
              :binary-decoder #'%decode-utf8 :binary-encoder #'%encode-utf8)
    (register 1043 "varchar" #'%decode-utf8 #'%encode-utf8
              :binary-decoder #'%decode-utf8 :binary-encoder #'%encode-utf8)
    (register 1560 "bit"
              (lambda (value) (%decode-bit-text value nil))
              #'%encode-bit-text
              :binary-decoder (lambda (value) (%decode-bit-binary value nil))
              :binary-encoder #'%encode-bit-binary)
    (register 1562 "varbit"
              (lambda (value) (%decode-bit-text value t))
              #'%encode-bit-text
              :binary-decoder (lambda (value) (%decode-bit-binary value t))
              :binary-encoder #'%encode-bit-binary)
    (register 829 "macaddr"
              (lambda (value) (%decode-mac-address-text value 6))
              (lambda (value) (%encode-mac-address-text value 6))
              :binary-decoder (lambda (value) (%decode-mac-address-binary value 6))
              :binary-encoder (lambda (value) (%encode-mac-address-binary value 6)))
    (register 774 "macaddr8"
              (lambda (value) (%decode-mac-address-text value 8))
              (lambda (value) (%encode-mac-address-text value 8))
              :binary-decoder (lambda (value) (%decode-mac-address-binary value 8))
              :binary-encoder (lambda (value) (%encode-mac-address-binary value 8)))
    (register 650 "cidr"
              (lambda (value) (%decode-network-text value t))
              (lambda (value) (%encode-network-text value t))
              :binary-decoder (lambda (value) (%decode-network-binary value t))
              :binary-encoder (lambda (value) (%encode-network-binary value t)))
    (register 869 "inet"
              (lambda (value) (%decode-network-text value nil))
              (lambda (value) (%encode-network-text value nil))
              :binary-decoder (lambda (value) (%decode-network-binary value nil))
              :binary-encoder (lambda (value) (%encode-network-binary value nil)))
    (register 790 "money" #'%decode-utf8 #'%encode-utf8
              :binary-decoder (lambda (value) (%decode-binary-signed value 8))
              :binary-encoder (lambda (value) (%encode-binary-signed value 8)))
    (dolist (spec '((142 "xml")
                    (194 "pg_node_tree")
                    (1033 "aclitem")
                    (600 "point")
                    (601 "lseg")
                    (602 "path")
                    (603 "box")
                    (604 "polygon")
                    (628 "line")
                    (718 "circle")
                    (1790 "refcursor")
                    (2249 "record")
                    (2970 "txid_snapshot")
                    (5038 "pg_snapshot")
                    (3614 "tsvector")
                    (3615 "tsquery")
                    (4072 "jsonpath")))
      (destructuring-bind (oid name) spec
        (register oid name #'%decode-utf8 #'%encode-utf8)))
    (dolist (spec '((3734 "regconfig")
                    (3769 "regdictionary")
                    (4089 "regnamespace")
                    (4096 "regrole")
                    (4191 "regcollation")
                    (2202 "regprocedure")
                    (2203 "regoper")
                    (2204 "regoperator")
                    (2205 "regclass")
                    (2206 "regtype")))
      (destructuring-bind (oid name) spec
        (register oid name #'%decode-utf8 #'%encode-utf8
                  :binary-decoder (lambda (value)
                                    (%decode-binary-unsigned value 4))
                  :binary-encoder (lambda (value)
                                    (%encode-binary-unsigned value 4)))))
    (register 3220 "pg_lsn" #'%decode-pg-lsn-text #'%encode-pg-lsn-text
              :binary-decoder #'%decode-pg-lsn-binary
              :binary-encoder #'%encode-pg-lsn-binary)
    (register 5069 "xid8"
              (lambda (value) (%decode-integer value 0 (1- (ash 1 64))))
              (lambda (value) (%encode-integer value 0 (1- (ash 1 64))))
              :binary-decoder (lambda (value) (%decode-binary-unsigned value 8))
              :binary-encoder (lambda (value) (%encode-binary-unsigned value 8)))
    (register 1082 "date" #'%decode-date #'%encode-date
              :binary-decoder #'%decode-date-binary
              :binary-encoder #'%encode-date-binary)
    (register 1083 "time" #'%decode-time #'%encode-time
              :binary-decoder #'%decode-time-binary
              :binary-encoder #'%encode-time-binary)
    (register 1114 "timestamp" #'%decode-timestamp #'%encode-timestamp
              :binary-decoder #'%decode-timestamp-binary
              :binary-encoder #'%encode-timestamp-binary)
    (register 1184 "timestamptz" #'%decode-timestamptz #'%encode-timestamptz
              :binary-decoder #'%decode-timestamptz-binary
              :binary-encoder #'%encode-timestamptz-binary)
    (register 1266 "timetz" #'%decode-utf8 #'%encode-utf8
              :binary-decoder #'%decode-timetz-binary
              :binary-encoder #'%encode-timetz-binary)
    (register 1186 "interval" #'%decode-interval #'%encode-interval
              :binary-decoder #'%decode-interval-binary
              :binary-encoder #'%encode-interval-binary)
    (register 114 "json" #'%decode-json #'%encode-json)
    (register 3802 "jsonb" #'%decode-json #'%encode-json
              :binary-decoder #'%decode-jsonb :binary-encoder #'%encode-jsonb)
    (register 2950 "uuid" #'%decode-uuid-text #'%encode-uuid-text
              :binary-decoder #'%decode-uuid-binary :binary-encoder #'%encode-uuid-binary)
    (register 1700 "numeric" #'%decode-numeric #'%encode-numeric
              :binary-decoder #'%decode-numeric-binary
              :binary-encoder #'%encode-numeric-binary)
    (dolist (spec '((1000 "bool[]" 16)
                    (1001 "bytea[]" 17)
                    (1002 "char[]" 18)
                    (1003 "name[]" 19)
                    (1005 "int2[]" 21)
                    (1006 "int2vector[]" 22)
                    (1007 "int4[]" 23)
                    (1008 "regproc[]" 24)
                    (1009 "text[]" 25)
                    (1010 "tid[]" 27)
                    (1011 "xid[]" 28)
                    (1012 "cid[]" 29)
                    (1013 "oidvector[]" 30)
                    (1014 "bpchar[]" 1042)
                    (1015 "varchar[]" 1043)
                    (1016 "int8[]" 20)
                    (1021 "float4[]" 700)
                    (1022 "float8[]" 701)
                    (1028 "oid[]" 26)
                    (1115 "timestamp[]" 1114)
                    (1182 "date[]" 1082)
                    (1183 "time[]" 1083)
                    (1185 "timestamptz[]" 1184)
                    (1187 "interval[]" 1186)
                    (791 "money[]" 790)
                    (1270 "timetz[]" 1266)
                    (1231 "numeric[]" 1700)
                    (199 "json[]" 114)
                    (2951 "uuid[]" 2950)
                    (3807 "jsonb[]" 3802)
                    (3221 "pg_lsn[]" 3220)
                    (271 "xid8[]" 5069)
                    (775 "macaddr8[]" 774)
                    (1040 "macaddr[]" 829)
                    (651 "cidr[]" 650)
                    (1041 "inet[]" 869)
                    (1561 "bit[]" 1560)
                    (1563 "varbit[]" 1562)
                    (2207 "regprocedure[]" 2202)
                    (2208 "regoper[]" 2203)
                    (2209 "regoperator[]" 2204)
                    (2210 "regclass[]" 2205)
                    (2211 "regtype[]" 2206)
                    (3735 "regconfig[]" 3734)
                    (3770 "regdictionary[]" 3769)
                    (4090 "regnamespace[]" 4089)
                    (4097 "regrole[]" 4096)
                    (4192 "regcollation[]" 4191)))
      (destructuring-bind (oid name element-oid) spec
        (register oid name
                  (lambda (value)
                    (%decode-array-text registry element-oid value))
                  (lambda (value)
                    (%encode-array-text registry element-oid value))
                  :binary-decoder
                  (lambda (value)
                    (%decode-array-binary registry element-oid value))
                  :binary-encoder
                  (lambda (value)
                    (%encode-array-binary registry element-oid value)))))
    (dolist (spec '((143 "xml[]" 142)
                    (2201 "refcursor[]" 1790)
                    (2287 "record[]" 2249)
                    (1034 "aclitem[]" 1033)
                    (1017 "point[]" 600)
                    (1018 "lseg[]" 601)
                    (1019 "path[]" 602)
                    (1020 "box[]" 603)
                    (1027 "polygon[]" 604)
                    (629 "line[]" 628)
                    (719 "circle[]" 718)
                    (2971 "txid_snapshot[]" 2970)
                    (5039 "pg_snapshot[]" 5038)
                    (3643 "tsvector[]" 3614)
                    (3645 "tsquery[]" 3615)
                    (4073 "jsonpath[]" 4072)
                    ))
      (destructuring-bind (oid name element-oid) spec
        (register oid name
                  (lambda (value)
                    (%decode-array-text registry element-oid value))
                  (lambda (value)
                    (%encode-array-text registry element-oid value)))))
    registry))

(defparameter *default-type-registry* (make-type-registry))

(defun default-type-registry ()
  *default-type-registry*)
