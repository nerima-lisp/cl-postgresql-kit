(in-package #:cl-postgresql-kit/test)

(cl-weave:it-each
    ((1082 "2024-02-29" date-value-p)
     (1083 "12:34:56" time-value-p)
     (1114 "1999-12-31T23:59:59" timestamp-value-p)
     (1184 "2023-11-14T22:13:20Z" timestamptz-value-p))
  "temporal binary codecs accept ISO-8601 string parameters"
  (oid value predicate)
  (let* ((registry (make-type-registry))
         (wire (encode-value registry oid value :format 1))
         (decoded (decode-value registry oid wire :format 1)))
    (is (funcall predicate decoded))))

(cl-weave:it-each
    ((1082 date date-value-p)
     (1083 time time-value-p)
     (1114 timestamp timestamp-value-p)
     (1184 timestamptz timestamptz-value-p))
  "temporal binary codecs unwrap value wrappers"
  (oid wrapper-kind predicate)
  (let* ((registry (make-type-registry))
         (value
           (ecase wrapper-kind
             (date (make-date-value :value "2024-02-29"))
             (time (make-time-value :value "12:34:56"))
             (timestamp (make-timestamp-value :value "1999-12-31T23:59:59"))
             (timestamptz
              (make-timestamptz-value :value "2023-11-14T22:13:20Z"))))
         (wire (encode-value registry oid value :format 1))
         (decoded (decode-value registry oid wire :format 1)))
    (is (funcall predicate decoded))))

(it-signals-each 'parameter-error
    ((1082 "not-a-date")
     (1083 "not-a-time")
     (1114 "not-a-timestamp")
     (1184 "not-a-timestamptz"))
  "invalid temporal binary parameters signal parameter-error"
  (oid value)
  (let ((registry (make-type-registry)))
    (encode-value registry oid value :format 1)))

(cl-weave:it-each
    ((1082 "2024-02-29" date-value-p)
     (1083 "12:34:56" time-value-p)
     (1114 "1999-12-31T23:59:59" timestamp-value-p)
     (1184 "2023-11-14T22:13:20Z" timestamptz-value-p))
  "temporal text codecs round-trip ISO-8601 values"
  (oid value predicate)
  (let* ((registry (make-type-registry))
         (wire (cl-codec-kit:string-to-octets value :encoding :utf-8))
         (decoded (decode-value registry oid wire))
         (encoded (encode-value registry oid decoded)))
    (is (funcall predicate decoded))
    (is (equalp wire encoded))))

(cl-weave:it-each
    (("12:34:56.123456+09" "12:34:56.123456+09" -32400)
     ("04:05:06-08:30" "04:05:06-08:30" 30600)
     ("040506+0730" "04:05:06+07:30" -27000)
     ("24:00:00-15:59:59" "24:00:00-15:59:59" 57599)
     ("00:00:00Z" "00:00:00+00" 0))
  "timetz text codecs parse and canonicalize PostgreSQL offsets"
  (value canonical timezone-seconds)
  (let* ((registry (make-type-registry))
         (wire (cl-codec-kit:string-to-octets value :encoding :utf-8))
         (decoded (decode-value registry 1266 wire))
         (encoded (encode-value registry 1266 decoded)))
    (is (timetz-value-p decoded))
    (is (= timezone-seconds
           (postgres-time-with-time-zone-timezone-seconds
            (timetz-value-value decoded))))
    (is (equal canonical
               (cl-postgresql-kit::%decode-utf8 encoded)))))

(it-signals-each 'protocol-error
    (("12:34:56")
     ("25:00:00+00")
     ("12:34:56.1234567+00")
     ("12:34:56+16:00")
     ("12:34:56+00:60"))
  "invalid timetz text payloads signal protocol-error"
  (value)
  (let ((registry (make-type-registry)))
    (decode-value registry 1266
                  (cl-codec-kit:string-to-octets value :encoding :utf-8))))

(it-signals-each 'parameter-error
    (("12:34:56")
     ("12:34:56+16:00")
     (42))
  "invalid timetz text parameters signal parameter-error"
  (value)
  (let ((registry (make-type-registry)))
    (encode-value registry 1266 value)))
