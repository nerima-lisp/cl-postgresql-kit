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
