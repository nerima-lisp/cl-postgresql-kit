(in-package #:cl-postgresql-kit/test)

(deftest temporal-binary-codecs
  (let ((registry (make-type-registry)))
    (let* ((value (cl-date-kit:local-date-of 2024 2 29))
           (wire (encode-value registry 1082 value :format 1))
           (decoded (decode-value registry 1082 wire :format 1))
           (round-trip (date-value-value decoded)))
      (is (date-value-p decoded))
      (is (cl-date-kit:local-date-p round-trip))
      (is (= 2024 (cl-date-kit:local-date-year round-trip)))
      (is (= 2 (cl-date-kit:local-date-month round-trip)))
      (is (= 29 (cl-date-kit:local-date-day round-trip))))
    (let* ((value (cl-date-kit:local-time-of 12 34 56 123456000))
           (wire (encode-value registry 1083 value :format 1))
           (decoded (decode-value registry 1083 wire :format 1))
           (round-trip (time-value-value decoded)))
      (is (time-value-p decoded))
      (is (cl-date-kit:local-time-p round-trip))
      (is (= (cl-date-kit:local-time-to-nano-of-day value)
             (cl-date-kit:local-time-to-nano-of-day round-trip))))
    (let* ((value
             (cl-date-kit:local-date-time-of
              1999 12 31 23 59 59 123456000))
           (wire (encode-value registry 1114 value :format 1))
           (decoded (decode-value registry 1114 wire :format 1))
           (round-trip (timestamp-value-value decoded)))
      (is (timestamp-value-p decoded))
      (is (cl-date-kit:local-date-time-p round-trip))
      (is (= (cl-date-kit:local-date-time-year value)
             (cl-date-kit:local-date-time-year round-trip)))
      (is (= (cl-date-kit:local-date-time-month value)
             (cl-date-kit:local-date-time-month round-trip)))
      (is (= (cl-date-kit:local-date-time-day value)
             (cl-date-kit:local-date-time-day round-trip)))
      (is (= (cl-date-kit:local-date-time-hour value)
             (cl-date-kit:local-date-time-hour round-trip)))
      (is (= (cl-date-kit:local-date-time-minute value)
             (cl-date-kit:local-date-time-minute round-trip)))
      (is (= (cl-date-kit:local-date-time-second value)
             (cl-date-kit:local-date-time-second round-trip)))
      (is (= (cl-date-kit:local-date-time-nanosecond value)
             (cl-date-kit:local-date-time-nanosecond round-trip))))
    (let* ((value (cl-date-kit:instant-of-epoch-micros 1700000000123456))
           (wire (encode-value registry 1184 value :format 1))
           (decoded (decode-value registry 1184 wire :format 1))
           (round-trip (timestamptz-value-value decoded)))
      (is (timestamptz-value-p decoded))
      (is (cl-date-kit:instant-p round-trip))
      (is (= (cl-date-kit:instant-to-epoch-micros value)
             (cl-date-kit:instant-to-epoch-micros round-trip))))
    (is-case-each
        ((oid value)
         '((1082 "infinity")
           (1114 "infinity")
           (1184 "infinity")
           (1082 "-infinity")
           (1114 "-infinity")
           (1184 "-infinity")))
      (let* ((wire (encode-value registry oid value :format 1))
             (decoded (decode-value registry oid wire :format 1)))
        (is (cond ((date-value-p decoded)
                   (string= value (date-value-value decoded)))
                  ((timestamp-value-p decoded)
                   (string= value (timestamp-value-value decoded)))
                  ((timestamptz-value-p decoded)
                   (string= value (timestamptz-value-value decoded)))
                  (t nil)))))
    (let* ((wire (cl-codec-kit:string-to-octets "1 day 02:03:04"
                                                :encoding :utf-8))
           (decoded (decode-value registry 1186 wire))
           (encoded (encode-value registry 1186 decoded)))
      (is (interval-value-p decoded))
      (is (string= "1 day 02:03:04" (interval-value-value decoded)))
      (is (equalp wire encoded)))
    (let* ((value (make-postgres-interval :months 14
                                          :days -2
                                          :microseconds -1234567))
           (wrapped (make-interval-value :value value))
           (wire (encode-value registry 1186 wrapped :format 1))
           (decoded (decode-value registry 1186 wire :format 1))
           (round-trip (interval-value-value decoded)))
      (is (equalp
           (octets #xff #xff #xff #xff #xff #xed #x29 #x79
                   #xff #xff #xff #xfe
                   0 0 0 14)
           wire))
      (is (interval-value-p decoded))
      (is (postgres-interval-p round-trip))
      (is (= 14 (postgres-interval-months round-trip)))
      (is (= -2 (postgres-interval-days round-trip)))
      (is (= -1234567 (postgres-interval-microseconds round-trip)))
      (is (equalp wire (encode-value registry 1186 decoded :format 1))))
    (let* ((values (vector
                    (make-interval-value
                     :value (make-postgres-interval :months 1))
                    (make-interval-value
                     :value (make-postgres-interval :days 2))))
           (wire (encode-value registry 1187 values :format 1))
           (decoded (decode-value registry 1187 wire :format 1))
           (elements (coerce (postgres-array-elements decoded) 'list)))
      (is (= 2 (length elements)))
      (is (= 1
             (postgres-interval-months
              (interval-value-value (first elements)))))
      (is (= 2
             (postgres-interval-days
              (interval-value-value (second elements)))))
      (is (equalp wire (encode-value registry 1187 decoded :format 1))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (decode-value registry 1186
                                    (subseq (octets 0 0 0 0 0 0 0 0
                                                    0 0 0 0 0 0 0)
                                            0 15)
                                    :format 1)))
    (assert-signals 'parameter-error
                    (lambda ()
                      (encode-value registry 1186
                                    (make-interval-value :value "1 second")
                                    :format 1)))
    (it-signals-each 'protocol-error
        ((:negative-time-microseconds -1)
         (:time-microseconds-overflow
          cl-postgresql-kit::*postgres-microseconds-per-day*))
      "time binary codec rejects out-of-range payload case ~A"
      (label value)
      (declare (ignore label))
      (decode-value
       registry 1083
       (cl-postgresql-kit::%encode-binary-signed value 8)
       :format 1))))

(deftest money-and-timetz-binary-codecs
  (let ((registry (make-type-registry)))
    (let* ((value -42)
           (wire (encode-value registry 790 value :format 1))
           (decoded (decode-value registry 790 wire :format 1)))
      (is (equalp (octets 255 255 255 255 255 255 255 214) wire))
      (is (= value decoded)))
    (let* ((value (make-postgres-time-with-time-zone
                   :microseconds 1
                   :timezone-seconds -1))
           (wrapped (make-timetz-value :value value))
           (wire (encode-value registry 1266 wrapped :format 1))
           (decoded (decode-value registry 1266 wire :format 1))
           (round-trip (timetz-value-value decoded)))
      (is (equalp (octets 0 0 0 0 0 0 0 1 255 255 255 255) wire))
      (is (timetz-value-p decoded))
      (is (postgres-time-with-time-zone-p round-trip))
      (is (= 1 (postgres-time-with-time-zone-microseconds round-trip)))
      (is (= -1 (postgres-time-with-time-zone-timezone-seconds round-trip)))
      (is (equalp wire (encode-value registry 1266 decoded :format 1))))
    (let* ((values #(42 -7))
           (wire (encode-value registry 791 values :format 1))
           (decoded (decode-value registry 791 wire :format 1)))
      (is (equalp values (postgres-array-elements decoded)))
      (is (equalp wire (encode-value registry 791 decoded :format 1))))
    (let* ((values (vector
                    (make-timetz-value
                     :value (make-postgres-time-with-time-zone
                             :microseconds 1 :timezone-seconds -1))
                    (make-timetz-value
                     :value (make-postgres-time-with-time-zone
                             :microseconds 2 :timezone-seconds 1))))
           (wire (encode-value registry 1270 values :format 1))
           (decoded (decode-value registry 1270 wire :format 1))
           (elements (coerce (postgres-array-elements decoded) 'list)))
      (is (= 2 (length elements)))
      (is (= -1
             (postgres-time-with-time-zone-timezone-seconds
              (timetz-value-value (first elements)))))
      (is (= 1
             (postgres-time-with-time-zone-timezone-seconds
              (timetz-value-value (second elements)))))
      (is (equalp wire (encode-value registry 1270 decoded :format 1))))
    (let* ((value (make-postgres-time-with-time-zone
                   :microseconds cl-postgresql-kit::+postgres-microseconds-per-day+
                   :timezone-seconds 0))
           (wire (encode-value registry 1266 value :format 1))
           (decoded (decode-value registry 1266 wire :format 1)))
      (is (= cl-postgresql-kit::+postgres-microseconds-per-day+
             (postgres-time-with-time-zone-microseconds
              (timetz-value-value decoded)))))
    (it-signals-each 'protocol-error
        ((:money-payload-truncated
          790
          (octets 0 0 0 0 0 0 0))
         (:timetz-payload-truncated
          1266
          (octets 0 0 0 0 0 0 0 0 0 0 0)))
      "money/timetz binary codecs reject truncated payload case ~A"
      (label oid payload)
      (declare (ignore label))
      (decode-value registry oid payload :format 1))
    (it-signals-each 'protocol-error
        ((:negative-microseconds -1)
         (:microseconds-overflow
          (1+ cl-postgresql-kit::+postgres-microseconds-per-day+)))
      "timetz binary codec rejects out-of-range microseconds case ~A"
      (label value)
      (declare (ignore label))
      (let ((builder (make-octet-builder 12)))
        (append-i64 builder value)
        (append-i32 builder 0)
        (decode-value registry 1266
                      (builder-octets builder)
                      :format 1)))
    (it-signals-each 'protocol-error
        ((:timezone-upper-bound
          cl-postgresql-kit::+postgres-timetz-zone-limit+)
         (:timezone-lower-bound
          (- cl-postgresql-kit::+postgres-timetz-zone-limit+)))
      "timetz binary codec rejects out-of-range timezone case ~A"
      (label value)
      (declare (ignore label))
      (let ((builder (make-octet-builder 12)))
        (append-i64 builder 0)
        (append-i32 builder value)
        (decode-value registry 1266
                      (builder-octets builder)
                      :format 1)))
    (assert-signals 'parameter-error
                    (lambda ()
                      (encode-value registry 1266 "12:00:00+00" :format 1)))))
