(in-package #:cl-postgresql-kit/test)

(deftest data-constructors-preserve-domain-values
  (let ((codec (make-type-codec :oid 23 :name "int4"))
        (typed (make-typed-value "42" 23 1))
        (interval (cl-postgresql-kit::%make-postgres-interval
                    :months 2 :days 3 :microseconds 4))
        (timetz (cl-postgresql-kit::%make-postgres-time-with-time-zone
                  :microseconds 5 :timezone-seconds 3600))
        (tid (cl-postgresql-kit::%make-postgres-tid
               :block-number 6 :offset-number 7))
        (bit-string (make-postgres-bit-string :bits "101" :varying-p t))
        (mac (make-postgres-mac-address :octets #(1 2 3 4 5 6)))
        (inet (cl-postgresql-kit::%make-postgres-inet
                :octets #(127 0 0 1) :family 4 :netmask 32 :cidr-p t))
        (array (make-postgres-array :elements #(1 2) :dimensions #(2)
                                    :lower-bounds #(1) :element-oid 23))
        (range (cl-postgresql-kit::%make-postgres-range
                 :lower 1 :upper 2 :lower-inclusive t :upper-inclusive nil))
        (multirange (cl-postgresql-kit::%make-postgres-multirange
                      :ranges #()))
        (composite (make-postgres-composite :fields #(1)
                                            :field-names #("value")
                                            :field-oids #(23))))
    (is (= 23 (type-codec-oid codec)))
    (is (string= "int4" (type-codec-name codec)))
    (is (equalp "42" (typed-value-value typed)))
    (is (= 1 (typed-value-format typed)))
    (is (= 2 (cl-postgresql-kit::postgres-interval-months interval)))
    (is (= 3600
           (cl-postgresql-kit::postgres-time-with-time-zone-timezone-seconds
            timetz)))
    (is (= 7 (cl-postgresql-kit::postgres-tid-offset-number tid)))
    (is (cl-postgresql-kit::postgres-bit-string-varying-p bit-string))
    (is (= 6 (length (cl-postgresql-kit::postgres-mac-address-octets mac))))
    (is (cl-postgresql-kit::postgres-inet-cidr-p inet))
    (is (= 23 (cl-postgresql-kit::postgres-array-element-oid array)))
    (is (cl-postgresql-kit::postgres-range-lower-inclusive range))
    (is (vectorp (cl-postgresql-kit::postgres-multirange-ranges multirange)))
    (is (equalp #("value")
                (cl-postgresql-kit::postgres-composite-field-names composite)))))

(deftest type-data-factories-preserve-configuration
  (let* ((text-decoder (lambda (value) value))
         (text-encoder (lambda (value) value))
         (binary-decoder (lambda (value) value))
         (binary-encoder (lambda (value) value))
         (codec (make-type-codec
                 :oid 9000
                 :name "custom"
                 :text-decoder text-decoder
                 :text-encoder text-encoder
                 :binary-decoder binary-decoder
                 :binary-encoder binary-encoder))
         (empty (make-type-registry :include-defaults nil)))
    (is (= 9000 (type-codec-oid codec)))
    (is (string= "custom" (type-codec-name codec)))
    (is (eq text-decoder (type-codec-text-decoder codec)))
    (is (eq text-encoder (type-codec-text-encoder codec)))
    (is (eq binary-decoder (type-codec-binary-decoder codec)))
    (is (eq binary-encoder (type-codec-binary-encoder codec)))
    (is (type-registry-p empty))
    (is (hash-table-p (cl-postgresql-kit::type-registry-by-oid empty)))
    (is (hash-table-p (cl-postgresql-kit::type-registry-by-name empty)))
    (is (not (eq empty (make-type-registry :include-defaults nil))))))

(deftest structured-value-accessors-preserve-all-fields
  (let ((interval (cl-postgresql-kit::%make-postgres-interval
                    :months 2 :days 3 :microseconds 4))
        (timetz (cl-postgresql-kit::%make-postgres-time-with-time-zone
                  :microseconds 5 :timezone-seconds 3600))
        (tid (cl-postgresql-kit::%make-postgres-tid
               :block-number 6 :offset-number 7))
        (bit-string (make-postgres-bit-string :bits "101" :varying-p t))
        (mac (make-postgres-mac-address :octets #(1 2 3 4 5 6)))
        (inet (cl-postgresql-kit::%make-postgres-inet
                :octets #(127 0 0 1) :family 4 :netmask 32 :cidr-p t))
        (array (make-postgres-array :elements #(1 2) :dimensions #(2)
                                    :lower-bounds #(1) :element-oid 23))
        (range (cl-postgresql-kit::%make-postgres-range
                 :lower 1 :upper 2 :lower-inclusive t :upper-inclusive nil))
        (multirange (cl-postgresql-kit::%make-postgres-multirange
                      :ranges #(1 2)))
        (composite (make-postgres-composite :fields #(1)
                                            :field-names #("value")
                                            :field-oids #(23))))
    (is (= 2 (cl-postgresql-kit::postgres-interval-months interval)))
    (is (= 3 (cl-postgresql-kit::postgres-interval-days interval)))
    (is (= 4 (cl-postgresql-kit::postgres-interval-microseconds interval)))
    (is (= 5 (cl-postgresql-kit::postgres-time-with-time-zone-microseconds timetz)))
    (is (= 3600
           (cl-postgresql-kit::postgres-time-with-time-zone-timezone-seconds timetz)))
    (is (= 6 (cl-postgresql-kit::postgres-tid-block-number tid)))
    (is (= 7 (cl-postgresql-kit::postgres-tid-offset-number tid)))
    (is (string= "101" (cl-postgresql-kit::postgres-bit-string-bits bit-string)))
    (is (cl-postgresql-kit::postgres-bit-string-varying-p bit-string))
    (is (equalp #(1 2 3 4 5 6)
                (cl-postgresql-kit::postgres-mac-address-octets mac)))
    (is (equalp #(127 0 0 1) (cl-postgresql-kit::postgres-inet-octets inet)))
    (is (= 4 (cl-postgresql-kit::postgres-inet-family inet)))
    (is (= 32 (cl-postgresql-kit::postgres-inet-netmask inet)))
    (is (cl-postgresql-kit::postgres-inet-cidr-p inet))
    (is (equalp #(1 2) (cl-postgresql-kit::postgres-array-elements array)))
    (is (equalp #(2) (cl-postgresql-kit::postgres-array-dimensions array)))
    (is (equalp #(1) (cl-postgresql-kit::postgres-array-lower-bounds array)))
    (is (= 23 (cl-postgresql-kit::postgres-array-element-oid array)))
    (is (= 1 (cl-postgresql-kit::postgres-range-lower range)))
    (is (= 2 (cl-postgresql-kit::postgres-range-upper range)))
    (is (cl-postgresql-kit::postgres-range-lower-inclusive range))
    (is (null (cl-postgresql-kit::postgres-range-upper-inclusive range)))
    (is (null (cl-postgresql-kit::postgres-range-empty-p range)))
    (is (equalp #(1 2) (cl-postgresql-kit::postgres-multirange-ranges multirange)))
    (is (equalp #(1) (cl-postgresql-kit::postgres-composite-fields composite)))
    (is (equalp #("value")
                (cl-postgresql-kit::postgres-composite-field-names composite)))
    (is (equalp #(23) (cl-postgresql-kit::postgres-composite-field-oids composite)))))

(deftest typed-value-defaults-format-to-text
  (let ((typed (make-typed-value "value" 9000)))
    (is (= 9000 (typed-value-type-oid typed)))
    (is (zerop (typed-value-format typed)))))

(deftest data-constructors-default-optional-fields
  (let ((bit-string (make-postgres-bit-string))
        (mac (make-postgres-mac-address))
        (array (make-postgres-array))
        (range (cl-postgresql-kit::%make-postgres-range))
        (inet (cl-postgresql-kit::%make-postgres-inet)))
    (is (string= "" (cl-postgresql-kit::postgres-bit-string-bits bit-string)))
    (is (equalp #() (cl-postgresql-kit::postgres-mac-address-octets mac)))
    (is (equalp #() (cl-postgresql-kit::postgres-array-elements array)))
    (is (null (cl-postgresql-kit::postgres-range-empty-p range)))
    (is (null (cl-postgresql-kit::postgres-inet-cidr-p inet)))))

(deftest scalar-value-constructors-preserve-values
  (let ((json (make-json-value :value "{\"ok\":true}"))
        (bytea (make-bytea-value :value #(1 2 3)))
        (date (make-date-value :value "2026-08-13"))
        (time (make-time-value :value "12:34:56"))
        (timestamp (make-timestamp-value :value "2026-08-13T12:34:56"))
        (interval (make-interval-value :value "1 day"))
        (timetz (make-timetz-value :value "12:34:56+09"))
        (uuid (make-uuid-value :value "00000000-0000-0000-0000-000000000000")))
    (is-case-each ((actual expected)
                   (list (list (json-value-value json) "{\"ok\":true}")
                         (list (bytea-value-value bytea) #(1 2 3))
                         (list (date-value-value date) "2026-08-13")
                         (list (time-value-value time) "12:34:56")
                         (list (timestamp-value-value timestamp) "2026-08-13T12:34:56")
                         (list (interval-value-value interval) "1 day")
                         (list (timetz-value-value timetz) "12:34:56+09")
                         (list (uuid-value-value uuid)
                               "00000000-0000-0000-0000-000000000000")))
      (is (equalp expected actual)))))
