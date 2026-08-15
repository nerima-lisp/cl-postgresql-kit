(in-package #:cl-postgresql-kit/test)

(deftest dynamic-custom-type-codecs
  (let ((registry (make-type-registry)))
    (register-enum-type registry
                        :oid 8000
                        :name "mood"
                        :labels '("sad" "ok" "happy"))
    (register-domain-type registry
                          :oid 8001
                          :name "positive_int"
                          :base-oid 23)
    (register-range-type registry
                         :oid 8002
                         :name "int4range"
                         :subtype-oid 23)
    (register-composite-type registry
                             :oid 8003
                             :name "number_label"
                             :field-oids #(23 25)
                             :field-names #("number" "label"))
    (register-array-type registry
                         :oid 8004
                         :name "mood[]"
                         :element-oid 8000)
    (let ((wire (cl-codec-kit:string-to-octets "happy" :encoding :utf-8)))
      (dolist (format '(0 1))
        (is (string= "happy" (decode-value registry 8000 wire :format format)))
        (is (equalp wire (encode-value registry 8000 "happy" :format format)))))
    (let ((wire (cl-codec-kit:string-to-octets "42" :encoding :utf-8))
          (binary-wire (octets 0 0 0 42)))
      (is (= 42 (decode-value registry 8001 wire)))
      (is (equalp wire (encode-value registry 8001 42)))
      (is (= 42 (decode-value registry 8001 binary-wire :format 1)))
      (is (equalp binary-wire (encode-value registry 8001 42 :format 1))))
    (let* ((wire (cl-codec-kit:string-to-octets "[1,10)" :encoding :utf-8))
           (value (decode-value registry 8002 wire)))
      (is (postgres-range-p value))
      (is (= 1 (postgres-range-lower value)))
      (is (= 10 (postgres-range-upper value)))
      (is (postgres-range-lower-inclusive value))
      (is (not (postgres-range-upper-inclusive value)))
      (is (equalp wire (encode-value registry 8002 value))))
    (let* ((wire (octets 2 0 0 0 4 0 0 0 1
                         0 0 0 4 0 0 0 10))
           (value (decode-value registry 8002 wire :format 1)))
      (is (= 1 (postgres-range-lower value)))
      (is (= 10 (postgres-range-upper value)))
      (is (equalp wire (encode-value registry 8002 value :format 1))))
    (let* ((wire (cl-codec-kit:string-to-octets
                  "(42,\"hello, world\")"
                  :encoding :utf-8))
           (value (decode-value registry 8003 wire))
           (fields (postgres-composite-fields value)))
      (is (postgres-composite-p value))
      (is (= 42 (aref fields 0)))
      (is (string= "hello, world" (aref fields 1)))
      (is (equalp #("number" "label")
                  (postgres-composite-field-names value)))
      (is (equalp wire (encode-value registry 8003 value))))
    (let* ((label-wire (cl-codec-kit:string-to-octets "hello, world"
                                                       :encoding :utf-8))
           (builder (make-octet-builder)))
      (append-i32 builder 2)
      (append-i32 builder 4)
      (append-octets builder (octets 0 0 0 42))
      (append-i32 builder (length label-wire))
      (append-octets builder label-wire)
      (let* ((wire (builder-octets builder))
             (value (decode-value registry 8003 wire :format 1))
             (fields (postgres-composite-fields value)))
        (is (= 42 (aref fields 0)))
        (is (string= "hello, world" (aref fields 1)))
        (is (equalp wire (encode-value registry 8003 value :format 1)))))
    (let* ((array (decode-value
                   registry 8004
                   (cl-codec-kit:string-to-octets "{sad,happy}"
                                                  :encoding :utf-8)))
           (binary-wire (encode-value registry 8004 array :format 1))
           (binary-array (decode-value registry 8004 binary-wire :format 1)))
      (is (equalp #("sad" "happy") (postgres-array-elements array)))
      (is (equalp (cl-codec-kit:string-to-octets "{\"sad\",\"happy\"}"
                                                 :encoding :utf-8)
                  (encode-value registry 8004 array)))
      (is (equalp #("sad" "happy")
                  (postgres-array-elements binary-array)))
      (is (equalp binary-wire
                  (encode-value registry 8004 binary-array :format 1))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (decode-value
                       registry 8000
                       (cl-codec-kit:string-to-octets "angry"
                                                      :encoding :utf-8))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (decode-value
                       registry 8002
                       (cl-codec-kit:string-to-octets "[1"
                                                      :encoding :utf-8))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (decode-value
                       registry 8003
                       (cl-codec-kit:string-to-octets "(42)"
                                                      :encoding :utf-8))))))

(deftest structured-codec-boundaries
  (let ((registry (make-type-registry)))
    (register-range-type registry
                         :oid 8002
                         :name "int4range"
                         :subtype-oid 23)
    (register-composite-type registry
                             :oid 8003
                             :name "number_label"
                             :field-oids #(23 25)
                             :field-names #("number" "label"))
    (register-range-type registry
                         :oid 8005
                         :name "text_range"
                         :subtype-oid 25)
    (register-composite-type registry
                             :oid 8006
                             :name "empty_composite"
                             :field-oids #()
                             :field-names #())
    (let ((wire (cl-codec-kit:string-to-octets "empty" :encoding :utf-8))
          (value (decode-value
                  registry 8002
                  (cl-codec-kit:string-to-octets "empty" :encoding :utf-8))))
      (is (postgres-range-empty-p value))
      (is (equalp wire (encode-value registry 8002 value))))
    (let* ((value (make-postgres-range :lower nil :upper nil))
           (wire (cl-codec-kit:string-to-octets "(,)" :encoding :utf-8))
           (encoded (encode-value registry 8002 value))
           (decoded (decode-value registry 8002 wire)))
      (is (equalp wire encoded))
      (is (null (postgres-range-lower decoded)))
      (is (null (postgres-range-upper decoded))))
    (let* ((wire (cl-codec-kit:string-to-octets
                  "[\"a,b\",\"a\\\\b\"]"
                  :encoding :utf-8))
           (value (decode-value registry 8005 wire)))
      (is (string= "a,b" (postgres-range-lower value)))
      (is (string= "a\\b" (postgres-range-upper value)))
      (is (equalp wire (encode-value registry 8005 value))))
    (it-signals-each 'protocol-error
        ((:scalar-text
          "x")
         (:mismatched-delimiter
          "[1,2}")
         (:extra-separator
          "[1,2,3)")
         (:missing-quote-separator
          "[1\"2\",3)")
         (:trailing-junk-after-quoted-lower
          "[\"1\"x,2)")
         (:unterminated-quoted-lower
          "[\"1,2)"))
      "range text codec rejects malformed payload case ~A"
      (label payload)
      (declare (ignore label))
      (decode-value
       registry 8002
       (cl-codec-kit:string-to-octets payload
                                      :encoding :utf-8)))
    (it-signals-each 'protocol-error
        ((:invalid-utf8
          (octets #xc3 #x28))
         (:malformed-point-syntax
          (cl-codec-kit:string-to-octets "x1,2)"
                                         :encoding :utf-8)))
      "point text codec rejects malformed payload case ~A"
      (label payload)
      (declare (ignore label))
      (decode-value registry 8002 payload))
    (assert-signals 'parameter-error
                    (lambda () (encode-value registry 8002 1)))
    (let* ((wire (cl-codec-kit:string-to-octets "(,hello)"
                                                :encoding :utf-8))
           (value (decode-value registry 8003 wire))
           (fields (postgres-composite-fields value)))
      (is (sql-null-p (aref fields 0)))
      (is (string= "hello" (aref fields 1)))
      (is (equalp wire (encode-value registry 8003 value))))
    (is (equalp (cl-codec-kit:string-to-octets "(42,hello)"
                                               :encoding :utf-8)
                (encode-value registry 8003 '(42 "hello"))))
    (let* ((value (decode-value
                   registry 8006
                   (cl-codec-kit:string-to-octets "()" :encoding :utf-8)))
           (fields (postgres-composite-fields value)))
      (is (zerop (length fields)))
      (is (equalp (cl-codec-kit:string-to-octets "()" :encoding :utf-8)
                  (encode-value registry 8006 value))))
    (it-signals-each 'protocol-error
        ((:scalar-text
          "x")
         (:missing-closing-paren
          "(42")
         (:missing-opening-paren
          "42)"))
      "composite text codec rejects malformed payload case ~A"
      (label payload)
      (declare (ignore label))
      (decode-value
       registry 8003
       (cl-codec-kit:string-to-octets payload
                                      :encoding :utf-8)))
    (assert-signals 'protocol-error
                    (lambda ()
                      (decode-value
                       registry 8003
                       (octets #x28 #xc3 #x28 #x2c #x68 #x69 #x29))))
    (assert-signals 'parameter-error
                    (lambda ()
                      (encode-value
                       registry
                       8003
                       (make-postgres-composite
                        :fields #(42 "hello")
                        :field-oids #(23 23)))))
    (let* ((value (make-postgres-composite
                   :fields (vector +sql-null+ "hello")
                   :field-oids #(23 25)))
           (label-wire (cl-codec-kit:string-to-octets "hello"
                                                      :encoding :utf-8))
           (builder (make-octet-builder)))
      (append-i32 builder 2)
      (append-i32 builder -1)
      (append-i32 builder (length label-wire))
      (append-octets builder label-wire)
      (let* ((wire (builder-octets builder))
             (decoded (decode-value registry 8003 wire :format 1))
             (fields (postgres-composite-fields decoded)))
        (is (sql-null-p (aref fields 0)))
        (is (string= "hello" (aref fields 1)))
        (is (equalp wire (encode-value registry 8003 value :format 1)))))
    (it-signals-each 'protocol-error
        ((:negative-field-count
          (octets #xff #xff #xff #xff))
         (:truncated-field-oid
          (octets 0 0 0 1))
         (:invalid-null-sentinel
          (octets 0 0 0 2 #xff #xff #xff #xfe))
         (:truncated-field-payload
          (octets 0 0 0 2 0 0 0 4 1))
         (:trailing-octet-after-fields
          (octets 0 0 0 2
                  #xff #xff #xff #xff
                  #xff #xff #xff #xff
                  0)))
      "composite binary codec rejects malformed payload case ~A"
      (label payload)
      (declare (ignore label))
      (decode-value registry 8003 payload :format 1))
    (register-type
     registry
     :oid 8007
     :name "nullish"
     :text-decoder #'identity
     :text-encoder (lambda (value)
                     (declare (ignore value))
                     nil))
    (register-composite-type registry
                             :oid 8008
                             :name "nullish_composite"
                             :field-oids #(8007 25)
                             :field-names #("value" "label"))
    (assert-signals 'parameter-error
                    (lambda ()
                      (encode-value registry 8008 '("bad" "hello"))))
    (register-type
     registry
     :oid 8009
     :name "nullish_range_base"
     :text-decoder #'identity
     :text-encoder (lambda (value)
                     (declare (ignore value))
                     nil))
    (register-range-type registry
                         :oid 8010
                         :name "nullish_range"
                         :subtype-oid 8009)
    (assert-signals 'parameter-error
                    (lambda ()
                      (encode-value
                       registry
                       8010
                       (make-postgres-range :lower "bad" :upper nil))))))
