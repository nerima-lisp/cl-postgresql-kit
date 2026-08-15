(in-package #:cl-postgresql-kit/test)

(deftest type-registry-isolates-connections
  (let ((left (ready-memory-connection))
        (right (ready-memory-connection)))
    (unwind-protect
         (progn
           (is (not (eq (connection-type-registry left)
                        (connection-type-registry right))))
           (register-type (connection-type-registry left)
                          :oid 90000
                          :name "local_type")
           (is (find-type-codec (connection-type-registry left) 90000))
           (is (null (find-type-codec (connection-type-registry right) 90000))))
      (disconnect left)
      (disconnect right))))

(deftest type-registry-replacement-is-atomic
  (let ((registry (make-type-registry :include-defaults nil))
        (first (make-type-codec :oid 90001 :name "old_name"))
        (second (make-type-codec :oid 90001 :name "new_name")))
    (register-type registry :codec first)
    (register-type registry :codec second)
    (is (eq second (find-type-codec registry 90001)))
    (is (eq second (find-type-codec registry "new_name")))
    (is (null (find-type-codec registry "old_name")))))

(deftest type-registry-registration-boundaries
  (let ((registry (make-type-registry :include-defaults nil)))
    (it-signals-each 'parameter-error
        ((:non-codec :not-a-codec nil)
         (:negative-oid -1 "bad")
         (:empty-name 90002 ""))
      "rejects invalid type registration inputs"
      (label oid name)
      (declare (ignore label))
      (register-type
       registry
       :codec
       (if (eq oid :not-a-codec)
           1
           (make-type-codec :oid oid :name name))))
    (let ((first (make-type-codec :oid 90002 :name "same"))
          (second (make-type-codec :oid 90003 :name "same")))
      (register-type registry :codec first)
      (register-type registry :codec second)
      (is (null (find-type-codec registry 90002)))
      (is (eq second (find-type-codec registry 90003)))
      (is (eq second (find-type-codec registry 'same))))
    (assert-signals 'simple-type-error
                    (lambda () (find-type-codec nil 1)))
    (register-enum-type registry :oid 90010 :name "mood" :labels '("ok"))
    (cl-weave:it "rejects invalid derived type definitions"
      (is-case-each
          ((label thunk)
           (list
            (list :invalid-enum-value
                  (lambda () (encode-value registry 90010 "bad")))
            (list :duplicate-enum-labels
                  (lambda ()
                    (register-enum-type
                     registry
                     :oid 90011
                     :name "bad-enum"
                     :labels '("a" "a"))))
            (list :enum-label-with-nul
                  (lambda ()
                    (register-enum-type
                     registry
                     :oid 90012
                     :name "nul-enum"
                     :labels (list (format nil "bad~Cvalue" (code-char 0))))))
            (list :recursive-domain
                  (lambda ()
                    (register-domain-type
                     registry
                     :oid 90013
                     :name "bad-domain"
                     :base-oid 90013)))
            (list :recursive-range
                  (lambda ()
                    (register-range-type
                     registry
                     :oid 90014
                     :name "bad-range"
                     :subtype-oid 90014)))
            (list :mismatched-composite-fields
                  (lambda ()
                    (register-composite-type
                     registry
                     :oid 90015
                     :name "bad-composite"
                     :field-oids #(23)
                     :field-names #("first" "extra"))))
            (list :recursive-composite
                  (lambda ()
                    (register-composite-type
                     registry
                     :oid 90016
                     :name "recursive"
                     :field-oids #(90016)
                     :field-names #("self"))))))
        (declare (ignore label))
        (assert-signals 'parameter-error thunk)))))

(deftest malformed-type-payloads-are-protocol-errors
  (let ((registry (default-type-registry)))
    (it-signals-each 'protocol-error
        ((:short-bytea-hex 17 "\\x0")
         (:invalid-bytea-hex 17 "\\xgg")
         (:legacy-bytea-escape 17 "\\001\\002\\\\A")
         (:invalid-int4-text 23 "not-an-integer")
         (:invalid-int2vector-text 22 "1 32768")
         (:invalid-float8-text 701 "BOGUS"))
      "rejects malformed text payloads"
      (label oid payload)
      (declare (ignore label))
      (decode-value
       registry oid
       (cl-codec-kit:string-to-octets payload :encoding :utf-8)))
    (it-signals-each 'protocol-error
        ((:short-int4-binary 23 (0 1) 1)
         (:short-oid-binary 26 (0) 1)
         (:empty-bool-text 16 "" :text)
         (:overflow-int2-text 21 "32768" :text))
      "rejects malformed scalar payloads"
      (label oid payload &optional format)
      (declare (ignore label))
      (decode-value
       registry oid
       (etypecase payload
         (string (cl-codec-kit:string-to-octets payload :encoding :utf-8))
         (list (apply #'octets payload))
         ((vector (unsigned-byte 8)) payload))
       :format (or format 0)))
    (is (= #xffffffff
           (decode-value registry 26
                         (cl-codec-kit:string-to-octets "4294967295"
                                                        :encoding :utf-8))))
    (it-signals-each 'protocol-error
        ((:overflow-oid-text 26 "4294967296")
         (:complex-float8-text 701 "#C(1 2)")
         (:empty-float8-text 701 "")
         (:trailing-float8-text 701 "1.0 trailing"))
      "rejects invalid numeric text payloads"
      (label oid payload)
      (declare (ignore label))
      (decode-value
       registry oid
       (cl-codec-kit:string-to-octets payload :encoding :utf-8)))
    (cl-weave:it "rejects invalid codec API inputs"
      (is-case-each
          ((label thunk)
           (list
            (list :int4-overflow
                  (lambda () (encode-value registry 23 2147483648)))
            (list :int2vector-not-vector
                  (lambda () (encode-value registry 22 1)))
            (list :int2vector-member-overflow
                  (lambda () (encode-value registry 22 #(32768))))
            (list :int4-binary-wrong-type
                  (lambda () (encode-value registry 23 "1" :format 1)))
            (list :oid-binary-wrong-type
                  (lambda () (encode-value registry 26 "1" :format 1)))
            (list :oid-binary-negative
                  (lambda () (encode-value registry 26 -1 :format 1)))
            (list :oid-binary-overflow
                  (lambda () (encode-value registry 26 #x100000000 :format 1)))
            (list :int2-binary-overflow
                  (lambda () (encode-value registry 21 32768 :format 1)))
            (list :unknown-format
                  (lambda () (decode-value registry 23 #() :format 2)))))
        (declare (ignore label))
        (assert-signals 'parameter-error thunk)))))
