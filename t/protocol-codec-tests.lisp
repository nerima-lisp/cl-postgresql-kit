(in-package #:cl-postgresql-kit/test)

(deftest extended-built-in-type-codecs
  (let ((registry (make-type-registry)))
    (is-case-each
        ((oid value)
         '((18 "char")
           (19 "name")
           (142 "xml")
           (2205 "regclass")
           (3220 "1/2")
           (4072 "jsonpath")))
      (let ((wire (cl-codec-kit:string-to-octets value :encoding :utf-8)))
        (is (string= value (decode-value registry oid wire)))
        (is (equalp wire (encode-value registry oid value)))))
    (let* ((wire (cl-codec-kit:string-to-octets "1 -2 32767"
                                                :encoding :utf-8))
           (value (decode-value registry 22 wire)))
      (is (equalp #(1 -2 32767) value))
      (is (equalp wire (encode-value registry 22 value))))
    (let* ((wire (cl-codec-kit:string-to-octets "0 42 4294967295"
                                                :encoding :utf-8))
           (value (decode-value registry 30 wire)))
      (is (equalp #(0 42 4294967295) value))
      (is (equalp wire (encode-value registry 30 value))))
    (let ((xid-wire (octets 0 0 0 42))
          (xid8-wire (octets 0 0 0 0 0 0 0 42)))
      (is (= 42 (decode-value registry 28 xid-wire :format 1)))
      (is (equalp xid-wire (encode-value registry 28 42 :format 1)))
      (is (= 42 (decode-value registry 5069 xid8-wire :format 1)))
      (is (equalp xid8-wire (encode-value registry 5069 42 :format 1))))
    (let* ((array (decode-value
                   registry 1002
                   (cl-codec-kit:string-to-octets "{a,b}"
                                                  :encoding :utf-8))))
      (is (equalp #("a" "b") (postgres-array-elements array)))
      (is (equalp (cl-codec-kit:string-to-octets "{\"a\",\"b\"}"
                                                 :encoding :utf-8)
                  (encode-value registry 1002 array))))
    (let* ((array (decode-value
                   registry 1041
                   (cl-codec-kit:string-to-octets "{192.0.2.1,192.0.2.2}"
                                                  :encoding :utf-8))))
      (is (every #'postgres-inet-p
                 (coerce (postgres-array-elements array) 'list))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 22
                      (cl-codec-kit:string-to-octets "1 invalid"
                                                     :encoding :utf-8))))))

(deftest binary-built-in-character-name-tid-vector-and-registry-codecs
  (let ((registry (make-type-registry)))
    (let ((char-value (string (code-char 255))))
      (is (= 255
             (char-code
              (aref (decode-value registry 18 (octets 255) :format 1) 0))))
      (is (equalp (octets 255)
                  (encode-value registry 18 char-value :format 1)))
      (assert-signals 'parameter-error
                     (lambda ()
                       (encode-value registry 18 "ab" :format 1))))
    (let ((name "postgres")
          (wire (octets 112 111 115 116 103 114 101 115)))
      (is (string= name (decode-value registry 19 wire :format 1)))
      (is (equalp wire (encode-value registry 19 name :format 1)))
      (let ((long-name (make-string 64 :initial-element #\x)))
        (assert-signals 'parameter-error
                       (lambda ()
                         (encode-value registry 19 long-name :format 1))))
      (assert-signals 'protocol-error
                     (lambda ()
                       (decode-value registry 19 (octets 0) :format 1))))
    (let* ((wire (octets 0 0 0 42 1 2))
           (value (decode-value registry 27 wire :format 1)))
      (is (postgres-tid-p value))
      (is (= 42 (postgres-tid-block-number value)))
      (is (= 258 (postgres-tid-offset-number value)))
      (is (equalp wire (encode-value registry 27 value :format 1))))
    (assert-signals 'parameter-error
                   (lambda ()
                     (make-postgres-tid :block-number (ash 1 32))))
    (it-binary-round-trips-each
        (registry
         ((:regproc-family 24
            #(0 0 0 42)
            42
            =)
           (:regproc-family 2202
            #(0 0 0 42)
            42
            =)
           (:regproc-family 2203
            #(0 0 0 42)
            42
            =)
           (:regproc-family 2204
            #(0 0 0 42)
            42
            =)
           (:regproc-family 2205
            #(0 0 0 42)
            42
            =)
           (:regproc-family 2206
            #(0 0 0 42)
            42
            =)
           (:regproc-family 3734
            #(0 0 0 42)
            42
            =)
           (:regproc-family 3769
            #(0 0 0 42)
            42
            =)
           (:regproc-family 4089
            #(0 0 0 42)
            42
            =)
           (:regproc-family 4096
            #(0 0 0 42)
            42
            =)
           (:regproc-family 4191
            #(0 0 0 42)
            42
            =)
           (:int2vector 22
            #(0 0 0 1 0 0 0 0 0 21 0 0 0 2 0 1 255 254 127 255)
            #(1 -2 32767)
            equalp)
           (:oidvector 30
            #(0 0 0 1 0 0 0 0 0 26 0 0 0 3
              0 0 0 0 0 0 0 42 255 255 255 255)
            #(0 42 4294967295)
            equalp)))
      "binary codec round-trips ~A (oid ~A)"
      (label oid wire expected predicate))
    (it-signals-each 'protocol-error
        ((:non-zero-lower-bound
          (binary-i16-vector-wire 21 #(1) :lower-bound 1))
         (:unexpected-null-flag
          (binary-i16-vector-wire 21 #(1) :has-null 1)))
      "vector codec rejects malformed binary payload case ~A"
      (label payload)
      (declare (ignore label))
      (decode-value registry 22 payload :format 1))
    (let* ((array (make-postgres-array
                   :elements (list #(1 -2) #(32767))
                   :lower-bounds #(0)
                   :element-oid 22))
           (wire (encode-value registry 1006 array :format 1))
           (decoded (decode-value registry 1006 wire :format 1)))
      (is (equalp (list #(1 -2) #(32767))
                  (coerce (postgres-array-elements decoded) 'list)))
      (is (equalp wire (encode-value registry 1006 decoded :format 1))))
      (let* ((array (make-postgres-array
                   :elements #("a" "b")
                   :lower-bounds #(0)
                   :element-oid 18))
           (wire (encode-value registry 1002 array :format 1))
           (decoded (decode-value registry 1002 wire :format 1)))
      (is (equalp #("a" "b") (postgres-array-elements decoded)))
      (is (equalp wire (encode-value registry 1002 decoded :format 1))))))
