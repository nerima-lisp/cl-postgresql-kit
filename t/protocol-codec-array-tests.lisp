(in-package #:cl-postgresql-kit/test)

(deftest uuid-and-array-codecs
  (let ((registry (make-type-registry)))
    (let* ((uuid-text "550E8400-E29B-41D4-A716-446655440000")
           (uuid (decode-value
                  registry 2950
                  (cl-codec-kit:string-to-octets uuid-text :encoding :utf-8)))
           (uuid-text-wire (encode-value registry 2950 uuid))
           (uuid-binary (encode-value registry 2950 uuid :format 1))
           (uuid-round-trip (decode-value registry 2950 uuid-binary :format 1)))
      (is (uuid-value-p uuid))
      (is (string= "550e8400-e29b-41d4-a716-446655440000"
                   (uuid-value-value uuid)))
      (is (equalp (cl-codec-kit:string-to-octets
                   "550e8400-e29b-41d4-a716-446655440000"
                   :encoding :utf-8)
                  uuid-text-wire))
      (is (= 16 (length uuid-binary)))
      (is (string= (uuid-value-value uuid)
                   (uuid-value-value uuid-round-trip)))
      (assert-signals 'protocol-error
                     (lambda ()
                       (decode-value
                        registry 2950
                        (cl-codec-kit:string-to-octets
                         "550e8400-e29b-41d4-a716-44665544000Z"
                         :encoding :utf-8))))
      (assert-signals 'parameter-error
                     (lambda () (encode-value registry 2950 "not-a-uuid"))))
    (let* ((array (decode-value
                   registry 1007
                   (cl-codec-kit:string-to-octets "{1,2,NULL}"
                                                  :encoding :utf-8)))
           (elements (postgres-array-elements array))
           (text-wire (encode-value registry 1007 array))
           (binary-wire (encode-value registry 1007 array :format 1))
           (binary-round-trip
             (decode-value registry 1007 binary-wire :format 1)))
      (is (postgres-array-p array))
      (is (equalp #(3) (postgres-array-dimensions array)))
      (is (equalp #(1) (postgres-array-lower-bounds array)))
      (is (= 1 (aref elements 0)))
      (is (= 2 (aref elements 1)))
      (is (sql-null-p (aref elements 2)))
      (is (equalp (cl-codec-kit:string-to-octets "{\"1\",\"2\",NULL}"
                                                  :encoding :utf-8)
                  text-wire))
      (is (equalp (postgres-array-elements binary-round-trip)
                  elements))
      (is (equalp (postgres-array-dimensions binary-round-trip)
                  (postgres-array-dimensions array))))
    (let* ((array (decode-value
                   registry 1007
                   (cl-codec-kit:string-to-octets "{{1,2},{3,4}}"
                                                  :encoding :utf-8)))
           (elements (postgres-array-elements array)))
      (is (equalp #(2 2) (postgres-array-dimensions array)))
      (is (= 1 (aref (aref elements 0) 0)))
      (is (= 4 (aref (aref elements 1) 1))))
    (let* ((array (decode-value
                   registry 1000
                   (cl-codec-kit:string-to-octets "{t,f,NULL}"
                                                  :encoding :utf-8)))
           (elements (postgres-array-elements array)))
      (is (eq t (aref elements 0)))
      (is (null (aref elements 1)))
      (is (sql-null-p (aref elements 2))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 1007
                      (join-octets
                       (encode-value
                        registry 1007
                        (decode-value
                         registry 1007
                         (cl-codec-kit:string-to-octets "{1}"
                                                        :encoding :utf-8))
                        :format 1)
                       (octets 0)))))))

(deftest built-in-array-registration-completeness
  (let ((registry (make-type-registry)))
    (let* ((array
             (decode-value
              registry 271
              (cl-codec-kit:string-to-octets
               "{42,18446744073709551615}" :encoding :utf-8)))
           (elements (postgres-array-elements array))
           (wire (encode-value registry 271 array :format 1))
           (round-trip (decode-value registry 271 wire :format 1)))
      (is (find-type-codec registry 271))
      (is (= 5069 (postgres-array-element-oid array)))
      (is (= 42 (aref elements 0)))
      (is (= (1- (ash 1 64)) (aref elements 1)))
      (is (equalp elements (postgres-array-elements round-trip))))
    (dolist (spec '((1017 600)
                    (1018 601)
                    (1019 602)
                    (1020 603)
                    (1027 604)
                    (629 628)
                    (719 718)
                    (1034 1033)
                    (5039 5038)))
      (destructuring-bind (array-oid element-oid) spec
        (let* ((array
                 (decode-value
                  registry array-oid
                  (cl-codec-kit:string-to-octets
                   "{\"one\",\"two\"}" :encoding :utf-8)))
               (elements (postgres-array-elements array)))
          (is (find-type-codec registry array-oid))
          (is (= element-oid (postgres-array-element-oid array)))
          (is (equalp #("one" "two") elements))
          (is (equalp
               (cl-codec-kit:string-to-octets
                "{\"one\",\"two\"}" :encoding :utf-8)
               (encode-value registry array-oid array))))))))

(deftest array-codec-validates-shape-and-wire-boundaries
  (let ((registry (make-type-registry)))
    (let* ((array
             (decode-value
              registry 1009
              (cl-codec-kit:string-to-octets
               "{\"a,b\",\"quoted\"}" :encoding :utf-8)))
           (elements (postgres-array-elements array)))
      (is (equalp #("a,b" "quoted") elements))
      (is (equalp
           (cl-codec-kit:string-to-octets
            "{\"a,b\",\"quoted\"}" :encoding :utf-8)
           (encode-value registry 1009 array))))
    (let* ((array
             (make-postgres-array
              :elements #(1 2)
              :dimensions #(2)
              :lower-bounds #(0)
              :element-oid 23))
           (wire (encode-value registry 1007 array :format 1))
           (round-trip (decode-value registry 1007 wire :format 1)))
      (is (equalp #(1 2) (postgres-array-elements round-trip)))
      (is (equalp #(2) (postgres-array-dimensions round-trip)))
      (is (equalp #(0) (postgres-array-lower-bounds round-trip))))
    (let* ((array
             (make-postgres-array
              :elements (vector (octets 0 1 255))
              :dimensions #(1)
              :lower-bounds #(1)
              :element-oid 17))
           (wire (encode-value registry 1001 array :format 1))
           (round-trip (decode-value registry 1001 wire :format 1)))
      (is (equalp (vector (octets 0 1 255))
                  (postgres-array-elements round-trip))))
    (it-signals-each
        'parameter-error
        ((:nested-ragged)
         (:element-oid-mismatch)
         (:dimension-mismatch))
      "signals for invalid array shape/wire case ~A"
      (label)
      (case label
        (:nested-ragged
         (encode-value
          registry 1007
          (make-postgres-array
           :elements #(#(1 2) #(3))
           :element-oid 23)))
        (:element-oid-mismatch
         (encode-value
          registry 1007
          (make-postgres-array
           :elements #(1)
           :element-oid 25)))
        (:dimension-mismatch
         (encode-value
          registry 1007
          (make-postgres-array
           :elements #(1 2)
           :dimensions #(3)
           :element-oid 23)))))
    (it-signals-each
        'protocol-error
        ((:unterminated-text)
         (:truncated-binary))
      "signals for malformed array payload case ~A"
      (label)
      (case label
        (:unterminated-text
         (decode-value
          registry 1009
          (cl-codec-kit:string-to-octets
           "{\"unterminated"
           :encoding :utf-8)))
        (:truncated-binary
         (decode-value
          registry 1007
          (octets
           0 0 0 1
           0 0 0 2
           0 0 0 23
           0 0 0 1
           0 0 0 1
           0 0 0 4
           0 0 0 1)
          :format 1))))))

(deftest array-codec-covers-list-and-malformed-boundaries
  (let ((registry (make-type-registry))
        (escaped-text (format nil "{\"a~C~Cb\",\"a~C~Cb\"}"
                              #\\ #\\ #\\ #\")))
    (is (equalp
         (cl-codec-kit:string-to-octets "{\"1\",\"2\"}" :encoding :utf-8)
         (encode-value registry 1007 '(1 2))))
    (assert-signals 'parameter-error
                    (lambda ()
                      (encode-value registry 1007 "scalar")))
    (let* ((wire
             (encode-value
              registry 1007
              (make-postgres-array
               :elements '(1 2)
               :dimensions '(2)
               :lower-bounds '(0)
               :element-oid 23)
              :format 1))
           (array (decode-value registry 1007 wire :format 1)))
      (is (equalp #(2) (postgres-array-dimensions array)))
      (is (equalp #(0) (postgres-array-lower-bounds array))))
    (let* ((wire
             (encode-value
              registry 1007
              (make-postgres-array
               :elements #()
               :element-oid 23)
              :format 1))
           (array (decode-value registry 1007 wire :format 1)))
      (is (equalp #() (postgres-array-elements array)))
      (is (equalp #(0) (postgres-array-dimensions array))))
    (it-signals-each 'parameter-error
        ((:lower-bound-cardinality-mismatch '(0 1))
         (:lower-bound-overflow #(2147483648)))
      "array codec rejects malformed lower bounds case ~A"
      (label lower-bounds)
      (declare (ignore label))
      (encode-value
       registry 1007
       (make-postgres-array
        :elements #(1 2)
        :lower-bounds lower-bounds
        :element-oid 23)))
    (let* ((array
             (decode-value
              registry 1009
              (cl-codec-kit:string-to-octets escaped-text
                                             :encoding :utf-8)))
           (elements (postgres-array-elements array)))
      (is (equalp
           (vector (format nil "a~Cb" #\\)
                   (format nil "a~Cb" #\"))
           elements))
      (is (equalp
           (cl-codec-kit:string-to-octets escaped-text :encoding :utf-8)
           (encode-value registry 1009 array))))
    (it-signals-each 'protocol-error
        ((:scalar-text "1")
         (:trailing-comma "{1,}")
         (:unterminated-array "{1")
         (:unterminated-nested "{{")
         (:invalid-separator "{{1};2}")
         (:trailing-junk "{}junk")
         (:trailing-escape
          #.(concatenate 'string "{\"x" (string #\\))))
      "array text codec rejects malformed payload case ~A"
      (label payload)
      (declare (ignore label))
      (decode-value
       registry 1009
       (cl-codec-kit:string-to-octets payload
                                      :encoding :utf-8)))
    (it-signals-each 'protocol-error
        ((:dimension-count-overflow
          (binary-array-wire 7 0 23 nil nil))
         (:invalid-has-null-flag
          (binary-array-wire 1 2 23 '(1) '(1)))
         (:element-oid-mismatch
          (binary-array-wire 1 0 25 '(1) '(1)))
         (:negative-dimension
          (binary-array-wire 1 0 23 '(-1) '(1)))
         (:dimension-limit-overflow
          (binary-array-wire 1 0 23 '(1000001) '(1)))
         (:missing-element-length
          (binary-array-wire 1 0 23 '(1) '(1)
                             (octets 255 255 255 255)))
         (:negative-element-length
          (binary-array-wire 1 0 23 '(1) '(1)
                             (octets 255 255 255 254)))
         (:truncated-element-body
          (binary-array-wire 1 0 23 '(1) '(1)
                             (octets 0 0 0 4 1)))
         (:trailing-octet-after-element
          (binary-array-wire 1 0 23 '(1) '(1)
                             (octets 0 0 0 4 0 0 0 1 9)))
         (:short-element-payload
          (binary-array-wire 1 0 23 '(1) '(1)
                             (octets 0 0 0 1 1 9))))
      "array binary codec rejects malformed payload case ~A"
      (label payload)
      (declare (ignore label))
      (decode-value registry 1007 payload :format 1))))
