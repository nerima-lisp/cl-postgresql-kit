(in-package #:cl-postgresql-kit/test)

(deftest range-binary-boundaries
  (let ((registry (make-type-registry)))
    (register-range-type registry
                         :oid 8002
                         :name "int4range"
                         :subtype-oid 23)
    (let* ((value (make-postgres-range :empty-p t))
           (wire (encode-value registry 8002 value :format 1))
           (decoded (decode-value registry 8002 wire :format 1)))
      (is (equalp (octets 1) wire))
      (is (postgres-range-empty-p decoded)))
    (let* ((value (make-postgres-range
                   :lower 1
                   :upper 10
                   :lower-inclusive t
                   :upper-inclusive t))
           (wire (encode-value registry 8002 value :format 1))
           (decoded (decode-value registry 8002 wire :format 1)))
      (is (equalp (octets 6 0 0 0 4 0 0 0 1
                          0 0 0 4 0 0 0 10)
                  wire))
      (is (postgres-range-lower-inclusive decoded))
      (is (postgres-range-upper-inclusive decoded)))
    (let* ((value (make-postgres-range :lower nil :upper nil))
           (wire (encode-value registry 8002 value :format 1))
           (decoded (decode-value registry 8002 wire :format 1)))
      (is (equalp (octets #x18) wire))
      (is (null (postgres-range-lower decoded)))
      (is (null (postgres-range-upper decoded)))
      (is (not (postgres-range-lower-inclusive decoded)))
      (is (not (postgres-range-upper-inclusive decoded))))
    (it-signals-each 'protocol-error
        ((:unknown-range-flag
          (octets #x20))
         (:empty-range-trailing-octet
          (octets #x01 0))
         (:lower-bound-missing-length
          (octets #x0a))
         (:upper-bound-missing-length
          (octets #x14))
         (:negative-lower-bound-length
          (octets #x02 #xff #xff #xff #xff))
         (:negative-upper-bound-length
          (octets #x08 #xff #xff #xff #xff))
         (:truncated-lower-bound-payload
          (octets #x02 0 0 0 4 0))
         (:truncated-upper-bound-payload
          (octets #x08 0 0 0 4 0))
         (:unbounded-range-trailing-octet
          (octets #x18 0)))
      "range binary codec rejects malformed payload case ~A"
      (label payload)
      (declare (ignore label))
      (decode-value registry 8002 payload :format 1))
    (assert-signals 'parameter-error
                    (lambda ()
                      (encode-value registry 8002 1 :format 1)))))

(deftest multirange-codec-boundaries
  (let ((registry (make-type-registry)))
    (register-range-type registry
                         :oid 8002
                         :name "int4range"
                         :subtype-oid 23)
    (register-multirange-type registry
                              :oid 8011
                              :name "int4multirange"
                              :subtype-oid 23)
    (let* ((value
             (make-postgres-multirange
              :ranges
              (list (make-postgres-range :lower 1
                                         :upper 10
                                         :lower-inclusive t)
                    (make-postgres-range :lower 20
                                         :upper 30
                                         :upper-inclusive t))))
           (text-wire (cl-codec-kit:string-to-octets
                       "{[1,10),(20,30]}"
                       :encoding :utf-8))
           (binary-wire
             (octets
              0 0 0 2
              0 0 0 #x11
              #x02
              0 0 0 4 0 0 0 1
              0 0 0 4 0 0 0 10
              0 0 0 #x11
              #x04
              0 0 0 4 0 0 0 20
              0 0 0 4 0 0 0 30)))
      (is (equalp text-wire (encode-value registry 8011 value)))
      (let ((decoded (decode-value registry 8011 text-wire)))
        (is (postgres-multirange-p decoded))
        (is (= 2 (length (postgres-multirange-ranges decoded))))
        (is (= 1 (postgres-range-lower
                  (aref (postgres-multirange-ranges decoded) 0))))
        (is (= 30 (postgres-range-upper
                   (aref (postgres-multirange-ranges decoded) 1)))))
      (is (equalp binary-wire
                  (encode-value registry 8011 value :format 1)))
      (let ((decoded (decode-value registry 8011 binary-wire :format 1)))
        (is (= 2 (length (postgres-multirange-ranges decoded))))
        (is (equalp binary-wire
                    (encode-value registry 8011 decoded :format 1)))))
    (let ((decoded
            (decode-value
             registry
             8011
             (cl-codec-kit:string-to-octets
              "{empty,[1,10),empty}"
              :encoding :utf-8))))
      (is (= 1 (length (postgres-multirange-ranges decoded))))
      (is (= 1 (postgres-range-lower
                (aref (postgres-multirange-ranges decoded) 0))))
      (is (= 10 (postgres-range-upper
                 (aref (postgres-multirange-ranges decoded) 0)))))
    (let* ((range-wire
             (encode-value
              registry
              8002
              (make-postgres-range :lower 1
                                   :upper 10
                                   :lower-inclusive t)
              :format 1))
           (builder (make-octet-builder)))
      (append-i32 builder 2)
      (append-i32 builder 1)
      (append-u8 builder 1)
      (append-i32 builder (length range-wire))
      (append-octets builder range-wire)
      (let ((decoded (decode-value registry 8011 (builder-octets builder)
                                   :format 1)))
        (is (= 1 (length (postgres-multirange-ranges decoded))))))
    (it-signals-each 'protocol-error
        ((:negative-range-count
          (octets #xff #xff #xff #xff))
         (:truncated-range-length
          (octets 0 0 0 1))
         (:truncated-range-payload
          (octets 0 0 0 1 0 0 0 2 1))
         (:trailing-octet-after-range
          (octets 0 0 0 1 0 0 0 1 1 0)))
      "multirange binary codec rejects malformed payload case ~A"
      (label payload)
      (declare (ignore label))
      (decode-value registry 8011 payload :format 1))
    (it-signals-each 'protocol-error
        ((:scalar-text "x")
         (:trailing-comma "{[1,10),}")
         (:missing-separator "{[1,10) [2,3)}")
         (:invalid-member "{foo}"))
      "multirange text codec rejects malformed payload case ~A"
      (label payload)
      (declare (ignore label))
      (decode-value
       registry
       8011
       (cl-codec-kit:string-to-octets payload :encoding :utf-8)))
    (it-signals-each 'parameter-error
        ((:empty-range-member)
         (:non-range-member)
         (:non-multirange-input))
      "multirange rejects invalid construction case ~A"
      (label)
      (ecase label
        (:empty-range-member
         (make-postgres-multirange
          :ranges (list (make-postgres-range :empty-p t))))
        (:non-range-member
         (make-postgres-multirange :ranges (list 1)))
        (:non-multirange-input
         (encode-value registry 8011 1))))))
