(in-package #:cl-postgresql-kit/test)

(deftest floating-point-binary-codecs
  (let ((registry (make-type-registry)))
    (let ((float4 (decode-value registry 700 (octets 63 192 0 0) :format 1))
          (float8 (decode-value registry 701
                                (octets 63 248 0 0 0 0 0 0)
                                :format 1)))
      (is (typep float4 'single-float))
      (is (= 1.5f0 float4))
      (is (typep float8 'double-float))
      (is (= 1.5d0 float8))
      (is (equalp (octets 63 192 0 0)
                  (encode-value registry 700 float4 :format 1)))
      (is (equalp (octets 63 248 0 0 0 0 0 0)
                  (encode-value registry 701 float8 :format 1))))
    (let ((negative (decode-value registry 700 (octets 192 32 0 0)
                                    :format 1)))
      (is (= -2.5f0 negative))
      (is (equalp (octets 192 32 0 0)
                  (encode-value registry 700 negative :format 1))))
    (is (string= "Infinity"
                 (decode-value registry 700 (octets 127 128 0 0)
                               :format 1)))
    (is (string= "-Infinity"
                 (decode-value registry 700 (octets 255 128 0 0)
                               :format 1)))
    (is (string= "NaN"
                 (decode-value registry 700 (octets 127 192 0 0)
                               :format 1)))
    (is (equalp (octets 127 128 0 0)
                (encode-value registry 700 "Infinity" :format 1)))
    (is (equalp (octets 255 128 0 0)
                (encode-value registry 700 "-Infinity" :format 1)))
    (is (equalp (octets 127 128 0 1)
                (encode-value registry 700 "NaN" :format 1)))
    (let ((subnormal (decode-value registry 700 (octets 0 0 0 1)
                                      :format 1)))
      (is (plusp subnormal))
      (is (equalp (octets 0 0 0 1)
                  (encode-value registry 700 subnormal :format 1))))
    (let ((negative-zero (decode-value registry 700 (octets 128 0 0 0)
                                          :format 1)))
      (is (= 0.0f0 negative-zero))
      (is (equalp (octets 128 0 0 0)
                  (encode-value registry 700 negative-zero :format 1))))
    (let ((wire (encode-value registry 700 1.5f0)))
      (is (= 1.5f0
             (decode-value registry 700 wire))))
    (is (string= "Infinity"
                 (cl-codec-kit:octets-to-string
                  (encode-value registry 700 "+Infinity")
                  :encoding :utf-8)))
    (is (string= "Infinity"
                 (decode-value
                  registry 701
                  (cl-codec-kit:string-to-octets "Infinity" :encoding :utf-8))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value registry 701 (octets 0 0 0) :format 1)))
    (assert-signals 'parameter-error
                   (lambda ()
                     (encode-value registry 700 #C(1 2) :format 1)))))
