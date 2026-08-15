(in-package #:cl-postgresql-kit/test)

(deftest binary-boolean-codec-boundaries
  (let ((registry (make-type-registry)))
    (is (null (decode-value registry 16 (octets 0) :format 1)))
    (is (eq t (decode-value registry 16 (octets 1) :format 1)))
    (is (equalp (octets 0)
                (encode-value registry 16 nil :format 1)))
    (is (equalp (octets 1)
                (encode-value registry 16 t :format 1)))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value registry 16 #() :format 1)))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value registry 16 (octets 0 1) :format 1)))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value registry 16 (octets 2) :format 1)))))

(deftest numeric-codecs
  (let ((registry (make-type-registry)))
    (is (= 5
           (decode-value
            registry 1700
            (cl-codec-kit:string-to-octets
             "  +.5e+1  " :encoding :utf-8))))
    (is (= (- 1/20)
           (decode-value
            registry 1700
            (cl-codec-kit:string-to-octets
             "-5.e-2" :encoding :utf-8))))
    (is (zerop
         (decode-value
          registry 1700
          (cl-codec-kit:string-to-octets
           "-0.00" :encoding :utf-8))))
    (is (equalp
         (cl-codec-kit:string-to-octets "0" :encoding :utf-8)
         (encode-value registry 1700 0)))
    (is (= 1.5
           (decode-value registry 1700
                         (encode-value registry 1700 1.5f0))))
    (is (equalp
         (cl-codec-kit:string-to-octets "NaN" :encoding :utf-8)
         (encode-value registry 1700 "NaN")))
    (is (= (/ 1234567890123456789012345 100000)
           (decode-value
            registry 1700
            (cl-codec-kit:string-to-octets
             "12345678901234567890.12345" :encoding :utf-8))))
    (is (equalp
         (cl-codec-kit:string-to-octets "-0.0012" :encoding :utf-8)
         (encode-value registry 1700 (/ -3 2500))))
    (assert-signals 'parameter-error
                   (lambda () (encode-value registry 1700 '(1 2 3))))
    (let ((binary (octets 0 2 0 0 0 0 0 2 0 123 17 148)))
      (is (= (/ 2469 20)
             (decode-value registry 1700 binary :format 1)))
      (is (equalp binary
                  (encode-value registry 1700 (/ 2469 20) :format 1))))
    (let ((binary (encode-value registry 1700 (- (/ 2469 20)) :format 1)))
      (is (= (- (/ 2469 20))
             (decode-value registry 1700 binary :format 1))))
    (is (string= "NaN"
                 (decode-value registry 1700
                               (octets 0 0 0 0 192 0 0 0)
                               :format 1)))
    (is (equalp (octets 0 0 0 0 192 0 0 0)
                (encode-value registry 1700 "NaN" :format 1)))
    (is (equalp (octets 0 0 0 0 0 0 0 0)
                (encode-value registry 1700 0 :format 1)))
    (is (zerop (decode-value registry 1700
                             (octets 0 0 0 0 0 0 0 0)
                             :format 1)))
    (is (zerop (decode-value registry 1700
                             (octets 0 0 0 0 64 0 0 0)
                             :format 1)))
    (is-each (payload '(#()
                        #(0 0 0 0 0 1 0 0)
                        #(0 0 0 0 0 0 255 255)
                        #(0 1 0 0 192 0 0 0 0 1)
                        #(0 1 0 0 0 0 0 0 39 16)
                        #(0 1 0 0 0 0 0 0)
                        #(256 0 0 0 0 0 0 0)))
      (assert-signals 'protocol-error
                     (lambda ()
                       (decode-value registry 1700 payload :format 1))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 1700
                      (cl-codec-kit:string-to-octets
                       "1e100001" :encoding :utf-8))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 1700
                      (cl-codec-kit:string-to-octets
                       (concatenate
                        'string "0."
                        (make-string (1+ *maximum-numeric-digits*)
                                     :initial-element #\0))
                       :encoding :utf-8))))
    (is-each (value '("" "." "1.2.3" "1e" "1e+" "1x" "1e2x"))
      (assert-signals 'protocol-error
                     (lambda ()
                       (decode-value
                        registry 1700
                        (cl-codec-kit:string-to-octets value
                                                       :encoding :utf-8)))))
    (it-signals-each 'parameter-error
        ((:invalid-numeric-string)
         (:binary-format-string)
         (:non-terminating-ratio))
      "numeric encoder rejects invalid input case ~A"
      (label)
      (ecase label
        (:invalid-numeric-string
         (encode-value registry 1700 "1x"))
        (:binary-format-string
         (encode-value registry 1700 "Infinity" :format 1))
        (:non-terminating-ratio
         (encode-value registry 1700 (/ 1 3)))))))
