(in-package #:cl-postgresql-kit/test)

(deftest bit-and-mac-address-codecs
  (let ((registry (make-type-registry)))
    (let* ((bit-text (cl-codec-kit:string-to-octets "010011" :encoding :utf-8))
           (bit (decode-value registry 1560 bit-text))
           (varbit (decode-value registry 1562 bit-text)))
      (is (postgres-bit-string-p bit))
      (is (string= "010011" (postgres-bit-string-bits bit)))
      (is (null (postgres-bit-string-varying-p bit)))
      (is (postgres-bit-string-varying-p varbit))
      (is (equalp bit-text (encode-value registry 1560 bit)))
      (is (equalp bit-text (encode-value registry 1562 varbit))))
    (let* ((binary (octets 0 0 0 9 128 128))
           (bit (decode-value registry 1560 binary :format 1)))
      (is (string= "100000001" (postgres-bit-string-bits bit)))
      (is (equalp binary (encode-value registry 1560 bit :format 1))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (decode-value registry 1560 (octets 0 0 0 1) :format 1)))
    (assert-signals 'parameter-error
                    (lambda ()
                      (encode-value registry 1560 "0102")))
    (let* ((mac-text "08:00:2b:01:02:03")
           (mac (decode-value
                 registry 829
                 (cl-codec-kit:string-to-octets mac-text :encoding :utf-8)))
           (mac-binary (encode-value registry 829 mac :format 1))
           (mac8 (decode-value
                  registry 774
                  (cl-codec-kit:string-to-octets
                   "0800.2bff.fe01.0203" :encoding :utf-8))))
      (is (postgres-mac-address-p mac))
      (is (equalp (octets 8 0 43 1 2 3)
                  (postgres-mac-address-octets mac)))
      (is (string= mac-text
                   (cl-codec-kit:octets-to-string
                    (encode-value registry 829 mac) :encoding :utf-8)))
      (is (equalp (octets 8 0 43 1 2 3) mac-binary))
      (is (equalp (octets 8 0 43 255 254 1 2 3)
                  (postgres-mac-address-octets mac8)))
      (is (equalp (octets 8 0 43 1 2 3)
                  (encode-value
                   registry 829
                   (make-postgres-mac-address
                    :octets '(8 0 43 1 2 3))
                   :format 1))))
    (is-each (oid '(775 1040 1561 1563))
      (is (functionp (type-codec-binary-decoder
                      (find-type-codec registry oid)))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (decode-value
                       registry 829
                       (cl-codec-kit:string-to-octets
                        "08:00:2b:01:02:zz" :encoding :utf-8))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (decode-value registry 829 (octets 8 0 43 1 2)
                                    :format 1)))
    (let ((mac8-binary (decode-value registry 774
                                     (octets 8 0 43 255 254 1 2 3)
                                     :format 1)))
      (is (equalp (octets 8 0 43 255 254 1 2 3)
                  (postgres-mac-address-octets mac8-binary))))
    (assert-signals 'parameter-error
                    (lambda ()
                      (encode-value registry 774 "08:00:2b:01:02:03")))))

(deftest network-codec-boundaries
  (with-selected-default-codecs (registry '(650 651 869 1041))
    (let* ((cidr-wire (cl-codec-kit:string-to-octets
                       "192.0.2.0/24"
                       :encoding :utf-8))
           (cidr (decode-value registry 650 cidr-wire))
           (inet (decode-value
                  registry
                  869
                  (cl-codec-kit:string-to-octets
                   "192.0.2.1/24"
                   :encoding :utf-8)))
           (ipv6 (decode-value
                  registry
                  869
                  (cl-codec-kit:string-to-octets
                   "2001:0DB8:0:0:0:0:0:1/64"
                   :encoding :utf-8))))
      (is (postgres-inet-p cidr))
      (is (= 4 (postgres-inet-family cidr)))
      (is (= 24 (postgres-inet-netmask cidr)))
      (is (postgres-inet-cidr-p cidr))
      (is (equalp cidr-wire (encode-value registry 650 cidr)))
      (is (not (postgres-inet-cidr-p inet)))
      (is (equalp
           (cl-codec-kit:string-to-octets
            "192.0.2.1/24"
            :encoding :utf-8)
           (encode-value registry 869 inet)))
      (is (equalp
           (cl-codec-kit:string-to-octets
            "2001:db8::1/64"
            :encoding :utf-8)
           (encode-value registry 869 ipv6))))
    (let* ((wire (octets 2 32 0 4 192 0 2 1))
           (value (decode-value registry 869 wire :format 1)))
      (is (= 4 (postgres-inet-family value)))
      (is (= 32 (postgres-inet-netmask value)))
      (is (equalp wire (encode-value registry 869 value :format 1))))
    (let* ((wire (octets 3 32 1 16
                         32 1 #x0d #xb8
                         0 0 0 0 0 0 0 0 0 0 0 0))
           (value (decode-value registry 650 wire :format 1)))
      (is (postgres-inet-cidr-p value))
      (is (equalp wire (encode-value registry 650 value :format 1))))
    (let* ((array (decode-value
                   registry
                   1041
                   (cl-codec-kit:string-to-octets
                    "{\"192.0.2.1\",\"198.51.100.2\"}"
                    :encoding :utf-8)))
           (wire (encode-value registry 1041 array :format 1))
           (round-trip (decode-value registry 1041 wire :format 1)))
      (is (postgres-array-p round-trip))
      (is (every #'postgres-inet-p
                 (coerce (postgres-array-elements round-trip) 'list))))
    (it-signals-each 'protocol-error
        ((:ipv4-netmask-overflow
          (octets 2 33 0 4 192 0 2 1))
         (:ipv4-length-mismatch
          (octets 2 32 0 16 192 0 2 1))
         (:unknown-address-family
          (octets 4 32 0 4 192 0 2 1))
         (:unknown-cidr-flag
          (octets 2 32 2 4 192 0 2 1)))
      "network codec rejects malformed inet binary payload case ~A"
      (label payload)
      (declare (ignore label))
      (decode-value registry 869 payload :format 1))
    (it-signals-each 'protocol-error
        ((:inet-invalid-mask-width
          (octets 2 32 0 4 192 0 2 0)
          1)
         (:inet-text-in-binary-format
          (cl-codec-kit:string-to-octets
           "192.0.2.1/24"
           :encoding :utf-8)
          0))
      "network codec rejects malformed inet payload case ~A"
      (label payload format)
      (declare (ignore label))
      (decode-value registry 650 payload :format format))
    (it-signals-each 'protocol-error
        ((:ipv6-double-compression
          "2001:::1")
         (:ipv6-duplicate-double-colon
          "2001:db8::1::2")
         (:ipv4-mask-overflow
          "1.2.3.4/33"))
      "network codec rejects malformed inet text payload case ~A"
      (label payload)
      (declare (ignore label))
      (decode-value
       registry
       869
       (cl-codec-kit:string-to-octets
        payload
        :encoding :utf-8)))
    (it-signals-each 'parameter-error
        ((:cidr-host-bits)
         (:invalid-octet-count))
      "inet rejects invalid construction case ~A"
      (label)
      (ecase label
        (:cidr-host-bits
         (make-postgres-inet
          :octets #(192 0 2 1)
          :netmask 24
          :cidr-p t))
        (:invalid-octet-count
         (make-postgres-inet :octets #(1 2 3)))))))
