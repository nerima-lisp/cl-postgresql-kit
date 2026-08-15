(in-package #:cl-postgresql-kit/test)

(deftest crypto-known-vectors
  (is (string= "900150983CD24FB0D6963F7D28E17F72"
               (octets-as-hex (md5-digest "abc"))))
  (is (string= "BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD"
               (octets-as-hex (sha256-digest "abc"))))
  (is (string= "F7BC83F430538424B13298E6AA6FB143EF4D59A14946175997479DBC2D1A3CD8"
               (octets-as-hex
                (hmac-sha256 "key" "The quick brown fox jumps over the lazy dog"))))
  (is (equalp (cl-codec-kit:string-to-octets "foo" :encoding :utf-8)
              (base64-to-octets "Zm9v"))))
