(in-package #:cl-postgresql-kit/test)

(cl-weave:it-each
    (((#xff #xff #xff #xff)))
  "function call response parses SQL NULL payloads"
  (payload-bytes)
  (is (equalp +sql-null+
              (apply #'parse-function-call-response
                     (list (apply #'octets payload-bytes))))))

(it-signals-each 'protocol-error
    (((#xff #xff #xff #xfe))
     ((0 0 0 4 1 2))
     ((0 0 0 1 65 66)))
  "function call response rejects truncated payloads"
  (payload-bytes)
  (parse-function-call-response
   (apply #'octets payload-bytes)))

(cl-weave:it-each
    (((0 2 0 0 0 23 0 0 0 25) #(23 25)))
  "parameter description parses OID vectors"
  (payload-bytes expected)
  (is (equalp expected
              (parse-parameter-description
               (apply #'octets payload-bytes)))))

(it-signals-each 'protocol-error
    (((0 1 0 0))
     ((0 0 1)))
  "parameter description rejects truncated payloads"
  (payload-bytes)
  (parse-parameter-description
   (apply #'octets payload-bytes)))
