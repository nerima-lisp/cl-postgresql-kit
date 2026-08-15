(in-package #:cl-postgresql-kit/test)

(deftest oauthbearer-authentication
  (let ((connection
          (make-oauthbearer-test-connection
           :input (make-frame #\R (octets 0 0 0 12))
           :tls-established-p t)))
    (unwind-protect
         (progn
           (cl-postgresql-kit::%authenticate-oauthbearer connection)
           (assert-oauthbearer-output connection))
      (disconnect connection))))

(deftest oauthbearer-authentication-boundaries
  (it-signals-each 'authentication-error
      ((:missing-oauth-provider)
       (:missing-oauth-token)
       (:non-string-oauth-token))
    "rejects oauthbearer authentication setup case ~A"
    (label)
    (let ((connection
            (case label
              (:missing-oauth-provider
               (ready-memory-connection))
              (:missing-oauth-token
               (make-oauthbearer-test-connection
                :oauth-token-provider nil))
              (:non-string-oauth-token
               (make-oauthbearer-test-connection
                :oauth-token-provider
                (lambda (ignored)
                  (declare (ignore ignored))
                  42)
                :tls-established-p t)))))
      (unwind-protect
           (cl-postgresql-kit::%authenticate-oauthbearer connection)
        (disconnect connection))))
  (labels ((assert-oauth-error (input)
             (let ((connection
                     (make-oauthbearer-test-connection
                      :input input
                      :tls-established-p t)))
               (unwind-protect
                    (assert-signals 'authentication-error
                                    (lambda ()
                                      (cl-postgresql-kit::%authenticate-oauthbearer
                                       connection)))
                 (disconnect connection)))))
    (let ((connection
            (make-oauthbearer-test-connection
             :input
             (join-octets
              (make-frame #\R (octets 0 0 0 11))
              (make-frame #\R (octets 0 0 0 12)))
             :tls-established-p t)))
      (unwind-protect
           (progn
             (cl-postgresql-kit::%authenticate-oauthbearer connection)
             (assert-oauthbearer-output connection))
        (disconnect connection)))
    (it-signals-each 'authentication-error
        ((:sasl-continue
          (make-frame #\R
                      (join-octets
                       (octets 0 0 0 11)
                       (cl-codec-kit:string-to-octets "challenge"))))
         (:sasl-final
          (make-frame #\R
                      (join-octets
                       (octets 0 0 0 12)
                       (cl-codec-kit:string-to-octets "error"))))
         (:ready-for-query
          (make-frame #\Z (octets (char-code #\I))))
         (:short-auth-frame
          (make-frame #\R (octets 0 0 0 0))))
      "rejects oauthbearer server response case ~A"
      (label input)
      (assert-oauth-error input))))
