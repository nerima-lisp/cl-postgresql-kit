(in-package #:cl-postgresql-kit/test)

(deftest gss-and-sspi-authentication-use-token-providers
  (let ((calls nil))
    (with-test-connection
        (connection
         (ready-memory-connection
          :gss-token-provider
          (lambda (ignored-connection type data)
            (declare (ignore ignored-connection))
            (push (list type data) calls)
            (if (eq type :gss)
                (octets 1 2)
                (octets 3 4)))))
      (cl-postgresql-kit::%handle-authentication
       connection
       '(:type :gss :data #()))
      (cl-postgresql-kit::%handle-authentication
       connection
       (list :type :gss-continue :data (octets 5 6)))
      (is (equalp '((:gss #()) (:gss-continue #(5 6)))
                  (reverse calls)))
      (is (equalp
           (join-octets (encode-gss-response (octets 1 2))
                        (encode-gss-response (octets 3 4)))
           (memory-transport-output (connection-transport connection))))))
  (let ((calls nil))
    (with-test-connection
        (connection
         (ready-memory-connection
          :sspi-token-provider
          (lambda (ignored-connection type data)
            (declare (ignore ignored-connection))
            (push (list type data) calls)
            (octets 7 8))))
      (cl-postgresql-kit::%handle-authentication
       connection
       (list :type :sspi :data (octets 9)))
      (is (equalp '((:sspi #(9))) calls))
      (is (equalp (encode-gss-response (octets 7 8))
                  (memory-transport-output (connection-transport connection))))))
  (with-test-connection
      (connection (ready-memory-connection :ssl-mode :verify-full))
    (assert-signals
     'authentication-error
     (lambda ()
       (cl-postgresql-kit::%handle-authentication
        connection
        '(:type :gss :data #()))))))

(deftest cleartext-authentication-requires-verified-tls
  (with-test-connection
      (connection (ready-memory-connection :password "secret"))
    (assert-signals 'authentication-error
                    (lambda ()
                      (cl-postgresql-kit::%authenticate-cleartext connection)))))

(deftest authentication-methods-reject-missing-credentials
  (with-test-connection
      (connection (ready-memory-connection))
    (it-signals-each 'authentication-error
        ((:md5)
         (:scram)
         (:cleartext)
         (:scram-plus)
         (:oauthbearer))
      "rejects missing authentication credential case ~A"
      (label)
      (case label
        (:md5
         (cl-postgresql-kit::%authenticate-md5
          connection (octets 1 2 3 4)))
        (:scram
         (cl-postgresql-kit::%authenticate-scram connection))
        (:cleartext
         (setf (connection-tls-established-p connection) t)
         (unwind-protect
              (cl-postgresql-kit::%authenticate-cleartext connection)
           (setf (connection-tls-established-p connection) nil)))
        (:scram-plus
         (setf (connection-password connection) "secret")
         (cl-postgresql-kit::%authenticate-scram
          connection :mechanism "SCRAM-SHA-256-PLUS"))
        (:oauthbearer
         (cl-postgresql-kit::%authenticate-oauthbearer connection))))))

(deftest oauthbearer-requires-provider-and-string-token
  (with-test-connection
      (connection (ready-memory-connection))
    (setf (connection-tls-established-p connection) t)
    (assert-signals 'authentication-error
                    (lambda ()
                      (cl-postgresql-kit::%authenticate-oauthbearer connection)))
    (setf (connection-oauth-token-provider connection)
          (lambda (ignored)
            (declare (ignore ignored))
            42))
    (assert-signals 'authentication-error
                    (lambda ()
                      (cl-postgresql-kit::%authenticate-oauthbearer connection)))))

(deftest authentication-negotiation-rejects-unsupported-methods
  (with-test-connection
      (connection (ready-memory-connection))
    (assert-signals 'authentication-error
                    (lambda ()
                      (cl-postgresql-kit::%handle-authentication
                       connection '(:type :unsupported))))
    (assert-signals 'authentication-error
                    (lambda ()
                      (cl-postgresql-kit::%handle-authentication
                       connection '(:type :sasl :mechanisms nil))))))

(deftest md5-authentication-writes-cstring-password-message
  (let* ((transport (make-memory-transport))
         (connection (make-connection :transport transport
                                      :user "alice"
                                      :password "secret")))
    (transport-open transport)
    (setf (connection-open connection) t
          (connection-state connection) :ready)
    (unwind-protect
         (progn
           (cl-postgresql-kit::%authenticate-md5 connection (octets 1 2 3 4))
           (is (equalp (memory-transport-output transport)
                       (encode-password-message
                        "md598a0412b9c31436fc53776e863350083"))))
      (disconnect connection))))

(deftest cleartext-authentication-writes-cstring-password-message
  (with-test-connection
      (connection (ready-memory-connection
                   :password "secret"
                   :ssl-mode :verify-full))
    (setf (connection-tls-established-p connection) t)
    (cl-postgresql-kit::%authenticate-cleartext connection)
    (is (equalp (memory-transport-output (connection-transport connection))
                (encode-password-message "secret")))))

(deftest authentication-dispatches-password-methods
  (is-case-each
      ((type &key tls salt)
       '((:cleartext-password :tls t)
         (:md5-password :salt (1 2 3 4))))
    (with-test-connection
        (connection (ready-memory-connection
                     :password "secret"
                     :ssl-mode :verify-full))
      (setf (connection-tls-established-p connection) tls)
      (cl-postgresql-kit::%handle-authentication
       connection
       (list :type type :salt (and salt (apply #'octets salt))))
      (is (plusp (length (memory-transport-output
                          (connection-transport connection))))))))
