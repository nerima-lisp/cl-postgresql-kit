(in-package #:cl-postgresql-kit/test)

(deftest connection-string-parsing
  (is-option-values
   (parse-connection-string
    "host=first.example host=database.example port=5433 user=alice password='s e\\'cret' dbname=app sslmode=verify-ca connect_timeout=5 application_name='my app' options='-c statement_timeout=1000' replication=database")
   (:host "database.example" string=)
   (:port 5433 =)
   (:user "alice" string=)
   (:password "s e'cret" string=)
   (:database "app" string=)
   (:application-name "my app" string=)
   (:ssl-mode :verify-ca eq)
   (:connect-timeout 5 =)
   (:startup-parameters
    '(("options" . "-c statement_timeout=1000")
      ("replication" . "database"))
    equal))
  (is-option-values
   (parse-connection-string "load_balance_hosts=random")
   (:load-balance-hosts :random eq))
  (let ((transport (make-memory-transport)))
    (is-connection-values
     (make-connection-from-string
      "host=database.example port=5433 user=alice dbname=app"
      :transport transport
      :port 5544
      :application-name "override")
     (connection-host "database.example" string=)
     (connection-port 5544 =)
     (connection-application-name "override" string=)
     (connection-transport transport eq)))
  (is-case-each
      ((connection-string expected-database)
       '(("database=first dbname=second" "second")
         ("dbname=first database=second" "second")))
    (is-option-values
     (parse-connection-string connection-string)
     (:database expected-database string=)))
  (is-option-values
   (parse-connection-string
    "postgresql://localhost/path?database=query")
   (:host "localhost" string=)
   (:database "query" string=)))

(deftest connection-uri-parsing
  (is-option-values
   (parse-connection-uri
    "postgresql://alice:secret%20word@[::1]:5433/app%20db?sslmode=verify-full&application_name=my%20app&application_name=last")
   (:host "::1" string=)
   (:port 5433 =)
   (:user "alice" string=)
   (:password "secret word" string=)
   (:database "app db" string=)
   (:application-name "last" string=)
   (:ssl-mode :verify-full eq))
  (is-connection-values
   (make-connection-from-uri
    "postgres://alice@localhost/app"
    :password "override"
    :ssl-mode :disable)
   (connection-host "localhost" string=)
   (connection-user "alice" string=)
   (connection-password "override" string=)
   (connection-ssl-mode :disable eq)))

(deftest connection-parsing-validation
  (let ((options
          (parse-connection-uri
           "postgresql://localhost/app?channel_binding=require")))
    (is (eq :require (getf options :channel-binding))))
  (is (eq :prefer
          (connection-channel-binding (make-connection))))
  (it-signals-each 'parameter-error
      ((:invalid-channel-binding)
       (:invalid-port)
       (:unterminated-string)
       (:invalid-percent-encoding))
    "rejects malformed connection string and URI inputs"
    (label)
    (case label
      (:invalid-channel-binding
       (parse-connection-uri
        "postgresql://localhost/app?channel_binding=invalid"))
      (:invalid-port
       (parse-connection-uri "postgresql://localhost:0/app"))
      (:unterminated-string
       (parse-connection-string "user='unterminated"))
      (:invalid-percent-encoding
       (parse-connection-uri "postgresql://localhost/app?bad=%ZZ")))))

(deftest connection-parser-edge-cases
  (assert-signals-each
   'parameter-error
   (list
    (concatenate 'string "user='abc" (string #\\))
    "user='abc'x"
    (concatenate 'string "user=abc" (string #\\))
    "=value"
    (concatenate 'string "us" (string (code-char 0)) "er=alice")
    "user alice"
    (concatenate 'string "user=a" (string (code-char 0)) "b")
    "port="
    "port=5432,"
    "port=0"
    "port=65536"
    "host=,localhost"
    "hostaddr=,127.0.0.1"
    "connect_timeout=-1"
    "connect_timeout=abc"
    "sslmode=invalid"
    "target_session_attrs=invalid"
    "load_balance_hosts=invalid"
    "postgresql://localhost/app?bad=%"
    "postgresql://localhost/app?bad=%C3%28"
    "postgresql://localhost/app?bad=%00"
    "postgresql://localhost/app#fragment"
    "postgresql://localhost/app?query"
    "postgresql://localhost/app?=value"
    "postgresql://,localhost/app"
    "postgresql://[::1/app"
    "postgresql://[]/app"
    "postgresql://[::1]x/app"
    "postgresql://localhost:/app"
    "postgresql://::1/app"
    "postgresql://localhost:65536/app"
    "mysql://localhost/db")
   (parse-connection-string value))
  (let ((condition
          (with-signaled-condition (condition 'unsupported-feature)
            (parse-connection-string "service=database"))))
    (is (string= "service" (unsupported-feature-name condition)))
    (is (search "service" (princ-to-string condition))))
  (is-option-values
   (parse-connection-uri
    "postgresql://first.example:5433,second.example/app")
   (:hosts '("first.example" "second.example") equal)
   (:ports '(5433 5432) equal))
  (it-signals-each 'parameter-error
      ((:keyword-tail)
       (:odd-arguments))
    "rejects invalid make-connection-from-string argument case ~A"
    (label)
    (case label
      (:keyword-tail
       (make-connection-from-string "" :host))
      (:odd-arguments
       (make-connection-from-string "" "host" "localhost")))))

(deftest tls-option-plumbing
  (let ((options
          (parse-connection-string
           "sslcert=client.crt sslkey=client.key sslpassword='secret' sslrootcert=ca.crt")))
    (is (equal '(:certificate "client.crt"
                 :key "client.key"
                 :password "secret"
                 :verify-location "ca.crt")
               (getf options :tls-options))))
  (let ((options (parse-connection-string "sslrootcert=system")))
    (is (eq :verify-full (getf options :ssl-mode)))
    (is (equal '(:verify-location :default)
               (getf options :tls-options))))
  (let ((options
          (parse-connection-uri
           "postgresql://db.example/app?sslrootcert=ca.crt")))
    (is (equal '(:verify-location "ca.crt")
               (getf options :tls-options))))
  (assert-signals 'parameter-error
                 (lambda ()
                   (parse-connection-string
                    "sslrootcert=system sslmode=require")))
  (let* ((tls-options '(:certificate "client.crt"
                        :key "client.key"
                        :password "secret"
                        :alpn-protocols ("postgresql")
                        :cipher-list "HIGH"
                        :verify-location "ca.crt"))
         (transport (make-socket-transport :tls-options tls-options))
         (connection
           (make-connection :tls-options tls-options)))
    (is (equal tls-options (socket-transport-tls-options transport)))
    (is (equal tls-options (connection-tls-options connection)))
    (is (equal tls-options
               (socket-transport-tls-options (connection-transport connection))))
    (disconnect connection)
    (is (null (connection-tls-options connection)))
    (is (null (socket-transport-tls-options (connection-transport connection)))))
  (assert-signals 'parameter-error
                 (lambda ()
                   (make-connection
                    :tls-options '(:trust-store "ca.pem")))))
