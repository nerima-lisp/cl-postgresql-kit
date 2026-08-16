(in-package #:cl-postgresql-kit/test)

(defmacro with-test-connection-file ((var contents) &body body)
  `(let ((,var (merge-pathnames
                (format nil "cl-postgresql-kit-~A.conf" (gensym))
                (uiop:temporary-directory))))
     (unwind-protect
          (progn
            (with-open-file (stream ,var
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
              (write-string ,contents stream))
            ,@body)
       (when (probe-file ,var)
         (delete-file ,var)))))

(defun call-with-test-environment-variable (name value thunk)
  #+sbcl
  (let ((old-value (uiop:getenv name)))
    (unwind-protect
         (progn
           (require :sb-posix)
           (uiop:symbol-call :sb-posix :setenv name value 1)
           (funcall thunk))
      (if old-value
          (uiop:symbol-call :sb-posix :setenv name old-value 1)
          (uiop:symbol-call :sb-posix :unsetenv name))))
  #-sbcl
  (declare (ignore name value))
  #-sbcl
  (funcall thunk))

(defmacro with-test-environment-variable ((name value) &body body)
  `(call-with-test-environment-variable ,name ,value
                                        (lambda () ,@body)))

(deftest connection-string-parsing
  (is-option-values
   (parse-connection-string
    "host=first.example host=database.example port=5433 user=alice password='s e\\'cret' dbname=app sslmode=verify-ca connect_timeout=5 application_name='my app' options='-c statement_timeout=1000' replication=database require_auth='scram-sha-256,oauth'")
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
   (parse-connection-string "require_auth='scram-sha-256,oauth'")
   (:require-auth
    '(:mode :allow :methods (:scram-sha-256 :oauth))
    equal))
    (is-option-values
     (parse-connection-string "load_balance_hosts=random")
     (:load-balance-hosts :random eq))
  #+sbcl
  (with-test-environment-variable ("PGSERVICE" "")
    (with-test-environment-variable ("PGHOST" "env.example")
      (with-test-environment-variable ("PGPORT" "5444")
        (with-test-environment-variable ("PGDATABASE" "env-db")
          (with-test-environment-variable ("PGUSER" "env-user")
            (with-test-environment-variable ("PGAPPNAME" "env-app")
              (with-test-environment-variable
                  ("PGSSLMINPROTOCOLVERSION" "TLSv1.2")
                (with-test-environment-variable
                    ("PGSSLMAXPROTOCOLVERSION" "TLSv1.3")
                  (is-option-values
                   (parse-connection-string "")
                   (:host "env.example" string=)
                   (:port 5444 =)
                   (:database "env-db" string=)
                   (:user "env-user" string=)
                   (:application-name "env-app" string=)
                   (:tls-options
                    '(:min-proto-version :tlsv1-2
                      :max-proto-version :tlsv1-3)
                    equal))))))))))
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
  (let ((default-host #+win32 "127.0.0.1" #-win32 "/tmp"))
    (is-option-values
     (parse-connection-uri "postgresql:///app")
     (:host default-host string=)
     (:database "app" string=))
    (is-option-values
     (parse-connection-uri "postgresql://%2Ftmp/app")
     (:host "/tmp" string=)))

(deftest connection-parsing-validation
  (let ((options
          (parse-connection-uri
           "postgresql://localhost/app?channel_binding=require")))
    (is (eq :require (getf options :channel-binding))))
  (is (eq :prefer
          (connection-channel-binding (make-connection))))
  (is (equal '(:mode :deny :methods (:password :none))
             (connection-require-auth
              (make-connection :require-auth "!password,!none"))))
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
    "port=0"
    "port=65536"
    "connect_timeout=-1"
    "connect_timeout=abc"
    "require_auth="
    "require_auth=unknown"
    "require_auth=!"
    "require_auth=scram-sha-256,!md5"
    "require_auth=md5,md5"
    "sslmode=invalid"
    "target_session_attrs=invalid"
    "load_balance_hosts=invalid"
    "postgresql://localhost/app?bad=%"
    "postgresql://localhost/app?bad=%C3%28"
    "postgresql://localhost/app?bad=%00"
    "postgresql://localhost/app#fragment"
    "postgresql://localhost/app?query"
    "postgresql://localhost/app?=value"
    "postgresql://[::1/app"
    "postgresql://[]/app"
    "postgresql://[::1]x/app"
    "postgresql://::1/app"
    "postgresql://localhost:65536/app"
    "mysql://localhost/db")
   (parse-connection-string value))
  #+sbcl
  (with-test-connection-file
      (service-file
       (format nil
               "[base]~%host=db.example~%port=5433~%user=alice~%dbname=app~%password=from-service~%[app]~%service=base~%application_name=from-profile~%"))
    (with-test-environment-variable ("PGSERVICEFILE" (namestring service-file))
      (with-test-environment-variable ("PGSERVICE" "app")
        (is-option-values
         (parse-connection-string "service=app host=override")
         (:host "override" string=)
         (:port 5433 =)
         (:user "alice" string=)
         (:database "app" string=)
         (:password "from-service" string=)
         (:application-name "from-profile" string=))
        (is-option-values
         (parse-connection-string "host=override")
         (:host "override" string=)
         (:port 5433 =)
         (:user "alice" string=)
         (:database "app" string=)
         (:password "from-service" string=)
         (:application-name "from-profile" string=)))))
  #+sbcl
  (with-test-connection-file
      (passfile
       (format nil "db.example:5433:app:alice:s3cr\\:t~%"))
    (is-option-values
     (parse-connection-string
      (format nil "host=db.example port=5433 user=alice dbname=app passfile=~A"
              (namestring passfile)))
     (:password "s3cr:t" string=)
     (:passfile (namestring passfile) string=))
    (is-option-values
     (parse-connection-string
      (format nil "host=db.example port=5433 user=alice dbname=app password=explicit passfile=~A"
              (namestring passfile)))
     (:password "explicit" string=))
    (is-connection-values
     (make-connection-from-string
      (format nil "host=db.example port=5433 user=alice dbname=app passfile=~A"
              (namestring passfile)))
     (connection-password "s3cr:t" string=)
     (connection-passfile (namestring passfile) string=))
    (let ((connection
            (make-connection :host "db.example"
                             :port 5433
                             :user "alice"
                             :database "app"
                             :passfile passfile)))
      (is (string= "s3cr:t" (connection-password connection)))
      (is (equal passfile (connection-passfile connection)))
      (disconnect connection)))
  (let ((default-host #+win32 "127.0.0.1" #-win32 "/tmp"))
    (is-option-values
     (parse-connection-string "host=")
     (:host default-host string=))
    (is-option-values
     (parse-connection-string
      "host=,localhost hostaddr=,127.0.0.1 port=,5433")
     (:hosts '("" "localhost") equal)
     (:hostaddrs '("" "127.0.0.1") equal)
     (:ports '(5432 5433) equal))
    (is (equal
         (list (list :host default-host :hostaddr default-host :port 5432)
               (list :host "localhost" :hostaddr "127.0.0.1" :port 5433))
         (cl-postgresql-kit::%make-connection-endpoints
          :host ""
          :hosts '("" "localhost")
          :hostaddr ""
          :hostaddrs '("" "127.0.0.1")
          :port 5432
          :ports '(5432 5433)
          :host-supplied-p t))))
  (is-option-values
   (parse-connection-uri
    "postgresql://,localhost:5433/app")
   (:hosts '("" "localhost") equal)
   (:ports '(5432 5433) equal))
  (is-option-values
   (parse-connection-uri "postgresql://localhost:/app")
   (:host "localhost" string=))
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
                    "sslcert=client.crt sslkey=client.key sslpassword='secret' sslrootcert=ca.crt ssl_min_protocol_version=TLSv1.2 ssl_max_protocol_version=TLSv1.3")))
    (is (equal '(:certificate "client.crt"
                 :key "client.key"
                 :password "secret"
                 :verify-location "ca.crt"
                 :min-proto-version :tlsv1-2
                 :max-proto-version :tlsv1-3)
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
