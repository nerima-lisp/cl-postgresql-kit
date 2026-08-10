(in-package #:cl-postgresql-kit/test)

(deftest cancel-request-writes-control-packet
  (let ((cancel-transport nil)
        (connection nil))
    (unwind-protect
         (progn
           (setf connection
                 (ready-memory-connection
                  :cancel-transport-factory
                  (lambda (ignored)
                    (declare (ignore ignored))
                    (setf cancel-transport (make-memory-transport))
                    cancel-transport)))
           (setf (connection-backend-process-id connection) 42
                 (connection-backend-secret-key connection) 99)
           (is (cancel-request connection))
           (is (equalp (memory-transport-output cancel-transport)
                       (encode-cancel-request 42 99))))
      (when connection (disconnect connection)))))

(deftest gss-and-sspi-authentication-use-token-providers
  (let* ((calls nil)
        (connection
          (ready-memory-connection
           :gss-token-provider
           (lambda (ignored-connection type data)
             (declare (ignore ignored-connection))
             (push (list type data) calls)
             (if (eq type :gss)
                 (octets 1 2)
                 (octets 3 4))))))
    (unwind-protect
         (progn
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
                (memory-transport-output (connection-transport connection)))))
      (disconnect connection)))
  (let* ((calls nil)
        (connection
          (ready-memory-connection
           :sspi-token-provider
           (lambda (ignored-connection type data)
             (declare (ignore ignored-connection))
             (push (list type data) calls)
             (octets 7 8)))))
    (unwind-protect
         (progn
           (cl-postgresql-kit::%handle-authentication
            connection
            (list :type :sspi :data (octets 9)))
           (is (equalp '((:sspi #(9))) calls))
           (is (equalp (encode-gss-response (octets 7 8))
                       (memory-transport-output (connection-transport connection)))))
      (disconnect connection)))
  (let ((connection (ready-memory-connection)))
    (unwind-protect
         (assert-signals
          'authentication-error
          (lambda ()
            (cl-postgresql-kit::%handle-authentication
             connection
             '(:type :gss :data #()))))
      (disconnect connection))))

(deftest protocol-negotiation-updates-connection-state
  (let ((connection (ready-memory-connection)))
    (unwind-protect
         (let ((negotiation (make-octet-builder))
               (backend-key (make-octet-builder)))
           (append-i32 negotiation 2)
           (append-i32 negotiation 0)
           (multiple-value-bind (kind ignored)
               (cl-postgresql-kit::%process-backend-message
                connection
                (parse-frame (make-frame #\v
                                         (builder-octets negotiation))))
             (declare (ignore ignored))
             (is (eq :negotiate-protocol-version kind)))
           (is (= +protocol-version-3.2+
                  (connection-negotiated-protocol-version connection)))
           (append-i32 backend-key 42)
           (append-octets backend-key (octets 10 11 12 13 14))
           (cl-postgresql-kit::%process-backend-message
            connection
            (parse-frame (make-frame #\K (builder-octets backend-key))))
           (is (= 42 (connection-backend-process-id connection)))
           (is (equalp (octets 10 11 12 13 14)
                       (connection-backend-secret-key connection)))
           (cl-postgresql-kit::%clear-connection-session-state connection)
           (is (null (connection-negotiated-protocol-version connection))))
      (disconnect connection))))

(deftest cleartext-authentication-requires-verified-tls
  (let ((connection (ready-memory-connection :password "secret")))
    (unwind-protect
         (assert-signals 'authentication-error
                        (lambda ()
                          (cl-postgresql-kit::%authenticate-cleartext connection)))
      (disconnect connection))))

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
  (let ((connection (ready-memory-connection
                     :password "secret"
                     :ssl-mode :verify-full)))
    (setf (connection-tls-established-p connection) t)
    (unwind-protect
         (progn
           (cl-postgresql-kit::%authenticate-cleartext connection)
           (is (equalp (memory-transport-output (connection-transport connection))
                       (encode-password-message "secret"))))
      (disconnect connection))))

(deftest ssl-verification-modes-map-to-transport
  (dolist (spec '((:require "127.0.0.1" nil)
                  (:verify-ca nil t)
                  (:verify-full "127.0.0.1" t)))
    (destructuring-bind (mode expected-host expected-verify) spec
      (let* ((transport (make-instance 'tls-probe-transport
                                       :input (octets (char-code #\S))))
             (connection (make-connection :host "127.0.0.1"
                                          :transport transport
                                          :ssl-mode mode)))
        (transport-open transport)
        (unwind-protect
             (progn
               (cl-postgresql-kit::%ssl-request connection)
               (let ((options (tls-probe-options transport)))
                 (is (equal (getf options :hostname) expected-host))
                 (is (eq (getf options :verify) expected-verify)))
               (is (connection-tls-established-p connection)))
          (disconnect connection))))))

(deftest sslmode-startup-order-and-fallback
  (let* ((writes nil)
         (transport
           (make-instance
            'tls-probe-transport
            :on-write
            (lambda (transport octets)
              (push (copy-seq octets) writes)
              (if (equalp octets (encode-ssl-request))
                  (memory-transport-append-input
                   transport
                   (octets (char-code #\N)))
                  (append-ready-command transport "STARTUP")))))
         (connection (make-connection :user "alice"
                                      :host "127.0.0.1"
                                      :transport transport
                                      :ssl-mode :prefer)))
    (transport-open transport)
    (unwind-protect
         (progn
           (connect connection)
           (let ((ordered (reverse writes)))
             (is (= 2 (length ordered)))
             (is (equalp (first ordered) (encode-ssl-request)))
             (is (not (equalp (second ordered) (encode-ssl-request)))))
           (is (null (tls-probe-options transport)))
           (is (connection-open connection)))
      (disconnect connection)))
  (let* ((writes nil)
         (transport
           (make-instance
            'tls-probe-transport
            :on-write
            (lambda (transport octets)
              (push (copy-seq octets) writes)
              (unless (equalp octets (encode-ssl-request))
                (append-ready-command transport "STARTUP")))))
         (connection (make-connection :user "alice"
                                      :host "127.0.0.1"
                                      :transport transport
                                      :ssl-mode :allow)))
    (transport-open transport)
    (unwind-protect
         (progn
           (connect connection)
           (let ((ordered (reverse writes)))
             (is (= 1 (length ordered)))
             (is (not (equalp (first ordered) (encode-ssl-request)))))
           (is (null (tls-probe-options transport)))
           (is (connection-open connection)))
      (disconnect connection)))
  (let* ((writes nil)
         (ssl-required-error
           (make-frame
            #\E
            (join-octets (octets (char-code #\S))
                         (cstring "ERROR")
                         (octets (char-code #\C))
                         (cstring "08004")
                         (octets (char-code #\M))
                         (cstring "SSL required")
                         (octets 0))))
         (plain-attempt-p t)
         (transport
           (make-instance
            'tls-probe-transport
            :on-write
            (lambda (transport octets)
              (push (copy-seq octets) writes)
              (cond
                ((equalp octets (encode-ssl-request))
                 (memory-transport-append-input
                  transport
                  (octets (char-code #\S))))
                (plain-attempt-p
                 (setf plain-attempt-p nil)
                 (memory-transport-append-input transport ssl-required-error))
                (t
                 (append-ready-command transport "STARTUP"))))))
         (connection (make-connection :user "alice"
                                      :host "127.0.0.1"
                                      :transport transport
                                      :ssl-mode :allow)))
    (transport-open transport)
    (unwind-protect
         (progn
           (connect connection)
           (let ((ordered (reverse writes)))
             (is (= 3 (length ordered)))
             (is (not (equalp (first ordered) (encode-ssl-request))))
             (is (equalp (second ordered) (encode-ssl-request)))
             (is (not (equalp (third ordered) (encode-ssl-request)))))
           (is (equal '(:hostname "127.0.0.1" :verify nil)
                      (tls-probe-options transport)))
           (is (connection-open connection)))
      (disconnect connection))))

(deftest sslmode-does-not-retry-unrelated-startup-error
  (let* ((writes nil)
         (startup-error
           (make-frame
            #\E
            (join-octets (octets (char-code #\S))
                         (cstring "ERROR")
                         (octets (char-code #\C))
                         (cstring "28P01")
                         (octets (char-code #\M))
                         (cstring "password authentication failed")
                         (octets 0))))
         (transport
           (make-instance
            'tls-probe-transport
            :on-write
            (lambda (transport octets)
              (push (copy-seq octets) writes)
              (unless (equalp octets (encode-ssl-request))
                (memory-transport-append-input transport startup-error)))))
         (connection (make-connection :user "alice"
                                      :host "127.0.0.1"
                                      :transport transport
                                      :ssl-mode :allow)))
    (transport-open transport)
    (unwind-protect
         (let ((condition
                 (handler-case
                     (connect connection)
                   (server-error (condition)
                     condition))))
           (is (typep condition 'server-error))
           (is (= 1 (length writes)))
           (is (not (connection-open connection)))
           (is (eq (connection-state connection) :failed)))
      (disconnect connection))))

(deftest connection-candidates-fail-over-transport-errors
  (let* ((attempted nil)
         (connection
          (make-connection
           :user "alice"
           :hosts '("first" "second")
           :ports '(5432 5433)
           :transport-factory
           (lambda (connection endpoint)
             (declare (ignore connection))
             (push (list (getf endpoint :host)
                         (getf endpoint :hostaddr)
                         (getf endpoint :port))
                   attempted)
             (if (string= (getf endpoint :host) "first")
                 (make-memory-transport)
                 (make-memory-transport
                  :on-write (lambda (transport octets)
                              (declare (ignore octets))
                              (append-ready-command transport "STARTUP"))))))))
    (unwind-protect
         (progn
           (connect connection)
           (is (equal '(("first" "first" 5432)
                        ("second" "second" 5433))
                       (reverse attempted)))
           (is (string= "second" (connection-host connection)))
           (is (string= "second" (connection-hostaddr connection)))
           (is (= 5433 (connection-port connection)))
           (is (connection-open connection)))
      (disconnect connection))))

(deftest connection-endpoint-lists-preserve-correspondence
  (let ((connection
          (make-connection
           :user "alice"
           :hosts '("logical-first" "logical-second")
           :hostaddrs '("10.0.0.1" "10.0.0.2")
           :ports '(5432)
           :transport-factory
           (lambda (connection endpoint)
             (declare (ignore connection endpoint))
             (make-memory-transport)))))
    (unwind-protect
         (is (equal '((:host "logical-first" :hostaddr "10.0.0.1" :port 5432)
                      (:host "logical-second" :hostaddr "10.0.0.2" :port 5432))
                    (cl-postgresql-kit::connection--endpoints connection)))
      (disconnect connection)))
  (let ((connection
          (make-connection
           :user "alice"
           :hostaddrs '("10.0.0.1" "10.0.0.2")
           :ports '(5432)
           :transport-factory
           (lambda (connection endpoint)
             (declare (ignore connection endpoint))
             (make-memory-transport)))))
    (unwind-protect
         (is (equal '((:host "10.0.0.1" :hostaddr "10.0.0.1" :port 5432)
                      (:host "10.0.0.2" :hostaddr "10.0.0.2" :port 5432))
                    (cl-postgresql-kit::connection--endpoints connection)))
      (disconnect connection))))

(deftest connection-endpoint-lists-reject-cardinality-mismatches
  (dolist (arguments
            '((:hosts ("first" "second") :hostaddrs ("10.0.0.1"))
              (:hosts ("first" "second")
               :hostaddrs ("10.0.0.1" "10.0.0.2" "10.0.0.3"))
              (:hosts ("first" "second") :ports (5432 5433 5434))
              (:hosts ("first") :ports (5432 5433))))
    (assert-signals 'parameter-error
                    (lambda ()
                      (apply #'make-connection arguments)))))

(deftest load-balance-hosts-randomizes-candidates
  (let ((connection
          (make-connection
           :user "alice"
           :hosts '("first" "second" "third")
           :load-balance-hosts :random)))
    (unwind-protect
         (let ((orders
                 (loop repeat 32
                       collect
                       (cl-postgresql-kit::%connection-candidate-indices
                        connection))))
           (is (eq :random (connection-load-balance-hosts connection)))
           (dolist (order orders)
             (is (equal '(0 1 2) (sort (copy-list order) #'<))))
           (is (some (lambda (order)
                       (not (equal '(0 1 2) order)))
                     orders)))
      (disconnect connection))))

(deftest connection-candidates-do-not-fail-over-server-errors
  (let* ((attempted nil)
         (startup-error
           (make-frame
            #\E
            (join-octets (octets (char-code #\S))
                         (cstring "ERROR")
                         (octets (char-code #\C))
                         (cstring "28P01")
                         (octets (char-code #\M))
                         (cstring "password authentication failed")
                         (octets 0))))
         (connection
           (make-connection
            :user "alice"
            :hosts '("first" "second")
            :transport-factory
            (lambda (connection endpoint)
              (declare (ignore connection))
              (push (getf endpoint :host) attempted)
              (make-memory-transport
               :on-write (lambda (transport octets)
                           (declare (ignore octets))
                           (memory-transport-append-input
                            transport startup-error)))))))
    (unwind-protect
         (let ((condition
                 (handler-case
                     (connect connection)
                   (server-error (condition)
                     condition))))
           (is (typep condition 'server-error))
           (is (equal '("first") (reverse attempted)))
           (is (not (connection-open connection)))
           (is (eq (connection-state connection) :failed)))
      (disconnect connection))))

(deftest target-session-attributes-select-candidates
  (dolist (spec '((:read-only ("standby" "primary") "standby"
                   ("standby"))
                  (:standby ("standby" "primary") "standby"
                   ("standby"))
                  (:read-write ("standby" "primary") "primary"
                   ("standby" "primary"))
                  (:primary ("standby" "primary") "primary"
                   ("standby" "primary"))
                  (:prefer-standby ("primary") "primary"
                   ("primary" "primary"))))
    (destructuring-bind (target hosts expected-host expected-attempts) spec
      (let* ((attempted nil)
             (connection
              (make-connection
               :user "alice"
               :hosts hosts
               :target-session-attrs target
               :transport-factory
               (lambda (connection endpoint)
                 (declare (ignore connection))
                 (push (getf endpoint :host) attempted)
                 (make-target-session-transport
                  (string= (getf endpoint :host) "standby"))))))
        (unwind-protect
             (progn
               (connect connection)
               (is (equal expected-attempts (reverse attempted)))
               (is (string= expected-host (connection-host connection)))
               (is (connection-open connection)))
          (disconnect connection))))))

(deftest target-session-attributes-report-no-match
  (let ((connection
          (make-connection
           :user "alice"
           :hosts '("standby-a" "standby-b")
           :target-session-attrs :read-write
           :transport-factory
           (lambda (connection endpoint)
             (declare (ignore connection endpoint))
             (make-target-session-transport t)))))
    (unwind-protect
         (progn
           (assert-signals 'connection-error
                          (lambda () (connect connection)))
           (is (not (connection-open connection)))
           (is (eq (connection-state connection) :failed)))
      (disconnect connection))))

(deftest transaction-isolation-is-whitelisted
  (let ((connection (ready-memory-connection)))
    (unwind-protect
         (progn
           (is (string= (cl-postgresql-kit::%transaction-isolation-sql
                         :repeatable-read)
                        "REPEATABLE READ"))
           (assert-signals 'parameter-error
                          (lambda ()
                            (begin-transaction
                             connection
                             :isolation "SERIALIZABLE; DROP TABLE accounts"))))
      (disconnect connection))))

(deftest with-transaction-is-hygienic-and-evaluates-connection-once
  (let ((committed :caller-binding)
        (connection-evaluations 0)
        (events nil))
    (flet ((begin-transaction (connection &key isolation read-only deferrable)
             (declare (ignore connection isolation read-only deferrable))
             (push :begin events))
           (commit-transaction (connection)
             (declare (ignore connection))
             (push :commit events))
           (rollback-transaction (connection)
             (declare (ignore connection))
             (push :rollback events)))
      (multiple-value-bind (value marker)
          (with-transaction ((progn
                              (incf connection-evaluations)
                              :connection))
            (push committed events)
            (values :value :marker))
        (is (eql connection-evaluations 1))
        (is (equal events '(:commit :caller-binding :begin)))
        (is (eql value :value))
        (is (eql marker :marker))))))

(deftest with-transaction-rolls-back-when-the-body-signals
  (let ((events nil))
    (flet ((begin-transaction (connection &key isolation read-only deferrable)
             (declare (ignore connection isolation read-only deferrable))
             (push :begin events))
           (commit-transaction (connection)
             (declare (ignore connection))
             (push :commit events))
           (rollback-transaction (connection)
             (declare (ignore connection))
             (push :rollback events)))
      (assert-signals 'simple-error
                      (lambda ()
                        (with-transaction (:connection)
                          (push :body events)
                          (error "transaction body failed"))))
      (is (equal events '(:rollback :body :begin))))))

(deftest scram-attributes-are-strict
  (assert-signals 'authentication-error
                 (lambda ()
                   (cl-postgresql-kit::%scram-attributes "r=one,r=two")))
  (assert-signals 'authentication-error
                 (lambda ()
                   (cl-postgresql-kit::%scram-attributes
                    "r=one,x=two"
                    :allowed-names '("r"))))
  (assert-signals 'authentication-error
                 (lambda ()
                   (cl-postgresql-kit::%scram-attributes "r="))))

(deftest scram-crypto-boundaries
  (is (cl-postgresql-kit::%constant-time-string= "same" "same"))
  (is (not (cl-postgresql-kit::%constant-time-string= "same" "different")))
  (is (not (cl-postgresql-kit::%constant-time-string= "same" "short")))
  (is (cl-postgresql-kit::%constant-time-octets=
       (octets 1 2)
       (octets 1 2)))
  (is (not (cl-postgresql-kit::%constant-time-octets=
            (octets 1 2)
            (octets 1 3))))
  (is (not (cl-postgresql-kit::%constant-time-octets=
            (octets 1 2)
            (octets 1))))
  (is (string= "alice=3Dadmin=2Cops"
               (cl-postgresql-kit::%scram-username "alice=admin,ops")))
  (let ((attributes (cl-postgresql-kit::%scram-attributes
                     "r=one,s=two"
                     :allowed-names '("r" "s"))))
    (is (string= "one"
                 (cl-postgresql-kit::%scram-attribute attributes "r")))
    (is (string= "two"
                 (cl-postgresql-kit::%scram-attribute attributes "s"))))
  (is (equalp (octets 3 1)
              (cl-postgresql-kit::%xor-octet-vectors
               (octets 1 2)
               (octets 2 3))))
  (assert-signals 'protocol-error
                 (lambda ()
                   (cl-postgresql-kit::%xor-octet-vectors
                    (octets 1)
                    (octets 1 2)))))

(deftest connection-secrets-clear-on-disconnect
  (let ((connection (ready-memory-connection :password "secret")))
    (setf (connection-backend-process-id connection) 42
          (connection-backend-secret-key connection) 99)
    (disconnect connection)
    (is (null (connection-password connection)))
    (is (null (connection-backend-process-id connection)))
    (is (null (connection-backend-secret-key connection)))
    (is (null (connection-tls-established-p connection)))))

(deftest startup-parameters-are-sent
  (let* ((input (make-frame #\Z (octets (char-code #\I))))
         (transport (make-memory-transport :input input))
         (parameters '( ("search_path" . "public")
                        ("TimeZone" . "UTC")
                        ("options" . "-c statement_timeout=1000")))
         (connection (make-connection
                      :user "alice"
                      :database "app"
                      :application-name "kit"
                      :startup-parameters parameters
                      :transport transport
                      :ssl-mode :disable)))
    (unwind-protect
         (progn
           (connect connection)
           (is (connection-open connection))
           (is (equalp parameters (connection-startup-parameters connection)))
           (is (equalp
                (memory-transport-output transport)
                (encode-startup-message
                 :parameters
                 (append '(("user" . "alice")
                           ("database" . "app")
                           ("application_name" . "kit"))
                         parameters)))))
      (disconnect connection)))
  (assert-signals 'parameter-error
                 (lambda ()
                   (make-connection
                    :startup-parameters '(("user" . "override")))))
  (assert-signals 'parameter-error
                 (lambda ()
                   (make-connection
                    :startup-parameters '(("search_path" . 42))))))

(deftest startup-parameters-reject-improper-lists
  (assert-signals 'parameter-error
                  (lambda ()
                    (make-connection
                     :startup-parameters
                     '(("search_path" . "public") . "tail")))))

(deftest startup-protocol-version-is-configurable
  (let* ((input (make-frame #\Z (octets (char-code #\I))))
         (transport (make-memory-transport :input input))
         (connection (make-connection
                      :user "alice"
                      :transport transport
                      :ssl-mode :disable
                      :protocol-version +protocol-version-3.2+)))
    (unwind-protect
         (progn
           (connect connection)
           (is (= +protocol-version-3.2+
                  (connection-protocol-version connection)))
           (is (equalp
                (memory-transport-output transport)
                (encode-startup-message
                 :protocol-version +protocol-version-3.2+
                 :parameters '(("user" . "alice")
                               ("application_name" . "cl-postgresql-kit"))))))
      (disconnect connection))))

(deftest connection-string-parsing
  (let ((options
          (parse-connection-string
           "host=first.example host=database.example port=5433 user=alice password='s e\\'cret' dbname=app sslmode=verify-ca connect_timeout=5 application_name='my app' options='-c statement_timeout=1000' replication=database")))
    (is (string= "database.example" (getf options :host)))
    (is (= 5433 (getf options :port)))
    (is (string= "alice" (getf options :user)))
    (is (string= "s e'cret" (getf options :password)))
    (is (string= "app" (getf options :database)))
    (is (string= "my app" (getf options :application-name)))
    (is (eq :verify-ca (getf options :ssl-mode)))
    (is (= 5 (getf options :connect-timeout)))
    (is (equal '( ("options" . "-c statement_timeout=1000")
                  ("replication" . "database"))
               (getf options :startup-parameters)))
    (is (eq :random
            (getf (parse-connection-string "load_balance_hosts=random")
                  :load-balance-hosts))))
  (let* ((transport (make-memory-transport))
         (connection
           (make-connection-from-string
            "host=database.example port=5433 user=alice dbname=app"
            :transport transport
            :port 5544
            :application-name "override")))
    (is (string= "database.example" (connection-host connection)))
    (is (= 5544 (connection-port connection)))
    (is (string= "override" (connection-application-name connection)))
    (is (eq transport (connection-transport connection))))
  (is (equal '(:database "second")
             (parse-connection-string "database=first dbname=second")))
  (is (equal '(:database "second")
             (parse-connection-string "dbname=first database=second")))
  (let ((options
          (parse-connection-string
           "postgresql://localhost/path?database=query")))
    (is (string= "localhost" (getf options :host)))
    (is (string= "query" (getf options :database)))))

(deftest connection-uri-parsing
  (let ((options
          (parse-connection-uri
           "postgresql://alice:secret%20word@[::1]:5433/app%20db?sslmode=verify-full&application_name=my%20app&application_name=last")))
    (is (string= "::1" (getf options :host)))
    (is (= 5433 (getf options :port)))
    (is (string= "alice" (getf options :user)))
    (is (string= "secret word" (getf options :password)))
    (is (string= "app db" (getf options :database)))
    (is (string= "last" (getf options :application-name)))
    (is (eq :verify-full (getf options :ssl-mode))))
  (let ((connection
          (make-connection-from-uri
           "postgres://alice@localhost/app"
           :password "override"
           :ssl-mode :disable)))
    (is (string= "localhost" (connection-host connection)))
    (is (string= "alice" (connection-user connection)))
    (is (string= "override" (connection-password connection)))
    (is (eq :disable (connection-ssl-mode connection)))))

(deftest connection-parsing-validation
  (let ((options
          (parse-connection-uri
           "postgresql://localhost/app?channel_binding=require")))
    (is (eq :require (getf options :channel-binding))))
  (is (eq :prefer
          (connection-channel-binding (make-connection))))
  (assert-signals 'parameter-error
                 (lambda ()
                   (parse-connection-uri
                    "postgresql://localhost/app?channel_binding=invalid")))
  (assert-signals 'parameter-error
                 (lambda ()
                   (parse-connection-uri "postgresql://localhost:0/app")))
  (assert-signals 'parameter-error
                 (lambda ()
                   (parse-connection-string "user='unterminated")))
  (assert-signals 'parameter-error
                 (lambda ()
                   (parse-connection-uri "postgresql://localhost/app?bad=%ZZ"))))

(deftest connection-parser-edge-cases
  (dolist (connection-string
           (list
            (concatenate 'string "user='abc" (string (code-char 92)))
            "user='abc'x"
            (concatenate 'string "user=abc" (string (code-char 92)))
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
            "mysql://localhost/db"))
    (assert-signals 'parameter-error
                   (lambda ()
                     (parse-connection-string connection-string))))
  (assert-signals 'unsupported-feature
                 (lambda ()
                   (parse-connection-string "service=database")))
  (let ((options
          (parse-connection-uri
           "postgresql://first.example:5433,second.example/app")))
    (is (equal '("first.example" "second.example")
               (getf options :hosts)))
    (is (equal '(5433 5432) (getf options :ports))))
  (assert-signals 'parameter-error
                 (lambda ()
                   (make-connection-from-string "" :host)))
  (assert-signals 'parameter-error
                 (lambda ()
                   (make-connection-from-string "" "host" "localhost"))))

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

(deftest transport-base-boundaries-and-tls-option-validation
  (let ((transport (make-instance 'transport)))
    (is (not (transport-alive-p transport)))
    (is (eq transport (transport-open transport)))
    (is (transport-alive-p transport))
    (is (zerop (length (transport-read-available transport))))
    (is (not (transport-wait-readable transport)))
    (is (null (transport-channel-binding-data transport)))
    (is (transport-flush transport))
    (assert-signals 'unsupported-feature
                   (lambda ()
                     (transport-start-tls transport
                                           :hostname "db.example"
                                           :verify t)))
    (is (eq transport (transport-close transport)))
    (is (not (transport-alive-p transport))))
  (let ((transport
          (make-socket-transport
           :host "db.example"
           :port 5433
           :timeout 5
           :tls-options '(:certificate #p"client.crt"
                          :key #p"client.key"
                          :password nil
                          :alpn-protocols ("postgresql")
                          :cipher-list "HIGH"
                          :method :tlsv1.3
                          :verify-location #p"ca.crt"))))
    (is (equal '(:certificate "client.crt"
                 :key "client.key"
                 :password nil
                 :alpn-protocols ("postgresql")
                 :cipher-list "HIGH"
                 :method :tlsv1.3
                 :verify-location "ca.crt")
               (socket-transport-tls-options transport))))
  (dolist (options
            (list '(:alpn-protocols ("postgresql" 1))
                  '(:certificate 42)
                  '(:password 42)
                  '(:verify-location 42)
                  '(:verify-location ("ca.crt" 1))
                  '(:unsupported "x")
                  '(:certificate "a" :certificate "b")
                  '(:certificate)
                  :not-a-plist
                  '(:certificate "a" :key)))
    (assert-signals 'parameter-error
                   (lambda ()
                     (cl-postgresql-kit::%normalize-tls-options options))))
  (let ((options (list :certificate "a")))
    (setf (cdr (last options)) :tail)
    (assert-signals 'parameter-error
                   (lambda ()
                     (cl-postgresql-kit::%normalize-tls-options options))))
  (let ((locations (list "ca.crt")))
    (setf (cdr (last locations)) "tail")
    (assert-signals 'parameter-error
                   (lambda ()
                     (cl-postgresql-kit::%normalize-tls-verify-location
                      locations))))
  (let ((protocols (list "postgresql")))
    (setf (cdr (last protocols)) 1)
    (assert-signals 'parameter-error
                   (lambda ()
                     (cl-postgresql-kit::%normalize-tls-option-value
                      :alpn-protocols protocols))))
  (let ((options (list :certificate "a")))
    (setf (cdr (last options)) options)
    (assert-signals 'parameter-error
                   (lambda ()
                     (cl-postgresql-kit::%normalize-tls-options options)))))

#+sbcl
(deftest socket-transport-requires-open-stream
  (let ((transport (make-socket-transport)))
    (assert-signals 'transport-error
                   (lambda ()
                     (transport-read-exactly transport 1)))
    (assert-signals 'transport-error
                   (lambda ()
                     (transport-read-available transport)))
    (assert-signals 'transport-error
                   (lambda ()
                     (transport-wait-readable transport 0)))
    (assert-signals 'transport-error
                   (lambda ()
                     (transport-write-all transport (octets 1))))
    (is (transport-flush transport))
    (is (not (transport-alive-p transport)))))

(deftest channel-binding-policy
  (let ((connection
          (make-connection
           :channel-binding :require
           :transport (make-memory-transport))))
    (unwind-protect
         (assert-signals 'authentication-error
                        (lambda ()
                          (cl-postgresql-kit::%handle-authentication
                           connection
                           '(:type :sasl
                             :mechanisms ("SCRAM-SHA-256")))))
      (disconnect connection)))
  (let* ((transport (make-memory-transport))
         (connection
           (make-connection
            :channel-binding :disable
            :transport transport)))
    (transport-open transport)
    (setf (connection-open connection) t
          (connection-state connection) :ready
          (connection-tls-established-p connection) t)
    (unwind-protect
         (assert-signals 'authentication-error
                        (lambda ()
                          (cl-postgresql-kit::%handle-authentication
                           connection
                           '(:type :sasl
                             :mechanisms ("SCRAM-SHA-256-PLUS")))))
      (disconnect connection))))

(deftest scram-input-limits-are-enforced
  (let ((*maximum-scram-iterations* 2))
    (assert-signals 'parameter-error
                   (lambda () (scram-hi "password" "salt" 3))))
  (assert-signals 'authentication-error
                 (lambda ()
                   (cl-postgresql-kit::%scram-attributes
                    (make-string (1+ cl-postgresql-kit::*maximum-scram-message-length*)
                                 :initial-element #\x))))
  (assert-signals 'authentication-error
                 (lambda ()
                   (cl-postgresql-kit::%scram-attributes
                    (format nil "~{~A~^,~}"
                            (loop for index below
                                      (1+ cl-postgresql-kit::*maximum-scram-attribute-count*)
                                  collect (format nil "x~D=v" index))))))
  (assert-signals 'authentication-error
                 (lambda ()
                   (cl-postgresql-kit::%scram-attributes
                    (format nil "r=~A"
                            (make-string (1+ cl-postgresql-kit::*maximum-scram-attribute-length*)
                                         :initial-element #\x))))))

(deftest failed-exchange-retires-connection
  (let* ((input (make-frame #\D (octets 0 0 255)))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (progn
           (assert-signals 'protocol-error
                          (lambda () (query connection "select 1")))
           (is (not (connection-open connection)))
           (is (eq (connection-state connection) :failed)))
      (disconnect connection))))

(deftest unknown-backend-message-retires-connection
  (let ((connection (ready-memory-connection
                     :input (make-frame #\? #()))))
    (unwind-protect
         (progn
           (assert-signals 'protocol-error
                          (lambda () (query connection "select 1")))
           (is (not (connection-open connection)))
           (is (eq (connection-state connection) :failed)))
      (disconnect connection))))

(deftest wrong-copy-response-retires-connection
  (let ((connection (ready-memory-connection
                     :input (make-frame #\H (copy-response-payload)))))
    (unwind-protect
         (progn
           (assert-signals 'copy-error
                          (lambda ()
                            (copy-in connection "copy target from stdin")))
           (is (not (connection-open connection)))
           (is (eq (connection-state connection) :failed)))
      (disconnect connection))))

(deftest memory-transport-rejects-closed-write
  (let ((transport (make-memory-transport)))
    (transport-open transport)
    (transport-close transport)
    (assert-signals 'transport-error
                   (lambda ()
                     (transport-write-all transport (octets 1))))))

(deftest memory-transport-read-write-boundaries
  (let ((transport (make-memory-transport)))
    (assert-signals 'transport-error
                   (lambda () (transport-read-available transport)))
    (assert-signals 'transport-error
                   (lambda () (transport-wait-readable transport)))
    (transport-open transport)
    (is (zerop (length (transport-read-available transport))))
    (is (not (transport-wait-readable transport)))
    (assert-signals 'parameter-error
                   (lambda () (transport-read-exactly transport -1)))
    (assert-signals 'transport-error
                   (lambda () (transport-read-exactly transport 1)))
    (memory-transport-append-input transport (octets 7 8))
    (is (transport-wait-readable transport))
    (is (equalp (octets 7) (transport-read-exactly transport 1)))
    (is (equalp (octets 8) (transport-read-available transport)))
    (is (not (transport-wait-readable transport)))
    (is (equalp (octets 9 10)
                (transport-write-all transport (octets 9 10))))
    (is (transport-flush transport))
    (is (equalp (octets 9 10) (memory-transport-output transport)))
    (transport-close transport)
    (is (not (transport-alive-p transport)))
    (assert-signals 'transport-error
                   (lambda () (transport-flush transport)))))

(deftest auth-protocol-buffer-and-handler-boundaries
  (let* ((frame (make-frame #\Z (octets (char-code #\I))))
         (split 4)
         (connection (ready-memory-connection
                      :input (subseq frame 0 split)
                      :max-notifications 0))
         (transport (connection-transport connection))
         (notices nil)
         (notifications nil)
         (notification-payload
           (join-octets (octets 0 0 0 1)
                        (cstring "events")
                        (cstring "created"))))
    (unwind-protect
         (progn
           (setf (connection-notice-handler connection)
                 (lambda (ignored notice)
                   (declare (ignore ignored))
                   (push notice notices)))
           (setf (connection-notification-handler connection)
                 (lambda (ignored process-id channel payload)
                   (declare (ignore ignored))
                   (push (list process-id channel payload)
                         notifications)))
           (is (= split
                  (cl-postgresql-kit::%connection-fill-read-buffer connection)))
           (is (null
                (cl-postgresql-kit::%connection-buffered-backend-message
                 connection)))
           (memory-transport-append-input transport (subseq frame split))
           (is (= (- (length frame) split)
                  (cl-postgresql-kit::%connection-fill-read-buffer connection)))
           (let ((message
                   (cl-postgresql-kit::%connection-buffered-backend-message
                    connection)))
             (is (= (char-code #\Z) (backend-message-type message)))
             (is (equalp (octets (char-code #\I))
                         (backend-message-payload message))))
           (is (null
                (cl-postgresql-kit::%connection-buffered-backend-message
                 connection)))
           (cl-postgresql-kit::%handle-notice-fields
            connection '((:message . "notice")))
           (cl-postgresql-kit::%handle-notification
            connection notification-payload)
           (is (= 1 (length notices)))
           (is (= 1 (length notifications)))
           (is (string= "notice"
                        (cdr (assoc :message
                                    (notice-fields (first notices))))))
           (is (equal '((1 "events" "created")) notifications))
           (is (null (connection-notifications connection)))
           (is (null
                (cl-postgresql-kit::%send-frontend-message
                 connection (make-frame #\Q #()) :flush-p nil)))
           (is (equalp (make-frame #\Q #())
                       (memory-transport-output transport)))
           (setf (connection-state connection) :closed)
           (assert-signals 'connection-error
                          (lambda ()
                            (cl-postgresql-kit::%send-frontend-message
                             connection (make-frame #\Q #())))))
      (disconnect connection))))

(deftest auth-protocol-rejects-invalid-buffered-frames
  (let ((connection
          (ready-memory-connection
           :input (octets (char-code #\Z) 0 0 0 3))))
    (unwind-protect
         (progn
           (cl-postgresql-kit::%connection-fill-read-buffer connection)
           (assert-signals 'protocol-error
                          (lambda ()
                            (cl-postgresql-kit::%connection-buffered-backend-message
                             connection))))
      (disconnect connection)))
  (let ((*maximum-frame-size* 4)
        (connection
          (ready-memory-connection
           :input (octets (char-code #\Z) 0 0 0 5 0))))
    (unwind-protect
         (progn
           (cl-postgresql-kit::%connection-fill-read-buffer connection)
           (assert-signals 'protocol-error
                          (lambda ()
                            (cl-postgresql-kit::%connection-buffered-backend-message
                             connection))))
      (disconnect connection))))

(deftest wire-and-transport-boundaries
  (let ((builder (make-octet-builder)))
    (assert-signals 'parameter-error (lambda () (append-u8 builder 256)))
    (assert-signals 'parameter-error (lambda () (append-i16 builder 32768)))
    (assert-signals 'parameter-error (lambda () (make-frame 256 #())))
    (assert-signals 'parameter-error
                   (lambda () (append-cstring builder (format nil "a~Cb" #\Null)))))
  (assert-signals 'parameter-error
                 (lambda () (encode-copy-data-message '(256))))
  (assert-signals 'parameter-error
                 (lambda () (encode-copy-data-message :invalid)))
  (let ((transport (make-memory-transport)))
    (assert-signals 'parameter-error
                   (lambda () (transport-read-exactly transport -1))))
  (let ((*maximum-frame-size* 16))
    (assert-signals 'parameter-error
                   (lambda ()
                     (encode-startup-message
                      :user "user"
                      :parameters (list (cons "large" (make-string 20
                                                                      :initial-element #\x)))))))
  (let* ((message (parse-frame (encode-bind-message (list nil))))
         (expected (octets 0 0 0 0 0 1 0 0 0 0 0 0)))
    (is (equalp expected (backend-message-payload message)))))

(deftest disconnect-clears-session-state
  (let ((connection (ready-memory-connection)))
    (setf (gethash "name" (connection-parameters connection)) "value"
          (gethash "statement"
                   (cl-postgresql-kit::connection--prepared-statements connection))
          t
          (connection-last-result connection) :sentinel
          (cl-postgresql-kit::connection--active-copy connection) :sentinel
          (cl-postgresql-kit::connection--pending-error connection) :sentinel)
    (disconnect connection)
    (is (= 0 (hash-table-count (connection-parameters connection))))
    (is (= 0
           (hash-table-count
            (cl-postgresql-kit::connection--prepared-statements connection))))
    (is (null (connection-last-result connection)))
    (is (null (cl-postgresql-kit::connection--active-copy connection)))
    (is (null (cl-postgresql-kit::connection--pending-error connection)))
    (is (eq (connection-transaction-status connection) :idle))))

(deftest notification-queue-snapshot-and-pop
  (let ((connection (ready-memory-connection)))
    (unwind-protect
         (progn
           (cl-postgresql-kit::%handle-notification
            connection
            (join-octets (octets 0 0 0 1)
                         (cstring "events")
                         (cstring "created")))
           (is (= 1 (length (connection-notifications connection))))
           (is (equalp (poll-notification connection)
                       (list :pid 1 :channel "events" :payload "created")))
           (is (null (poll-notification connection))))
      (disconnect connection))))

(deftest notification-queue-limit-keeps-newest
  (let ((connection (ready-memory-connection :max-notifications 2)))
    (unwind-protect
         (flet ((payload (pid)
                  (join-octets (octets 0 0 0 pid)
                               (cstring "events")
                               (cstring "created"))))
           (cl-postgresql-kit::%handle-notification connection (payload 1))
           (cl-postgresql-kit::%handle-notification connection (payload 2))
           (cl-postgresql-kit::%handle-notification connection (payload 3))
           (is (= 2 (connection-max-notifications connection)))
           (is (= 2 (length (connection-notifications connection))))
           (is (= 3 (getf (first (connection-notifications connection)) :pid)))
           (is (= 2 (getf (second (connection-notifications connection)) :pid)))
           (is (= 3 (getf (poll-notification connection) :pid)))
           (is (= 2 (getf (poll-notification connection) :pid)))
           (is (null (poll-notification connection))))
      (disconnect connection))))

(deftest notify-uses-parameterized-query
  (let ((writes nil)
        (payload "a\\b'payload")
        (connection nil))
    (setf connection
          (ready-memory-connection
           :on-write
           (lambda (transport octets)
             (push (copy-seq octets) writes)
             (when (and (> (length octets) 0)
                        (= (aref octets 0) (char-code #\S)))
               (memory-transport-append-input
                transport
                (join-octets
                 (make-frame #\1 #())
                 (make-frame #\2 #())
                 (make-frame #\T (octets 0 0))
                 (make-frame #\C (cstring "SELECT 1"))
                 (make-frame #\Z (octets (char-code #\I)))))))))
    (unwind-protect
         (progn
           (notify connection "events" payload)
           (let* ((ordered-writes (reverse writes))
                  (types (mapcar (lambda (wire)
                                   (code-char (aref wire 0)))
                                 ordered-writes))
                  (payload-octets
                    (cl-codec-kit:string-to-octets payload :encoding :utf-8)))
             (is (equal '(#\P #\B #\D #\E #\S) types))
             (is (not (search payload-octets (first ordered-writes))))
             (is (search payload-octets (second ordered-writes))))
           (assert-signals
            'parameter-error
            (lambda ()
              (notify connection "")))
           (assert-signals
            'parameter-error
            (lambda ()
              (notify connection
                      (format nil "bad~C" #\Null))))
           (assert-signals
            'type-error
            (lambda ()
              (notify connection "events" 1)))
           (assert-signals
            'parameter-error
            (lambda ()
              (notify connection "events"
                      (format nil "bad~C" #\Null)))))
      (disconnect connection))))

(deftest notification-poll-reads-transport
  (let* ((input
           (join-octets
            (make-frame #\A
                        (join-octets (octets 0 0 0 1)
                                     (cstring "events")
                                     (cstring "created")))
            (make-frame #\C (cstring "SELECT 1"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (progn
           (is (equalp (poll-notification connection)
                       (list :pid 1 :channel "events" :payload "created")))
           (is (null (poll-notification connection)))
           (let ((result (query connection "select 1")))
             (is (string= (result-command-tag result) "SELECT 1"))))
           (disconnect connection))))

(deftest notification-poll-preserves-partial-frame
  (let* ((input
           (make-frame #\A
                       (join-octets (octets 0 0 0 2)
                                    (cstring "events")
                                    (cstring "created"))))
         (split (floor (length input) 2))
         (connection (ready-memory-connection
                      :input (subseq input 0 split)))
         (transport (connection-transport connection)))
    (unwind-protect
         (progn
           (is (null (poll-notification connection)))
           (memory-transport-append-input transport (subseq input split))
           (is (equalp (poll-notification connection)
                       (list :pid 2 :channel "events" :payload "created"))))
      (disconnect connection))))

(deftest notification-wait-uses-transport-readiness
  (let* ((input
           (make-frame #\A
                       (join-octets (octets 0 0 0 3)
                                    (cstring "events")
                                    (cstring "created"))))
         (connection (ready-memory-connection :input input))
         (transport (connection-transport connection)))
    (unwind-protect
         (progn
           (is (transport-wait-readable transport 0))
           (is (equalp (wait-for-notification connection 0)
                       (list :pid 3 :channel "events" :payload "created")))
           (is (not (transport-wait-readable transport 0)))
           (is (null (wait-for-notification connection 0))))
      (disconnect connection))))

(deftest oauthbearer-authentication
  (let* ((input (make-frame #\R (octets 0 0 0 12)))
         (transport (make-memory-transport :input input))
         (connection (make-connection
                      :transport transport
                      :oauth-token-provider
                      (lambda (ignored)
                        (declare (ignore ignored))
                        "test-token"))))
    (transport-open transport)
    (setf (connection-open connection) t
          (connection-state connection) :ready
          (connection-tls-established-p connection) t)
    (unwind-protect
         (progn
           (cl-postgresql-kit::%authenticate-oauthbearer connection)
           (let ((expected
                   (format nil "n,a=,~Cauth=Bearer test-token~C~C"
                           (code-char 1) (code-char 1) (code-char 1))))
             (is (equalp
                  (memory-transport-output transport)
                  (encode-sasl-initial-response "OAUTHBEARER" expected)))))
      (disconnect connection))))

(deftest oauthbearer-authentication-boundaries
  (let ((connection (ready-memory-connection)))
    (unwind-protect
         (assert-signals 'authentication-error
                        (lambda ()
                          (cl-postgresql-kit::%authenticate-oauthbearer
                           connection)))
      (disconnect connection)))
  (let ((connection (ready-memory-connection :ssl-mode :verify-full)))
    (setf (connection-tls-established-p connection) t)
    (unwind-protect
         (assert-signals 'authentication-error
                        (lambda ()
                          (cl-postgresql-kit::%authenticate-oauthbearer
                           connection)))
      (disconnect connection)))
  (let ((connection
          (ready-memory-connection
           :ssl-mode :verify-full
           :oauth-token-provider
           (lambda (ignored)
             (declare (ignore ignored))
             42))))
    (setf (connection-tls-established-p connection) t)
    (unwind-protect
         (assert-signals 'authentication-error
                        (lambda ()
                          (cl-postgresql-kit::%authenticate-oauthbearer
                           connection)))
      (disconnect connection)))
  (labels ((make-oauth-connection (input)
             (let ((connection
                     (ready-memory-connection
                      :input input
                      :ssl-mode :verify-full
                      :oauth-token-provider
                      (lambda (ignored)
                        (declare (ignore ignored))
                        "test-token"))))
               (setf (connection-tls-established-p connection) t)
               connection))
           (assert-oauth-error (input)
             (let ((connection (make-oauth-connection input)))
               (unwind-protect
                    (assert-signals 'authentication-error
                                   (lambda ()
                                     (cl-postgresql-kit::%authenticate-oauthbearer
                                      connection)))
                 (disconnect connection)))))
    (let ((connection
            (make-oauth-connection
             (join-octets
              (make-frame #\R (octets 0 0 0 11))
              (make-frame #\R (octets 0 0 0 12))))))
      (unwind-protect
           (progn
             (cl-postgresql-kit::%authenticate-oauthbearer connection)
             (is (equalp
                  (memory-transport-output (connection-transport connection))
                  (encode-sasl-initial-response
                   "OAUTHBEARER"
                   (format nil "n,a=,~Cauth=Bearer test-token~C~C"
                           (code-char 1) (code-char 1) (code-char 1))))))
        (disconnect connection)))
    (assert-oauth-error
     (make-frame #\R
                 (join-octets (octets 0 0 0 11)
                              (cl-codec-kit:string-to-octets "challenge"))))
    (assert-oauth-error
     (make-frame #\R
                 (join-octets (octets 0 0 0 12)
                              (cl-codec-kit:string-to-octets "error"))))
    (assert-oauth-error
     (make-frame #\Z (octets (char-code #\I))))
    (assert-oauth-error
     (make-frame #\R (octets 0 0 0 0)))))

(deftest scram-plus-channel-binding-header
  (let* ((binding (octets 1 2 3 4))
         (input (make-frame #\R (octets 0 0 0 11)))
         (transport (make-memory-transport
                     :input input
                     :channel-binding-data binding))
         (connection (make-connection
                      :transport transport
                      :user "alice"
                      :password "secret")))
    (transport-open transport)
    (setf (connection-open connection) t
          (connection-state connection) :ready
          (connection-tls-established-p connection) t)
    (unwind-protect
         (progn
           (assert-signals 'authentication-error
                           (lambda ()
                             (cl-postgresql-kit::%handle-authentication
                              connection
                              '(:type :sasl
                                :mechanisms ("SCRAM-SHA-256-PLUS")))))
           (let ((output (memory-transport-output transport)))
             (is (not (null
                       (search (cl-codec-kit:string-to-octets
                                "SCRAM-SHA-256-PLUS")
                               output))))
             (is (not (null
                       (search (cl-codec-kit:string-to-octets
                                "p=tls-server-end-point,,")
                               output))))))
      (disconnect connection))))
