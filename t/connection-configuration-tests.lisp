(in-package #:cl-postgresql-kit/test)

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
  (is-each (arguments '((:hosts ("first" "second") :hostaddrs ("10.0.0.1"))
                        (:hosts ("first" "second")
                         :hostaddrs ("10.0.0.1" "10.0.0.2" "10.0.0.3"))
                        (:hosts ("first" "second") :ports (5432 5433 5434))
                        (:hosts ("first") :ports (5432 5433))))
    (assert-signals 'parameter-error
                    (lambda ()
                      (apply #'make-connection arguments)))))

(deftest connection-option-normalizers-accept-canonical-forms
  (is-each (case '((cl-postgresql-kit::%normalize-target-session-attrs
                    ((nil :any) (:any :any) ("any" :any)
                     ("read-write" :read-write) ("read-only" :read-only)
                     ("primary" :primary) ("standby" :standby)
                     ("prefer-standby" :prefer-standby)))
                   (cl-postgresql-kit::%normalize-load-balance-hosts
                    ((nil :disable) (:disable :disable) ("disable" :disable)
                     ("random" :random)))
                   (cl-postgresql-kit::%normalize-channel-binding
                    ((nil :prefer) (:disable :disable) ("disable" :disable)
                     (:prefer :prefer) ("prefer" :prefer)
                     (:require :require) ("require" :require)))))
    (destructuring-bind (normalizer cases) case
      (is-funcall-results normalizer cases eq)))
  (is-case-each
      ((normalizer input)
       '((cl-postgresql-kit::%normalize-target-session-attrs "invalid")
         (cl-postgresql-kit::%normalize-load-balance-hosts :invalid)
         (cl-postgresql-kit::%normalize-channel-binding 42)))
    (assert-funcall-signals-each 'parameter-error (list input) normalizer)))

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
           (is-each (order orders)
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
         (assert-condition-fails-connection
             (condition server-error connection (connect connection))
           (is (equal '("first") (reverse attempted))))
      (disconnect connection))))

(deftest target-session-attributes-select-candidates
  (is-case-each
      ((target hosts expected-host expected-attempts)
       '((:read-only ("standby" "primary") "standby"
          ("standby"))
         (:standby ("standby" "primary") "standby"
          ("standby"))
         (:read-write ("standby" "primary") "primary"
          ("standby" "primary"))
         (:primary ("standby" "primary") "primary"
          ("standby" "primary"))
         (:prefer-standby ("primary") "primary"
          ("primary" "primary"))))
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
        (disconnect connection)))))

(deftest-condition-retires-connection
    target-session-attributes-report-no-match
    connection-error
    (make-connection
     :user "alice"
     :hosts '("standby-a" "standby-b")
     :target-session-attrs :read-write
     :transport-factory
     (lambda (connection endpoint)
       (declare (ignore connection endpoint))
       (make-target-session-transport t)))
  (connect connection))

(deftest startup-parameters-are-sent
  (let* ((input (make-frame #\Z (octets (char-code #\I))))
         (transport (make-memory-transport :input input))
         (parameters '(("search_path" . "public")
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
  (it-signals-each 'parameter-error
      ((:reserved-name '(("user" . "override")))
       (:non-string-value '(("search_path" . 42))))
    "rejects invalid startup parameter case ~A"
    (label parameters)
    (declare (ignore label))
    (make-connection :startup-parameters parameters)))

(deftest startup-parameters-reject-improper-lists
  (assert-signals 'parameter-error
                  (lambda ()
                    (make-connection
                     :startup-parameters
                     '(("search_path" . "public") . "tail")))))

(deftest startup-parameters-reject-invalid-pairs
  (it-signals-each 'parameter-error
      ((:duplicate-key
        (("search_path" . "public")
         ("SEARCH_PATH" . "private")))
       (:empty-key
        (("" . "public")))
       (:nul-in-value
        ((:dynamic-value)))
       (:nul-in-key
        ((:dynamic-key))))
    "rejects invalid startup parameter pair case ~A"
    (label parameters)
    (make-connection
     :startup-parameters
     (case label
       (:dynamic-value
        (list (cons "search_path" (format nil "pub~Clic" #\Null))))
       (:dynamic-key
        (list (cons (format nil "search~Cpath" #\Null) "public")))
       (t parameters)))))

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
