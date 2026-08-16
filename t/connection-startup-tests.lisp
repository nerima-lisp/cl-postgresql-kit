(in-package #:cl-postgresql-kit/test)

(deftest cancel-request-writes-control-packet
  (let ((cancel-transport nil))
    (with-test-connection
        (connection
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
                  (encode-cancel-request 42 99))))))

(deftest protocol-negotiation-updates-connection-state
  (with-test-connection
      (connection (ready-memory-connection))
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
      (is (null (connection-negotiated-protocol-version connection))))))

(deftest ssl-verification-modes-map-to-transport
  (is-case-each
      ((mode expected-host expected-verify)
       '((:require "127.0.0.1" nil)
         (:verify-ca nil t)
         (:verify-full "127.0.0.1" t)))
    (with-recording-probe-transport
        (transport writes :input (octets (char-code #\S)))
      (declare (ignore writes))
      (with-open-test-connection
          (connection (make-connection :host "127.0.0.1"
                                       :transport transport
                                       :ssl-mode mode))
        (cl-postgresql-kit::%ssl-request connection)
        (let ((options (tls-probe-options transport)))
          (is (equal (getf options :hostname) expected-host))
          (is (eq (getf options :verify) expected-verify)))
        (is (connection-tls-established-p connection))))))

(deftest sslmode-startup-order-and-fallback
  (with-recording-probe-transport
      (transport writes
       :on-write (lambda (transport octets)
                   (if (equalp octets (encode-ssl-request))
                       (memory-transport-append-input
                        transport
                        (octets (char-code #\N)))
                       (append-ready-command transport "STARTUP"))))
    (with-open-test-connection
        (connection (make-connection :user "alice"
                                     :host "127.0.0.1"
                                     :transport transport
                                     :ssl-mode :prefer))
      (connect connection)
      (let ((ordered (reverse writes)))
        (is (= 2 (length ordered)))
        (is (equalp (first ordered) (encode-ssl-request)))
        (is (not (equalp (second ordered) (encode-ssl-request)))))
      (is (null (tls-probe-options transport)))
      (is (connection-open connection))))
  (with-recording-probe-transport
      (transport writes
       :on-write (lambda (transport octets)
                   (unless (equalp octets (encode-ssl-request))
                     (append-ready-command transport "STARTUP"))))
    (with-open-test-connection
        (connection (make-connection :user "alice"
                                     :host "127.0.0.1"
                                     :transport transport
                                     :ssl-mode :allow))
      (connect connection)
      (let ((ordered (reverse writes)))
        (is (= 1 (length ordered)))
        (is (not (equalp (first ordered) (encode-ssl-request)))))
      (is (null (tls-probe-options transport)))
      (is (connection-open connection))))
  (let ((ssl-required-error
          (make-frame
           #\E
           (join-octets (octets (char-code #\S))
                        (cstring "ERROR")
                        (octets (char-code #\C))
                        (cstring "08004")
                        (octets (char-code #\M))
                        (cstring "SSL required")
                        (octets 0))))
        (plain-attempt-p t))
    (with-recording-probe-transport
        (transport writes
         :on-write (lambda (transport octets)
                     (cond
                       ((equalp octets (encode-ssl-request))
                        (memory-transport-append-input
                         transport
                         (octets (char-code #\S))))
                       (plain-attempt-p
                        (setf plain-attempt-p nil)
                        (memory-transport-append-input
                         transport
                         ssl-required-error))
                       (t
                        (append-ready-command transport "STARTUP")))))
      (with-open-test-connection
          (connection (make-connection :user "alice"
                                       :host "127.0.0.1"
                                       :transport transport
                                       :ssl-mode :allow))
        (connect connection)
        (let ((ordered (reverse writes)))
          (is (= 3 (length ordered)))
          (is (not (equalp (first ordered) (encode-ssl-request))))
          (is (equalp (second ordered) (encode-ssl-request)))
          (is (not (equalp (third ordered) (encode-ssl-request)))))
        (is (equal '(:hostname "127.0.0.1" :verify nil)
                   (tls-probe-options transport)))
        (is (connection-open connection))))))

(deftest oauthbearer-discovery-reconnects-with-provider-token
  (let* ((discovery-response
           "{\"scope\":\"openid\"}")
         (discovery-error
           (make-frame
            #\E
            (join-octets (octets (char-code #\S))
                         (cstring "ERROR")
                         (octets (char-code #\C))
                         (cstring "28000")
                         (octets (char-code #\M))
                         (cstring "OAuth discovery failed")
                         (octets 0))))
         (oauth-offer
           (make-frame
            #\R
            (join-octets (octets 0 0 0 10)
                         (cstring "OAUTHBEARER")
                         (octets 0))))
         (oauth-continue
           (make-frame
            #\R
            (join-octets (octets 0 0 0 11)
                         (cl-codec-kit:string-to-octets discovery-response
                                                         :encoding :utf-8))))
         (oauth-final (make-frame #\R (octets 0 0 0 12)))
         (ready (make-frame #\Z (octets (char-code #\I))))
         (startup-count 0)
         (callback-response nil))
    (with-recording-probe-transport
        (transport writes
         :on-write
         (lambda (transport octets)
           (cond
             ((equalp octets (encode-ssl-request))
              (memory-transport-append-input
               transport
               (octets (char-code #\S))))
             ((and (plusp (length octets))
                   (zerop (aref octets 0)))
              (incf startup-count)
              (memory-transport-append-input
               transport
               (if (= startup-count 1)
                   (join-octets oauth-offer oauth-continue discovery-error)
                   (join-octets oauth-offer oauth-final ready)))))))
      (with-open-test-connection
          (connection
            (make-connection
             :user "alice"
             :transport transport
             :ssl-mode :require
             :oauth-token-provider nil
             :oauth-discovery-provider
             (lambda (ignored response)
               (declare (ignore ignored))
               (setf callback-response response)
               "discovered-token")))
        (connect connection)
        (is (= 2 startup-count))
        (is (string= discovery-response callback-response))
        (is (connection-open connection))
        (is (not (null
                  (search (cl-codec-kit:string-to-octets
                           "Bearer discovered-token")
                          (memory-transport-output transport)))))
        (is (= 7 (length writes)))))))

(defclass failing-tls-probe-transport (tls-probe-transport) ())

(defmethod transport-start-tls ((transport failing-tls-probe-transport)
                                &key hostname verify)
  (declare (ignore transport hostname verify))
  (error "TLS handshake failed."))

(deftest sslmode-prefer-does-not-downgrade-tls-failure
  (with-recording-probe-transport
      (transport writes
       :class 'failing-tls-probe-transport
       :on-write (lambda (transport octets)
                   (if (equalp octets (encode-ssl-request))
                       (memory-transport-append-input
                        transport
                        (octets (char-code #\S)))
                       (append-ready-command transport "STARTUP"))))
    (with-open-test-connection
        (connection (make-connection :user "alice"
                                     :host "127.0.0.1"
                                     :transport transport
                                     :ssl-mode :prefer))
      (assert-condition-fails-connection
          (condition connection-error connection (connect connection))
        (is (= 1 (length writes)))
        (is (equalp (first writes) (encode-ssl-request)))))))

(deftest sslmode-does-not-retry-unrelated-startup-error
  (let ((startup-error
          (make-frame
           #\E
           (join-octets (octets (char-code #\S))
                        (cstring "ERROR")
                        (octets (char-code #\C))
                        (cstring "28P01")
                        (octets (char-code #\M))
                        (cstring "password authentication failed")
                        (octets 0)))))
    (with-recording-probe-transport
        (transport writes
         :on-write (lambda (transport octets)
                     (unless (equalp octets (encode-ssl-request))
                       (memory-transport-append-input transport startup-error))))
      (with-open-test-connection
          (connection (make-connection :user "alice"
                                       :host "127.0.0.1"
                                       :transport transport
                                       :ssl-mode :allow))
        (assert-condition-fails-connection
            (condition server-error connection (connect connection))
          (is (= 1 (length writes))))))))

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
