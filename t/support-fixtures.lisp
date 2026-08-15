(in-package #:cl-postgresql-kit/test)

(defun ready-memory-connection (&key input on-write password
                                      oauth-token-provider
                                      gss-token-provider
                                      sspi-token-provider
                                      cancel-transport-factory
                                      (max-notifications 10000)
                                      query-timeout
                                      metric-registry
                                      (ssl-mode :disable))
  (let ((transport (make-memory-transport :input input :on-write on-write)))
    (transport-open transport)
    (let ((connection (make-connection :transport transport
                                       :ssl-mode ssl-mode
                                       :password password
                                       :oauth-token-provider
                                       oauth-token-provider
                                       :gss-token-provider
                                       gss-token-provider
                                       :sspi-token-provider
                                       sspi-token-provider
                                       :query-timeout query-timeout
                                       :metric-registry metric-registry
                                       :max-notifications max-notifications
                                       :cancel-transport-factory
                                       cancel-transport-factory)))
      (setf (connection-open connection) t
            (connection-state connection) :ready)
      connection)))

(defmacro with-ready-memory-connection ((&rest options) &body body)
  "Run BODY with a ready in-memory connection and always disconnect it.

OPTIONS are passed directly to READY-MEMORY-CONNECTION.  The macro owns only
the fixture lifecycle; assertions and protocol behavior remain in BODY."
  `(let ((connection (ready-memory-connection ,@options)))
     (unwind-protect
          (progn ,@body)
       (disconnect connection))))

(defmacro with-test-connection ((var connection-form) &body body)
  "Bind VAR to CONNECTION-FORM and always disconnect it when non-NIL."
  `(let ((,var ,connection-form))
     (unwind-protect
          (progn ,@body)
       (when ,var
         (disconnect ,var)))))

(defmacro with-ready-memory-test-connection ((var &rest options) &body body)
  "Bind VAR to a ready in-memory connection and always disconnect it.

OPTIONS are passed directly to READY-MEMORY-CONNECTION."
  `(with-test-connection (,var (ready-memory-connection ,@options))
     ,@body))

(defmacro with-test-transport-connection
    ((connection transport connection-form) &body body)
  "Bind CONNECTION and its TRANSPORT from CONNECTION-FORM for BODY."
  `(with-test-connection (,connection ,connection-form)
     (let ((,transport (connection-transport ,connection)))
       ,@body)))

(defmacro with-logical-replication-decoder
    ((var &rest make-decoder-options) &body body)
  "Bind VAR to a fresh logical replication decoder for BODY."
  `(let ((,var (make-logical-replication-decoder ,@make-decoder-options)))
     ,@body))

(defmacro with-registered-logical-replication-relation
    ((decoder relation-var &key relation-id columns
                              (message-kind :relation)
                              (message-options '()))
     &body body)
  "Bind RELATION-VAR to a registered logical replication relation message.

RELATION-ID and COLUMNS are evaluated once and used to construct the relation
message registered in DECODER.  MESSAGE-KIND and MESSAGE-OPTIONS allow tests
to override message details when needed."
  `(let* ((,relation-var
            (cl-postgresql-kit::%make-logical-replication-message
             :kind ,message-kind
             :relation-id ,relation-id
             :columns ,columns
             ,@message-options)))
     (register-logical-replication-relation ,decoder ,relation-var)
     ,@body))

(defmacro with-open-test-connection ((var connection-form) &body body)
  "Like WITH-TEST-CONNECTION, but opens the transport before running BODY."
  `(let ((,var ,connection-form))
     (transport-open (connection-transport ,var))
     (unwind-protect
          (progn ,@body)
       (when ,var
         (disconnect ,var)))))

(defun oauthbearer-test-token (ignored)
  (declare (ignore ignored))
  "test-token")

(defun oauthbearer-initial-response-string (&optional (token "test-token"))
  (format nil "n,a=,~Cauth=Bearer ~A~C~C"
          (code-char 1) token (code-char 1) (code-char 1)))

(defun make-oauthbearer-test-connection (&key input
                                              (ssl-mode :verify-full)
                                              tls-established-p
                                              (oauth-token-provider
                                                #'oauthbearer-test-token))
  (let ((connection
          (ready-memory-connection
           :input input
           :ssl-mode ssl-mode
           :oauth-token-provider oauth-token-provider)))
    (when tls-established-p
      (setf (connection-tls-established-p connection) t))
    connection))

(defmacro assert-oauthbearer-output (connection &optional (token "test-token"))
  `(is (equalp
        (memory-transport-output (connection-transport ,connection))
        (encode-sasl-initial-response
         "OAUTHBEARER"
         (oauthbearer-initial-response-string ,token)))))

(defmacro assert-connection-failed-state (connection)
  `(progn
     (is (not (connection-open ,connection)))
     (is (eq (connection-state ,connection) :failed))))

(defmacro assert-connection-ready-state (connection)
  `(progn
     (is (connection-open ,connection))
     (is (eq (connection-state ,connection) :ready))))

(defmacro deftest-condition-preserves-connection
    (name condition-type connection-form &body operation)
  "Define a test that expects CONDITION-TYPE and keeps the connection ready."
  `(deftest ,name
     (with-test-connection
         (connection ,connection-form)
       (assert-signals ',condition-type
                       (lambda ()
                         ,@operation))
       (assert-connection-ready-state connection))))

(defmacro deftest-condition-retires-connection
    (name condition-type connection-form &body operation)
  "Define a test that expects CONDITION-TYPE and retires the connection."
  `(deftest ,name
     (with-test-connection
         (connection ,connection-form)
       (assert-signals ',condition-type
                       (lambda ()
                         ,@operation))
       (assert-connection-failed-state connection))))

(defmacro deftest-query-error-retires-connection
    (name connection-form &body operation)
  "Define a test that expects QUERY-ERROR and a failed, closed connection."
  `(deftest-condition-retires-connection
       ,name query-error ,connection-form
     ,@operation))

(defmacro assert-condition-fails-connection
    ((condition-var condition-type connection operation) &body assertions)
  "Assert that OPERATION signals CONDITION-TYPE and leaves CONNECTION failed."
  `(let ((,condition-var
           (handler-case
               ,operation
             (,condition-type (,condition-var)
               ,condition-var))))
     (is (typep ,condition-var ',condition-type))
     ,@assertions
     (assert-connection-failed-state ,connection)))

(defclass tls-probe-transport (memory-transport)
  ((tls-options :accessor tls-probe-options :initform nil)))

(defmethod transport-start-tls ((transport tls-probe-transport)
                                &key hostname verify)
  (setf (tls-probe-options transport)
        (list :hostname hostname :verify verify))
  transport)

(defmacro with-recording-probe-transport
    ((transport writes &key
                       (class ''tls-probe-transport)
                       input
                       on-write)
     &body body)
  "Bind TRANSPORT to a probe transport that records every write into WRITES."
  `(let* ((,writes nil)
          (,transport
           (make-instance
            ,class
            ,@(when input
                `(:input ,input))
            :on-write
            (lambda (transport octets)
              (push (copy-seq octets) ,writes)
              ,(when on-write
                 `(funcall ,on-write transport octets))))))
     ,@body))
