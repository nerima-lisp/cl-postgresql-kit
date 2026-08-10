(in-package #:cl-postgresql-kit)

(defun %ssl-request (connection)
  (transport-write-all (connection-transport connection)
                       (encode-ssl-request))
  (transport-flush (connection-transport connection))
  (let ((response (transport-read-exactly (connection-transport connection) 1)))
    (case (code-char (aref response 0))
      (#\S
       (transport-start-tls (connection-transport connection)
                            :hostname (unless (eq (connection-ssl-mode connection)
                                                  :verify-ca)
                                       (connection-host connection))
                            :verify (not (null (member (connection-ssl-mode connection)
                                                       '(:verify-ca :verify-full)))))
       (setf (connection-tls-established-p connection) t))
      (#\N
       (setf (connection-tls-established-p connection) nil)
       (when (member (connection-ssl-mode connection)
                     '(:require :verify-ca :verify-full))
         (error 'tls-error :message "The server refused SSL negotiation.")))
      (otherwise
       (error 'protocol-error :message "Invalid SSL negotiation response.")))))

(defun %ssl-mode-requests-tls-p (mode)
  (member mode '(:prefer :require :verify-ca :verify-full)))

(defun %ssl-required-server-error-p (condition)
  (when (typep condition 'server-error)
    (let ((message (string-downcase (or (server-error-message condition) "")))
          (sqlstate (server-error-sqlstate condition)))
      (or (search "ssl required" message)
          (search "ssl connection is required" message)
          (search "ssl is required" message)
          (search "tls required" message)
          (search "tls connection is required" message)
          (search "must use ssl" message)
          (search "no encryption" message)
          (search "ssl off" message)
          (and (member sqlstate '("08004" "28000") :test #'string=)
               (or (search "ssl" message)
                   (search "tls" message)
                   (search "encrypt" message)))))))

(defun %startup-parameters (connection)
  (append
   (remove nil
           (list (cons "user" (connection-user connection))
                 (and (connection-database connection)
                      (cons "database" (connection-database connection)))
                 (cons "application_name" (connection-application-name connection))))
   (connection-startup-parameters connection)))

(defun %connection-transaction-read-only-p (connection)
  (labels ((parse-value (value)
             (cond ((and (stringp value) (string-equal value "on")) t)
                   ((and (stringp value) (string-equal value "off")) nil)
                   (t
                    (error 'protocol-error
                           :message
                           (format nil
                                   "Invalid transaction_read_only value ~S."
                                   value))))))
    (let ((parameter
            (gethash "transaction_read_only"
                     (connection-parameters connection))))
      (if parameter
          (parse-value parameter)
          (let* ((result (%run-exchange connection
                                        :sql "SHOW transaction_read_only"))
                 (rows (query-result-rows result)))
            (unless (= (length rows) 1)
              (error 'protocol-error
                     :message
                     "SHOW transaction_read_only returned an unexpected row count."))
            (let ((row (aref rows 0)))
              (unless (= (length row) 1)
                (error 'protocol-error
                       :message
                       "SHOW transaction_read_only returned an unexpected column count."))
              (parse-value (aref row 0))))))))

(defun %connection-candidate-indices (connection)
  (let ((indices
          (make-array
           (length (connection--endpoints connection))
           :initial-contents
           (loop for index below (length (connection--endpoints connection))
                 collect index))))
    (when (eq (connection-load-balance-hosts connection) :random)
      (loop for index from (1- (length indices)) downto 1
            for swap-index = (random (1+ index))
            do (rotatef (aref indices index)
                        (aref indices swap-index))))
    (loop for index across indices collect index)))

(defun %connect-implementation (connection)
  (when (connection-open connection)
    (return-from %connect-implementation connection))
  (unless (and (connection-user connection)
               (plusp (length (connection-user connection))))
    (error 'connection-error :message "A PostgreSQL user is required."))
  (%clear-connection-session-state connection)
  (setf (connection-state connection) :connecting
        (connection-tls-established-p connection) nil)
  (let ((attempt-requests-ssl-p nil)
        (ssl-negotiation-complete-p nil)
        (authentication-started-p nil)
        (last-transport-condition nil)
        (preferred-endpoint-index nil))
    (labels ((fail (condition)
               (ignore-errors (transport-close (connection-transport connection)))
               (%clear-connection-session-state connection)
               (%clear-connection-secrets connection)
               (setf (connection-open connection) nil
                     (connection-state connection) :failed
                     (connection--initial-transport-used-p connection) nil)
               (if (typep condition 'postgresql-condition)
                   (error condition)
                   (error 'connection-error
                          :message (format nil
                                           "PostgreSQL connection failed: ~A"
                                           condition)
                          :cause condition)))
             (reset-for-retry ()
               (ignore-errors (transport-close (connection-transport connection)))
               (%clear-connection-session-state connection)
               (setf (connection-backend-process-id connection) nil
                     (connection-backend-secret-key connection) nil
                     (connection-open connection) nil
                     (connection-state connection) :connecting
                     (connection-tls-established-p connection) nil))
             (attempt (request-ssl-p)
               (setf attempt-requests-ssl-p request-ssl-p
                     ssl-negotiation-complete-p (not request-ssl-p)
                     authentication-started-p nil
                     (connection-open connection) nil
                     (connection-state connection) :connecting
                     (connection-tls-established-p connection) nil)
               (transport-open (connection-transport connection))
               (when request-ssl-p
                 (%ssl-request connection)
                 (setf ssl-negotiation-complete-p t))
               (transport-write-all (connection-transport connection)
                                    (encode-startup-message
                                     :protocol-version
                                     (connection-protocol-version connection)
                                     :parameters (%startup-parameters connection)))
               (transport-flush (connection-transport connection))
               (loop
                 for message = (%read-backend-message connection)
                 do (let ((kind (backend-message-kind
                                 (backend-message-type message))))
                      (case kind
                        (:authentication
                         (setf authentication-started-p t)
                         (%handle-authentication
                          connection
                          (parse-authentication
                           (backend-message-payload message))))
                        (:error-response
                         (%signal-server-fields
                          (parse-error-response
                           (backend-message-payload message))))
                        (:ready-for-query
                         (%process-backend-message connection message)
                         (return))
                        (otherwise
                         (%process-backend-message connection message)))))
               (setf (connection-open connection) t
                     (connection-state connection) :ready)
               (%connection-log connection "PostgreSQL connection is ready"
                                :host (connection-host connection)
                                :port (connection-port connection))
               connection)
             (attempt-current-endpoint ()
               (handler-case
                   (attempt (%ssl-mode-requests-tls-p
                             (connection-ssl-mode connection)))
                 (error (condition)
                   (if (or (and (eq (connection-ssl-mode connection) :allow)
                                (not attempt-requests-ssl-p)
                                (not authentication-started-p)
                                (%ssl-required-server-error-p condition))
                           (and (eq (connection-ssl-mode connection) :prefer)
                                attempt-requests-ssl-p
                                (not ssl-negotiation-complete-p)
                                (not authentication-started-p)))
                       (progn
                         (reset-for-retry)
                         (attempt (eq (connection-ssl-mode connection) :allow)))
                       (error condition)))))
             (target-session-status ()
               (case (connection-target-session-attrs connection)
                 (:any :accept)
                 ((:read-write :primary)
                  (if (%connection-transaction-read-only-p connection)
                      :mismatch
                      :accept))
                 ((:read-only :standby)
                  (if (%connection-transaction-read-only-p connection)
                      :accept
                      :mismatch))
                 (:prefer-standby
                  (if (%connection-transaction-read-only-p connection)
                      :accept
                      :fallback)))))
      (loop for index in (%connection-candidate-indices connection)
            for endpoint = (nth index (connection--endpoints connection))
            do (handler-case
                 (progn
                   (%activate-connection-endpoint connection endpoint index)
                   (attempt-current-endpoint)
                   (case (target-session-status)
                     (:accept
                      (return-from %connect-implementation connection))
                     (:fallback
                      (unless preferred-endpoint-index
                        (setf preferred-endpoint-index index))
                      (reset-for-retry))
                     (:mismatch
                      (reset-for-retry))))
                 (transport-error (condition)
                   (setf last-transport-condition condition)
                   (reset-for-retry))
                 (error (condition)
                   (fail condition))))
      (if preferred-endpoint-index
          (handler-case
              (let ((endpoint (nth preferred-endpoint-index
                                    (connection--endpoints connection))))
                (%activate-connection-endpoint connection endpoint
                                                preferred-endpoint-index)
                (attempt-current-endpoint)
                (if (member (target-session-status) '(:accept :fallback)
                            :test #'eq)
                    connection
                    (fail
                     (make-condition
                      'connection-error
                      :message
                      "The preferred PostgreSQL endpoint no longer matches target-session-attrs."))))
            (transport-error (condition)
              (fail condition))
            (error (condition)
              (fail condition)))
          (fail
           (or last-transport-condition
               (make-condition
                'connection-error
                :message
                (format nil
                        "No PostgreSQL endpoint matched target-session-attrs ~A."
                        (connection-target-session-attrs connection)))))))))
