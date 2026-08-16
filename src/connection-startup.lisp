(in-package #:cl-postgresql-kit)

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

(defun %connect-fail (connection condition)
  (ignore-errors (transport-close (connection-transport connection)))
  (%clear-connection-session-state connection)
  (%clear-connection-secrets connection)
  (%clear-connection-oauth-state connection)
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

(defun %connect-reset-for-retry (connection)
  (ignore-errors (transport-close (connection-transport connection)))
  (%clear-connection-session-state connection)
  (setf (connection-backend-process-id connection) nil
        (connection-backend-secret-key connection) nil
        (connection-open connection) nil
        (connection-state connection) :connecting
        (connection-tls-established-p connection) nil
        (connection-gss-established-p connection) nil))

(defun %connect-attempt (connection request-ssl-p)
  (setf (connection-open connection) nil
        (connection-state connection) :connecting
        (connection-tls-established-p connection) nil
        (connection-gss-established-p connection) nil)
  (transport-open (connection-transport connection))
  (let ((gss-result (when (%connection-gss-mode-requests-p connection)
                      (%gss-request connection)))
        (ssl-server-rejected-p nil)
        (authentication-started-p nil))
    (when (and (not (eq gss-result :accepted))
               (%connection-direct-tls-p connection))
      (%start-tls connection))
    (when (and request-ssl-p
               (not (eq gss-result :accepted)))
      (setf ssl-server-rejected-p
            (eq (%ssl-request connection) :rejected)))
    (transport-write-all (connection-transport connection)
                         (encode-startup-message
                          :protocol-version
                          (connection-protocol-version connection)
                          :parameters (%startup-parameters connection)))
    (transport-flush (connection-transport connection))
    (handler-case
        (progn
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
      (oauth-discovery-required (condition)
        (let ((provider (connection-oauth-discovery-provider connection)))
          (unless (functionp provider)
            (error condition))
          (let ((token (funcall provider
                                connection
                                (oauth-discovery-response condition))))
            (unless (stringp token)
              (error 'authentication-error
                     :message "OAuth discovery provider must return a string."))
            (setf (connection--oauth-discovery-token connection) token)
            (%connect-reset-for-retry connection)
            (%connect-attempt connection request-ssl-p))))
      (error (condition)
        (if (%connect-ssl-fallback-p connection condition request-ssl-p
                                     ssl-server-rejected-p
                                     authentication-started-p)
            (progn
              (%connect-reset-for-retry connection)
              (%connect-attempt connection
                                (eq (connection-ssl-mode connection) :allow)))
            (error condition))))))

(defun %connect-endpoint (connection endpoint index)
  (%clear-connection-oauth-state connection)
  (%activate-connection-endpoint connection endpoint index)
  (%connect-attempt connection (%connect-request-ssl-p connection)))

(defun %retry-preferred-endpoint (connection preferred-endpoint-index)
  (handler-case
      (let ((endpoint (nth preferred-endpoint-index
                           (connection--endpoints connection))))
        (%connect-endpoint connection endpoint preferred-endpoint-index)
        (if (member (%target-session-status connection) '(:accept :fallback)
                    :test #'eq)
            connection
            (%connect-fail
             connection
             (make-condition
              'connection-error
              :message
              "The preferred PostgreSQL endpoint no longer matches target-session-attrs."))))
    (transport-error (condition)
      (%connect-fail connection condition))
    (error (condition)
      (%connect-fail connection condition))))

(defun %connect-implementation (connection)
  (when (connection-open connection)
    (return-from %connect-implementation connection))
  (unless (and (connection-user connection)
               (plusp (length (connection-user connection))))
    (error 'connection-error :message "A PostgreSQL user is required."))
  (%clear-connection-session-state connection)
  (%clear-connection-oauth-state connection)
  (setf (connection-state connection) :connecting
        (connection-tls-established-p connection) nil
        (connection-gss-established-p connection) nil)
  (let ((last-transport-condition nil)
        (preferred-endpoint-index nil))
    (loop for index in (%connection-candidate-indices connection)
          for endpoint = (nth index (connection--endpoints connection))
          do (handler-case
               (progn
                 (%connect-endpoint connection endpoint index)
                 (multiple-value-bind (action next-preferred-endpoint-index)
                     (%connect-handle-target-session-status
                      connection index preferred-endpoint-index)
                   (setf preferred-endpoint-index next-preferred-endpoint-index)
                   (when (eq action :accept)
                     (return-from %connect-implementation connection))))
               (transport-error (condition)
                 (setf last-transport-condition condition)
                 (%connect-reset-for-retry connection))
               (error (condition)
                 (%connect-fail connection condition))))
    (if preferred-endpoint-index
        (%retry-preferred-endpoint connection preferred-endpoint-index)
        (%connect-fail
         connection
         (or last-transport-condition
             (make-condition
              'connection-error
              :message
              (format nil
                       "No PostgreSQL endpoint matched target-session-attrs ~A."
                       (connection-target-session-attrs connection))))))))
