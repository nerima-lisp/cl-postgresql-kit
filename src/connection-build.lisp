(in-package #:cl-postgresql-kit)

(defun %proper-list-p (value)
  (and (listp value)
       (let ((seen (make-hash-table :test #'eq))
             (tail value))
         (loop
           (cond ((null tail)
                  (return t))
                 ((not (consp tail))
                  (return nil))
                 ((gethash tail seen)
                  (return nil))
                 (t
                  (setf (gethash tail seen) t
                        tail (cdr tail))))))))

(defun %connection-endpoint-string-list (value parameter)
  (unless (%proper-list-p value)
    (error 'parameter-error
           :parameter parameter
           :message "Connection endpoint lists must be proper lists."))
  (mapcar (lambda (item)
            (unless (and (stringp item)
                         (plusp (length item))
                         (not (find #\Null item)))
              (error 'parameter-error
                     :parameter item
                     :message (format nil
                                      "Each ~A entry must be a non-empty NUL-free string."
                                      parameter)))
            item)
          value))

(defun %connection-endpoint-port-list (value)
  (unless (%proper-list-p value)
    (error 'parameter-error
           :parameter value
           :message "PORTS must be a proper list."))
  (mapcar (lambda (item)
            (unless (and (integerp item) (<= 1 item 65535))
              (error 'parameter-error
                     :parameter item
                     :message "Each PORTS entry must be between 1 and 65535."))
            item)
          value))

(defun %normalize-target-session-attrs (value)
  (let ((normalized
          (cond ((null value) :any)
                ((keywordp value) value)
                ((stringp value)
                 (cond ((string-equal value "any") :any)
                       ((string-equal value "read-write") :read-write)
                       ((string-equal value "read-only") :read-only)
                       ((string-equal value "primary") :primary)
                       ((string-equal value "standby") :standby)
                       ((string-equal value "prefer-standby") :prefer-standby)
                       (t nil)))
                (t nil))))
    (unless (member normalized
                    '(:any :read-write :read-only :primary :standby :prefer-standby)
                    :test #'eq)
      (error 'parameter-error
             :parameter value
             :message "TARGET-SESSION-ATTRS must be ANY, READ-WRITE, READ-ONLY, PRIMARY, STANDBY, or PREFER-STANDBY."))
    normalized))

(defun %normalize-load-balance-hosts (value)
  (let ((normalized
          (cond ((null value) :disable)
                ((keywordp value) value)
                ((stringp value)
                 (cond ((string-equal value "disable") :disable)
                       ((string-equal value "random") :random)
                       (t nil)))
                (t nil))))
    (unless (member normalized '(:disable :random) :test #'eq)
      (error 'parameter-error
             :parameter value
             :message "LOAD-BALANCE-HOSTS must be DISABLE or RANDOM."))
    normalized))

(defun %normalize-channel-binding (value)
  (let ((normalized
          (cond ((null value) :prefer)
                ((keywordp value) value)
                ((stringp value)
                 (cond ((string-equal value "disable") :disable)
                       ((string-equal value "prefer") :prefer)
                       ((string-equal value "require") :require)
                       (t nil)))
                (t nil))))
    (unless (member normalized '(:disable :prefer :require) :test #'eq)
      (error 'parameter-error
             :parameter value
             :message "CHANNEL-BINDING must be DISABLE, PREFER, or REQUIRE."))
    normalized))

(defun %make-connection-endpoints (&key host hosts hostaddr hostaddrs port ports
                                         host-supplied-p)
  (when (and hostaddr hostaddrs)
    (error 'parameter-error
           :parameter :hostaddr
           :message "HOSTADDR and HOSTADDRS are mutually exclusive."))
  (when (and ports (not (%proper-list-p ports)))
    (error 'parameter-error
           :parameter ports
           :message "PORTS must be a proper list."))
  (let* ((physical-hosts (cond (hostaddrs
                                (%connection-endpoint-string-list
                                 hostaddrs :hostaddrs))
                               (hostaddr
                                (%connection-endpoint-string-list
                                 (list hostaddr) :hostaddr))
                               (t nil)))
         (logical-hosts (cond (hosts
                               (%connection-endpoint-string-list hosts :hosts))
                              ((and physical-hosts (not host-supplied-p))
                               (copy-list physical-hosts))
                              (t
                               (%connection-endpoint-string-list
                                (list host) :host))))
         (endpoint-ports (if ports
                             (%connection-endpoint-port-list ports)
                             (list port)))
         (count (length logical-hosts)))
    (unless (plusp count)
      (error 'parameter-error
             :parameter :hosts
             :message "At least one PostgreSQL endpoint is required."))
    (when (and physical-hosts (/= (length physical-hosts) count))
      (error 'parameter-error
             :parameter (if hostaddrs :hostaddrs :hostaddr)
             :message "HOSTADDRS must have one address for each HOST candidate."))
    (unless (or (= (length endpoint-ports) 1)
                (= (length endpoint-ports) count))
      (error 'parameter-error
             :parameter :ports
             :message "PORTS must contain one port for each HOST candidate, or one port for all candidates."))
    (loop for index below count
          for logical-host = (nth index logical-hosts)
          for physical-host = (if physical-hosts
                                 (nth index physical-hosts)
                                 logical-host)
          for endpoint-port = (if (= (length endpoint-ports) 1)
                                  (first endpoint-ports)
                                  (nth index endpoint-ports))
          collect (list :host logical-host
                        :hostaddr physical-host
                        :port endpoint-port))))

(defun %make-connection-transport (connection endpoint index)
  (let ((transport
          (cond ((and (zerop index)
                      (connection--initial-transport connection)
                      (not (connection--initial-transport-used-p connection)))
                 (setf (connection--initial-transport-used-p connection) t)
                 (connection--initial-transport connection))
                ((connection--transport-factory connection)
                 (funcall (connection--transport-factory connection)
                          connection endpoint))
                (t
                 (make-socket-transport
                  :host (getf endpoint :hostaddr)
                  :port (getf endpoint :port)
                  :timeout (connection-connect-timeout connection)
                  :tls-options (connection-tls-options connection))))))
    (unless (typep transport 'transport)
      (error 'connection-error
             :message "The connection transport factory did not return a transport."))
    transport))

(defun %activate-connection-endpoint (connection endpoint index)
  (unless (and (integerp index) (>= index 0))
    (error 'parameter-error :parameter index
           :message "Connection endpoint index must be a non-negative integer."))
  (unless (eq endpoint (nth index (connection--endpoints connection)))
    (error 'parameter-error :parameter endpoint
           :message "Unknown connection endpoint."))
  (ignore-errors (transport-close (connection-transport connection)))
  (setf (connection-host connection) (getf endpoint :host)
        (connection-hostaddr connection) (getf endpoint :hostaddr)
        (connection-port connection) (getf endpoint :port)
        (connection-transport connection)
        (%make-connection-transport connection endpoint index))
  connection)

(defun connection-backend-pid (connection)
  "Return the backend process identifier advertised during startup."
  (connection-backend-process-id connection))

(defun connection-backend-secret (connection)
  "Return the backend secret key advertised during startup."
  (connection-backend-secret-key connection))

(defun %normalize-startup-parameters (parameters)
  (unless (%proper-list-p parameters)
    (error 'parameter-error
           :parameter parameters
           :message "Startup parameters must be a proper list of (name . value) pairs."))
  (let ((seen (make-hash-table :test #'equal))
        (normalized nil))
    (dolist (pair parameters (nreverse normalized))
      (unless (and (consp pair)
                   (stringp (car pair))
                   (stringp (cdr pair)))
        (error 'parameter-error
               :parameter pair
               :message "Startup parameters must be (string-name . string-value) pairs."))
      (let ((name (car pair))
            (value (cdr pair)))
        (when (or (zerop (length name))
                  (find #\Null name)
                  (find #\Null value))
          (error 'parameter-error
                 :parameter pair
                 :message "Startup parameter names and values must be non-empty and NUL-free."))
        (when (or (string-equal name "user")
                  (string-equal name "database")
                  (string-equal name "application_name"))
          (error 'parameter-error
                 :parameter pair
                 :message "Startup parameters must not override mandatory connection parameters."))
        (let ((key (string-downcase name)))
          (when (gethash key seen)
            (error 'parameter-error
                   :parameter pair
                   :message "Startup parameter names must be unique."))
          (setf (gethash key seen) t))
        (push (cons name value) normalized)))))

(defun make-connection (&key (host "127.0.0.1" host-supplied-p) hosts hostaddr hostaddrs
                              (port 5432) ports user password
                              oauth-token-provider
                              database (application-name "cl-postgresql-kit")
                              startup-parameters (ssl-mode :disable)
                              (protocol-version +protocol-version-3.0+)
                              tls-options
                              channel-binding
                              target-session-attrs load-balance-hosts
                              gss-token-provider sspi-token-provider
                              transport transport-factory logger metric-registry
                              type-registry
                              query-timeout connect-timeout notice-handler
                              notification-handler (max-notifications 10000)
                              cancel-transport-factory)
  "Create a PostgreSQL connection object.  The network is opened by CONNECT.

HOSTS, HOSTADDRS, and PORTS provide libpq-compatible candidate lists.  A
TRANSPORT-FACTORY receives the connection and an endpoint property list and
must return a fresh or reusable TRANSPORT for that candidate.  When
LOAD-BALANCE-HOSTS is RANDOM, the candidate order is shuffled for each
connection attempt."
  (check-type host string)
  (check-type port (integer 1 65535))
  (check-type oauth-token-provider (or null function))
  (unless (%supported-protocol-version-p protocol-version)
    (error 'parameter-error
           :parameter protocol-version
           :message "Only PostgreSQL protocol versions 3.0 and 3.2 are supported."))
  (check-type gss-token-provider (or null function))
  (check-type sspi-token-provider (or null function))
  (check-type transport (or null transport))
  (check-type transport-factory (or null function))
  (check-type cancel-transport-factory (or null function))
  (check-type metric-registry (or null cl-observability-kit:metric-registry))
  (check-type ssl-mode
              (member :disable :allow :prefer :require :verify-ca :verify-full))
  (check-type query-timeout (or null (real 0 *)))
  (check-type connect-timeout (or null (real 0 *)))
  (check-type max-notifications (integer 0 *))
  (let* ((normalized-tls-options (%normalize-tls-options tls-options))
         (endpoints (%make-connection-endpoints
                     :host host :hosts hosts
                     :hostaddr hostaddr :hostaddrs hostaddrs
                     :port port :ports ports
                     :host-supplied-p host-supplied-p))
         (first-endpoint (first endpoints)))
    (when (and transport
               (> (length endpoints) 1)
               (null transport-factory))
      (error 'parameter-error
             :parameter :transport
             :message "A custom TRANSPORT-FACTORY is required for multiple endpoints."))
    (let ((initial-transport
            (or transport
                (unless transport-factory
                  (make-socket-transport
                   :host (getf first-endpoint :hostaddr)
                   :port (getf first-endpoint :port)
                   :timeout connect-timeout
                   :tls-options normalized-tls-options)))))
      (make-instance 'connection
                     :host (getf first-endpoint :host)
                     :hostaddr (getf first-endpoint :hostaddr)
                     :port (getf first-endpoint :port)
                     :user user
                     :password password
                     :oauth-token-provider oauth-token-provider
                     :database database
                     :application-name application-name
                   :startup-parameters
                     (%normalize-startup-parameters startup-parameters)
                     :protocol-version protocol-version
                     :ssl-mode ssl-mode
                     :tls-options normalized-tls-options
                     :channel-binding
                     (%normalize-channel-binding channel-binding)
                     :target-session-attrs
                     (%normalize-target-session-attrs target-session-attrs)
                     :load-balance-hosts
                     (%normalize-load-balance-hosts load-balance-hosts)
                     :gss-token-provider gss-token-provider
                     :sspi-token-provider sspi-token-provider
                     :transport initial-transport
                     :initial-transport initial-transport
                     :endpoints endpoints
                     :transport-factory transport-factory
                     :logger logger
                     :metrics (%build-connection-metrics metric-registry)
                     :type-registry (or type-registry (make-type-registry))
                     :query-timeout query-timeout
                     :connect-timeout connect-timeout
                     :cancel-transport-factory cancel-transport-factory
                     :notice-handler notice-handler
                     :notification-handler notification-handler
                     :max-notifications max-notifications))))

(defun %connection-log (connection message &rest fields)
  (when (connection-logger connection)
    (log-kit:emit-log (connection-logger connection)
                      log-kit:+level-debug+
                      message
                      fields)))

(defun %require-open-connection (connection)
  (unless (connection-open connection)
    (error 'connection-error
           :message "The PostgreSQL connection is not open.")))

(defun %require-no-active-copy (connection)
  (when (connection--active-copy connection)
    (error 'copy-error
           :message "A COPY operation is already active on this connection.")))

(defun %require-no-active-cursors (connection)
  (when (connection--active-cursors connection)
    (error 'query-error
           :message "A cursor is already active on this connection.")))

(defun %clear-connection-secrets (connection)
  (setf (connection-password connection) nil
        (slot-value connection 'tls-options) nil
        (connection-backend-process-id connection) nil
        (connection-backend-secret-key connection) nil
        (connection-tls-established-p connection) nil)
  (%clear-transport-tls-options (connection-transport connection))
  (%clear-transport-tls-options (connection--initial-transport connection))
  connection)

(defun %clear-connection-session-state (connection)
  (clrhash (connection-parameters connection))
  (clrhash (connection--prepared-statements connection))
  (cl-concurrent-kit:with-lock-held ((connection--notifications-lock connection))
    (setf (connection--notifications connection) nil))
  (setf (connection--active-cursors connection) nil
        (connection--active-copy connection) nil
        (fill-pointer (connection--read-buffer connection)) 0
        (connection--pending-backend-messages connection) nil
        (connection--pending-error connection) nil
        (connection-last-result connection) nil
        (connection-negotiated-protocol-version connection) nil
        (connection--authentication-method connection) nil
        (connection-transaction-status connection) :idle)
  connection)

(defun %octets-to-lower-hex (octets)
  (with-output-to-string (stream)
    (loop for octet across octets
          do (format stream "~(~2,'0X~)" octet))))

(defun %string-to-octets (string)
  (cl-codec-kit:string-to-octets string :encoding :utf-8))

(defun %octets-to-string (octets)
  (cl-codec-kit:octets-to-string octets :encoding :utf-8))
