(in-package #:cl-postgresql-kit)

(defparameter *connection-string-supported-parameters*
  '("application_name" "client_encoding" "connect_timeout" "database"
    "dbname" "fallback_application_name" "host" "hostaddr" "options"
    "password" "passfile" "port" "replication" "service"
    "sslcert" "sslkey" "sslmode"
    "sslpassword" "sslrootcert" "ssl_min_protocol_version"
    "ssl_max_protocol_version"
    "sslnegotiation" "gssencmode"
    "krbsrvname" "channel_binding" "require_auth"
    "target_session_attrs"
    "load_balance_hosts" "user"))

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
                         (not (find #\Null item)))
              (error 'parameter-error
                     :parameter item
                     :message (format nil
                                      "Each ~A entry must be a NUL-free string."
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

(defmacro %define-enum-normalizer (name (&key default valid-values message))
  (let ((value (gensym "VALUE"))
        (normalized (gensym "NORMALIZED")))
    `(defun ,name (,value)
       (let ((,normalized
               (cond ((null ,value) ,default)
                     ((keywordp ,value) ,value)
                     ((stringp ,value)
                      (or (second (assoc ,value ',valid-values :test #'string-equal))
                          nil))
                     (t nil))))
         (unless (member ,normalized ',(mapcar #'second valid-values) :test #'eq)
           (error 'parameter-error
                  :parameter ,value
                  :message ,message))
         ,normalized))))

(%define-enum-normalizer %normalize-target-session-attrs
  (:default :any
   :valid-values (("any" :any)
                  ("read-write" :read-write)
                  ("read-only" :read-only)
                  ("primary" :primary)
                  ("standby" :standby)
                  ("prefer-standby" :prefer-standby))
   :message "TARGET-SESSION-ATTRS must be ANY, READ-WRITE, READ-ONLY, PRIMARY, STANDBY, or PREFER-STANDBY."))

(%define-enum-normalizer %normalize-load-balance-hosts
  (:default :disable
   :valid-values (("disable" :disable)
                  ("random" :random))
   :message "LOAD-BALANCE-HOSTS must be DISABLE or RANDOM."))

(%define-enum-normalizer %normalize-channel-binding
  (:default :prefer
   :valid-values (("disable" :disable)
                  ("prefer" :prefer)
                  ("require" :require))
   :message "CHANNEL-BINDING must be DISABLE, PREFER, or REQUIRE."))

(defparameter *require-auth-method-values*
  '(("password" :password)
    ("md5" :md5)
    ("gss" :gss)
    ("sspi" :sspi)
    ("scram-sha-256" :scram-sha-256)
    ("oauth" :oauth)
    ("none" :none)))

(defun %require-auth-method-p (method)
  (member method
          (mapcar #'second *require-auth-method-values*)
          :test #'eq))

(defun %normalized-require-auth-p (value)
  (and (%proper-list-p value)
       (member (getf value :mode) '(:allow :deny) :test #'eq)
       (%proper-list-p (getf value :methods))
       (plusp (length (getf value :methods)))
       (every #'%require-auth-method-p (getf value :methods))
       (= (length (getf value :methods))
          (length (remove-duplicates (getf value :methods) :test #'eq)))))

(defun %normalize-require-auth (value)
  (cond
    ((null value) nil)
    ((%normalized-require-auth-p value)
     (list :mode (getf value :mode)
           :methods (copy-list (getf value :methods))))
    ((not (stringp value))
     (error 'parameter-error
            :parameter :require-auth
            :message "REQUIRE-AUTH must be NIL, a method list string, or a normalized policy."))
    (t
     (let ((value-length (length value))
           (start 0)
           (methods nil)
           (negated-p nil))
       (when (zerop value-length)
         (%connection-string-parameter-error
          "require_auth" "REQUIRE_AUTH cannot be empty."))
       (loop
         for separator = (position #\, value :start start)
         for end = (or separator value-length)
         do (when (= start end)
              (%connection-string-parameter-error
               "require_auth"
               "REQUIRE_AUTH cannot contain empty entries."))
            (let* ((entry (subseq value start end))
                   (negated (and (plusp (length entry))
                                 (char= (char entry 0) #\!)))
                   (name (if negated (subseq entry 1) entry))
                   (method (second
                            (assoc name *require-auth-method-values*
                                   :test #'string-equal))))
              (unless (and (plusp (length name)) method)
                (%connection-string-parameter-error
                 "require_auth"
                 "REQUIRE_AUTH contains an unknown authentication method: ~A."
                 name))
              (when (and methods (not (eql negated negated-p)))
                (%connection-string-parameter-error
                 "require_auth"
                 "REQUIRE_AUTH cannot mix positive and negated methods."))
              (when (member method methods :test #'eq)
                (%connection-string-parameter-error
                 "require_auth"
                 "REQUIRE_AUTH cannot contain duplicate methods."))
              (setf negated-p negated)
              (push method methods))
            (if separator
                (setf start (1+ separator))
                (return (list :mode (if negated-p :deny :allow)
                              :methods (nreverse methods)))))))))

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
          for raw-logical-host = (nth index logical-hosts)
          for logical-host = (if (zerop (length raw-logical-host))
                                (%connection-default-unix-socket-directory)
                                raw-logical-host)
          for raw-physical-host = (if physical-hosts
                                      (nth index physical-hosts)
                                      raw-logical-host)
          for physical-host = (if (zerop (length raw-physical-host))
                                  logical-host
                                  raw-physical-host)
          for endpoint-port = (if (= (length endpoint-ports) 1)
                                  (first endpoint-ports)
                                  (nth index endpoint-ports))
          collect (list :host logical-host
                        :hostaddr physical-host
                        :port endpoint-port))))

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
