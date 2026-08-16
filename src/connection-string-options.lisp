(in-package #:cl-postgresql-kit)

(defun %parse-connection-port (value &optional (parameter "port"))
  (let ((port (handler-case (parse-integer value :junk-allowed nil)
                (error () nil))))
    (unless (and (integerp port) (<= 1 port 65535))
      (%connection-string-parameter-error
       parameter "Port must be an integer between 1 and 65535."))
    port))

(defun %parse-connection-comma-list (value parameter &optional parser)
  (let ((length (length value))
        (start 0)
        (items nil))
    (when (zerop length)
      (%connection-string-parameter-error
       parameter "Comma-separated connection values cannot be empty."))
    (loop
      for separator = (position #\, value :start start)
      for end = (or separator length)
      do (when (= start end)
           (%connection-string-parameter-error
            parameter "Comma-separated connection values cannot contain empty entries."))
         (push (if parser
                   (funcall parser (subseq value start end) parameter)
                   (subseq value start end))
               items)
         (if separator
             (setf start (1+ separator))
             (return (nreverse items))))))

(defun %parse-connection-host-list (value parameter)
  (mapcar (lambda (host)
            (when (find #\Null host)
              (%connection-string-parameter-error
               parameter "Host values must not contain NUL."))
            host)
          (%parse-connection-comma-list value parameter)))

(defun %parse-connection-nonnegative-integer (value parameter)
  (let ((number (handler-case (parse-integer value :junk-allowed nil)
                  (error () nil))))
    (unless (and (integerp number) (<= 0 number))
      (%connection-string-parameter-error
       parameter "Value must be a non-negative integer."))
    number))

(defun %connection-parameter (parameters name)
  (assoc name parameters :test #'string=))

(defmacro %with-connection-parameter ((entry parameters name) &body body)
  `(let ((,entry (%connection-parameter ,parameters ,name)))
     (when ,entry
       ,@body)))

(defun %connection-append-option (options name value)
  (append options (list name value)))

(defun %connection-append-list-option (options singular plural values)
  (%connection-append-option
   options
   (if (= (length values) 1) singular plural)
   (if (= (length values) 1) (first values) values)))

(defun %connection-append-tls-option (options name value)
  (let ((entry (member :tls-options options :test #'eq)))
    (if entry
        (progn
          (setf (cadr entry)
                (append (cadr entry) (list name value)))
          options)
        (%connection-append-option options :tls-options (list name value)))))

(defun %connection-trim (value)
  (string-trim '(#\Space #\Tab #\Return #\Newline) value))

(defun %connection-environment-value (name)
  (uiop:getenv name))

(defparameter *connection-environment-parameter-mappings*
  '(("PGHOST" . "host")
    ("PGHOSTADDR" . "hostaddr")
    ("PGPORT" . "port")
    ("PGDATABASE" . "database")
    ("PGUSER" . "user")
    ("PGPASSWORD" . "password")
    ("PGOPTIONS" . "options")
    ("PGAPPNAME" . "application_name")
    ("PGSSLMODE" . "sslmode")
    ("PGSSLNEGOTIATION" . "sslnegotiation")
    ("PGSSLCERT" . "sslcert")
    ("PGSSLKEY" . "sslkey")
    ("PGSSLROOTCERT" . "sslrootcert")
    ("PGSSLMINPROTOCOLVERSION" . "ssl_min_protocol_version")
    ("PGGSSENCMODE" . "gssencmode")
    ("PGKRBSRVNAME" . "krbsrvname")
    ("PGCHANNELBINDING" . "channel_binding")
    ("PGREQUIREAUTH" . "require_auth")
    ("PGCONNECT_TIMEOUT" . "connect_timeout")
    ("PGCLIENTENCODING" . "client_encoding")
    ("PGTARGETSESSIONATTRS" . "target_session_attrs")
    ("PGLOADBALANCEHOSTS" . "load_balance_hosts")))

(defun %connection-environment-parameters ()
  (loop for (environment-name . parameter-name)
          in *connection-environment-parameter-mappings*
        for value = (%connection-environment-value environment-name)
        when (and value (plusp (length value)))
          collect (cons parameter-name value)))

(defun %connection-option-pathname (value parameter)
  (unless (or (stringp value) (pathnamep value))
    (%connection-string-parameter-error
     parameter "The value must be a string or pathname."))
  (pathname value))

(defun %connection-service-file-path ()
  (let ((environment-value (%connection-environment-value "PGSERVICEFILE")))
    (if (and environment-value (plusp (length environment-value)))
        (pathname environment-value)
        (merge-pathnames ".pg_service.conf" (user-homedir-pathname)))))

(defun %merge-connection-parameter-alists (base overrides)
  (let ((merged (copy-tree base)))
    (dolist (entry overrides merged)
      (setf merged
            (%connection-string-set-parameter
             merged (car entry) (cdr entry))))))

(defun %read-connection-service-section (service path)
  (unless (probe-file path)
    (%connection-string-parameter-error
     "service" "Service file ~A does not exist." (namestring path)))
  (handler-case
      (with-open-file (stream path :direction :input)
        (let ((section nil)
              (matched nil)
              (parameters nil))
          (loop for raw-line = (read-line stream nil nil)
                while raw-line
                for line = (%connection-trim raw-line)
                do (cond
                     ((or (zerop (length line))
                          (member (char line 0) '(#\# #\;) :test #'char=)) nil)
                     ((char= (char line 0) #\[)
                      (unless (and (> (length line) 2)
                                   (char= (char line (1- (length line))) #\]))
                        (%connection-string-parameter-error
                         "service" "Malformed service section in ~A." (namestring path)))
                      (setf section
                            (%connection-trim
                             (subseq line 1 (1- (length line))))
                            matched (string= section service)))
                     ((and matched (position #\= line))
                      (let* ((equals (position #\= line))
                             (name (%connection-trim (subseq line 0 equals)))
                             (value (%connection-trim (subseq line (1+ equals)))))
                        (when (zerop (length name))
                          (%connection-string-parameter-error
                           "service" "A service option name cannot be empty."))
                        (setf parameters
                              (%connection-string-set-parameter
                               parameters (string-downcase name) value))))
                     ((and section (position #\= line)) nil)
                     (t
                      (%connection-string-parameter-error
                       "service" "Malformed service option in ~A." (namestring path)))))
          (unless matched
            (%connection-string-parameter-error
             "service" "Service ~A is not defined in ~A."
             service (namestring path)))
          (nreverse parameters)))
    (file-error (condition)
      (%connection-string-parameter-error
       "service" "Could not read service file ~A: ~A."
       (namestring path) condition))))

(defun %connection-service-profile-parameters (service path visited)
  (when (member service visited :test #'string=)
    (%connection-string-parameter-error
     "service" "Service definitions contain a cycle at ~A." service))
  (let* ((profile (%read-connection-service-section service path))
         (nested (%connection-parameter profile "service"))
         (local (remove "service" profile :key #'car :test #'string=)))
    (if nested
        (%merge-connection-parameter-alists
         (%connection-service-profile-parameters
          (cdr nested) path (cons service visited))
         local)
        local)))

(defun %connection-parameters-with-service (parameters)
  (let* ((environment-parameters (%connection-environment-parameters))
         (service (%connection-parameter parameters "service"))
         (environment-service (%connection-environment-value "PGSERVICE"))
         (service-name
           (cond
             (service (cdr service))
             ((and environment-service
                   (plusp (length environment-service)))
              environment-service))))
    (when (and service-name (zerop (length service-name)))
      (%connection-string-parameter-error
       "service" "SERVICE cannot be empty."))
    (if service-name
        (%merge-connection-parameter-alists
         environment-parameters
         (%merge-connection-parameter-alists
          (%connection-service-profile-parameters
           service-name (%connection-service-file-path) nil)
          (remove "service" parameters :key #'car :test #'string=)))
        (%merge-connection-parameter-alists
         environment-parameters parameters))))

(defun %connection-passfile-path (parameters)
  (let ((entry (%connection-parameter parameters "passfile"))
        (environment-value (%connection-environment-value "PGPASSFILE")))
    (cond
      (entry
       (when (zerop (length (cdr entry)))
         (%connection-string-parameter-error
          "passfile" "PASSFILE cannot be empty."))
       (values (%connection-option-pathname (cdr entry) "passfile") t))
      ((and environment-value (plusp (length environment-value)))
       (values (pathname environment-value) nil))
      (t
       (values (merge-pathnames ".pgpass" (user-homedir-pathname)) nil)))))

(defun %connection-passfile-fields (line)
  (let ((fields nil)
        (characters nil)
        (escaped nil))
    (loop for character across line
          do (cond
               (escaped
                (push character characters)
                (setf escaped nil))
               ((char= character #\\)
                (setf escaped t))
               ((char= character #\:)
                (push (coerce (nreverse characters) 'string) fields)
                (setf characters nil))
               (t
                (push character characters))))
    (when escaped
      (return-from %connection-passfile-fields nil))
    (push (coerce (nreverse characters) 'string) fields)
    (nreverse fields)))

(defun %connection-passfile-field-matches-p (pattern value)
  (or (string= pattern "*")
      (string= pattern value)))

(defun %connection-passfile-values (parameters name)
  (let ((entry (%connection-parameter parameters name)))
    (if (and entry (plusp (length (cdr entry))))
        (%parse-connection-comma-list (cdr entry) name)
        nil)))

(defun %connection-passfile-password (parameters)
  (multiple-value-bind (path explicit-p)
      (%connection-passfile-path parameters)
    (unless (probe-file path)
      (when explicit-p
        (%connection-string-parameter-error
         "passfile" "Passfile ~A does not exist." (namestring path)))
      (return-from %connection-passfile-password (values nil nil)))
    (let* ((hosts (remove-duplicates
                   (append (%connection-passfile-values parameters "host")
                           (%connection-passfile-values parameters "hostaddr"))
                   :test #'string=))
           (ports (or (%connection-passfile-values parameters "port")
                      (list "5432")))
           (database-entry (or (%connection-parameter parameters "database")
                               (%connection-parameter parameters "dbname")))
           (user-entry (%connection-parameter parameters "user"))
           (database (if database-entry (cdr database-entry) ""))
           (user (if user-entry (cdr user-entry) "")))
      (unless hosts
        (setf hosts (list "")))
      (handler-case
          (with-open-file (stream path :direction :input)
            (loop for raw-line = (read-line stream nil nil)
                  while raw-line
                  for line = (%connection-trim raw-line)
                  do (unless (or (zerop (length line))
                                 (char= (char line 0) #\#))
                       (let ((fields (%connection-passfile-fields line)))
                         (when (and (= (length fields) 5)
                                    (some (lambda (host)
                                            (%connection-passfile-field-matches-p
                                             (first fields) host))
                                          hosts)
                                    (some (lambda (port)
                                            (%connection-passfile-field-matches-p
                                             (second fields) port))
                                          ports)
                                    (%connection-passfile-field-matches-p
                                     (third fields) database)
                                    (%connection-passfile-field-matches-p
                                     (fourth fields) user))
                           (return-from %connection-passfile-password
                             (values t (fifth fields))))))))
        (file-error (condition)
          (when explicit-p
            (%connection-string-parameter-error
             "passfile" "Could not read passfile ~A: ~A."
             (namestring path) condition))))
    (values nil nil))))

(defun %connection-parameters-with-passfile (parameters)
  (if (%connection-parameter parameters "password")
      parameters
      (multiple-value-bind (found-p password)
          (%connection-passfile-password parameters)
        (if found-p
            (%merge-connection-parameter-alists
             parameters (list (cons "password" password)))
            parameters))))

(defun %connection-ssl-mode-value (ssl-mode)
  (cond ((string-equal ssl-mode "disable") :disable)
        ((string-equal ssl-mode "allow") :allow)
        ((string-equal ssl-mode "prefer") :prefer)
        ((string-equal ssl-mode "require") :require)
        ((string-equal ssl-mode "verify-ca") :verify-ca)
        ((string-equal ssl-mode "verify-full") :verify-full)
        (t
         (%connection-string-parameter-error
          "sslmode" "Unknown sslmode value ~A." ssl-mode))))

(%define-enum-normalizer %normalize-ssl-negotiation
  (:default :postgres
   :valid-values (("postgres" :postgres)
                  ("direct" :direct))
   :message "SSLNEGOTIATION must be POSTGRES or DIRECT."))

(%define-enum-normalizer %normalize-gssenc-mode
  (:default :prefer
   :valid-values (("disable" :disable)
                  ("prefer" :prefer)
                  ("require" :require))
   :message "GSSENCMODE must be DISABLE, PREFER, or REQUIRE."))

(defun %connection-startup-parameters (parameters)
  (let ((startup-parameters nil))
    (dolist (name '("client_encoding" "options" "replication"))
      (%with-connection-parameter (entry parameters name)
        (push (cons name (cdr entry)) startup-parameters)))
    (nreverse startup-parameters)))

(defun %validate-connection-parameters-supported (parameters)
  (dolist (parameter parameters)
    (unless (member (car parameter)
                    *connection-string-supported-parameters*
                    :test #'string=)
      (%connection-string-unsupported (car parameter)))))

(defun %connection-default-unix-socket-directory ()
  #+win32 "127.0.0.1"
  #-win32 "/tmp")

(defun %connection-endpoint-options (parameters)
  (let ((options nil)
        (host (%connection-parameter parameters "host"))
        (hostaddr (%connection-parameter parameters "hostaddr")))
    (cond
      ((and host (plusp (length (cdr host))))
       (setf options
             (%connection-append-list-option
              options :host :hosts
              (%parse-connection-host-list (cdr host) "host"))))
      ((and host (zerop (length (cdr host))))
       (setf options
             (%connection-append-list-option
              options :host :hosts
              (list (%connection-default-unix-socket-directory)))))
      ((not (and hostaddr (plusp (length (cdr hostaddr)))))
       (setf options
             (%connection-append-list-option
              options :host :hosts
              (list (%connection-default-unix-socket-directory))))))
    (when (and hostaddr (plusp (length (cdr hostaddr))))
      (let ((hostaddrs (%parse-connection-host-list (cdr hostaddr) "hostaddr")))
        (setf options
              (%connection-append-list-option
               options :hostaddr :hostaddrs hostaddrs))
        (unless host
          (setf options
                (%connection-append-list-option
                 options :host :hosts (copy-list hostaddrs))))))
    (%with-connection-parameter (port parameters "port")
      (setf options
            (%connection-append-list-option
             options :port :ports
             (%parse-connection-comma-list
              (cdr port) "port" #'%parse-connection-port))))
    options))

(defun %connection-basic-options (parameters)
  (let ((options nil))
    (dolist (mapping '(("user" . :user)
                       ("password" . :password)
                       ("passfile" . :passfile)
                       ("database" . :database)))
      (%with-connection-parameter (entry parameters (car mapping))
        (setf options
              (%connection-append-option
               options (cdr mapping) (cdr entry)))))
    (%with-connection-parameter (dbname parameters "dbname")
      (setf options
            (%connection-append-option
             options :database (cdr dbname))))
    (let ((application-name
            (or (%connection-parameter parameters "application_name")
                (%connection-parameter
                 parameters "fallback_application_name"))))
      (when application-name
        (setf options
              (%connection-append-option
               options :application-name (cdr application-name)))))
    options))

(defun %connection-ssl-options (parameters)
  (let ((options nil))
    (let* ((ssl-mode (%connection-parameter parameters "sslmode"))
           (ssl-root-cert (%connection-parameter parameters "sslrootcert"))
           (ssl-mode-value
             (when ssl-mode
               (%connection-ssl-mode-value (cdr ssl-mode)))))
      (when (and ssl-root-cert
                 (string-equal (cdr ssl-root-cert) "system"))
        (when (and ssl-mode (not (eq ssl-mode-value :verify-full)))
          (%connection-string-parameter-error
           "sslrootcert"
           "sslrootcert=system requires sslmode=verify-full."))
        (unless ssl-mode
          (setf ssl-mode-value :verify-full)))
      (when ssl-mode-value
        (setf options
              (%connection-append-option
               options :ssl-mode ssl-mode-value))))
    options))

(defun %connection-tls-options (parameters)
  (let ((options nil))
    (dolist (mapping '(("sslcert" . :certificate)
                       ("sslkey" . :key)
                       ("sslpassword" . :password)
                       ("sslrootcert" . :verify-location)
                       ("ssl_min_protocol_version" . :min-proto-version)))
      (%with-connection-parameter (entry parameters (car mapping))
        (setf options
              (%connection-append-tls-option
               options
               (cdr mapping)
               (if (and (string= (car mapping) "sslrootcert")
                        (string-equal (cdr entry) "system"))
                   :default
                   (cdr entry))))))
    options))

(defun %connection-security-options (parameters)
  (let ((options nil))
    (%with-connection-parameter
        (ssl-negotiation parameters "sslnegotiation")
      (setf options
            (%connection-append-option
             options :ssl-negotiation
             (%normalize-ssl-negotiation (cdr ssl-negotiation)))))
    (%with-connection-parameter (gssenc-mode parameters "gssencmode")
      (setf options
            (%connection-append-option
             options :gssenc-mode
             (%normalize-gssenc-mode (cdr gssenc-mode)))))
    (%with-connection-parameter (service-name parameters "krbsrvname")
      (setf options
            (%connection-append-option
             options :gss-service-name (cdr service-name))))
    (%with-connection-parameter (require-auth parameters "require_auth")
      (setf options
            (%connection-append-option
             options :require-auth
             (%normalize-require-auth (cdr require-auth)))))
    options))

(defun %connection-scalar-options (parameters)
  (let ((options nil))
    (%with-connection-parameter (channel-binding parameters "channel_binding")
      (setf options
            (%connection-append-option
             options :channel-binding
             (%normalize-channel-binding (cdr channel-binding)))))
    (%with-connection-parameter (connect-timeout parameters "connect_timeout")
      (setf options
            (%connection-append-option
             options :connect-timeout
             (%parse-connection-nonnegative-integer
              (cdr connect-timeout) "connect_timeout"))))
    (%with-connection-parameter
        (target-session-attrs parameters "target_session_attrs")
      (setf options
            (%connection-append-option
             options :target-session-attrs
             (%normalize-target-session-attrs
              (cdr target-session-attrs)))))
    (%with-connection-parameter
        (load-balance-hosts parameters "load_balance_hosts")
      (setf options
            (%connection-append-option
             options :load-balance-hosts
             (%normalize-load-balance-hosts
              (cdr load-balance-hosts)))))
    options))

(defun %connection-startup-options (parameters)
  (let ((startup-parameters (%connection-startup-parameters parameters)))
    (when startup-parameters
      (list :startup-parameters startup-parameters))))

(defun %connection-options-from-parameters (parameters)
  (let* ((serviced-parameters (%connection-parameters-with-service parameters))
         (effective-parameters
           (%connection-parameters-with-passfile serviced-parameters)))
    (%validate-connection-parameters-supported effective-parameters)
    (let ((options nil))
      (dolist (builder '(%connection-endpoint-options
                         %connection-basic-options
                         %connection-ssl-options
                         %connection-tls-options
                         %connection-security-options
                         %connection-scalar-options
                         %connection-startup-options))
        (setf options
              (append options
                      (funcall builder effective-parameters))))
      options)))

(defun %merge-connection-option-plists (base overrides)
  (unless (evenp (length overrides))
    (%connection-string-parameter-error
     nil "Connection option overrides must be a property list."))
  (let ((merged (copy-list base))
        (added nil))
    (loop for tail on overrides by #'cddr
          for name = (car tail)
          for value = (cadr tail)
          do (unless (keywordp name)
               (%connection-string-parameter-error
                name "Connection option override names must be keywords."))
             (let ((entry (or (member name merged :test #'eq)
                              (assoc name added :test #'eq))))
               (if (member name merged :test #'eq)
                   (setf (cadr entry) value)
                   (if entry
                       (setf (cdr entry) value)
                       (push (cons name value) added)))))
    (nconc merged
           (loop for (name . value) in (nreverse added)
                 append (list name value)))))
