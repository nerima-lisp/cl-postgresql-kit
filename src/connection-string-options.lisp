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

(defun %connection-endpoint-options (parameters)
  (let ((options nil)
        (host (%connection-parameter parameters "host"))
        (hostaddr (%connection-parameter parameters "hostaddr")))
    (when (and host (plusp (length (cdr host))))
      (setf options
            (%connection-append-list-option
             options :host :hosts
             (%parse-connection-host-list (cdr host) "host"))))
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
                       ("sslrootcert" . :verify-location)))
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
  (%validate-connection-parameters-supported parameters)
  (let ((options nil))
    (dolist (builder '(%connection-endpoint-options
                       %connection-basic-options
                       %connection-ssl-options
                       %connection-tls-options
                       %connection-scalar-options
                       %connection-startup-options))
      (setf options
            (append options
                    (funcall builder parameters))))
    options))

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
