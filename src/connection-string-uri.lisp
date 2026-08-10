(in-package #:cl-postgresql-kit)

(defun %hex-digit-value (character)
  (let ((code (char-code character)))
    (cond ((and (<= (char-code #\0) code) (<= code (char-code #\9)))
           (- code (char-code #\0)))
          ((and (<= (char-code #\A) code) (<= code (char-code #\F)))
           (+ 10 (- code (char-code #\A))))
          ((and (<= (char-code #\a) code) (<= code (char-code #\f)))
           (+ 10 (- code (char-code #\a)))))))

(defun %percent-decode-connection-component (string &key (start 0) end parameter)
  (let* ((end (or end (length string)))
         (parameter (or parameter string))
         (octets (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0)))
    (loop with position = start
          while (< position end)
          do (if (char= (char string position) #\%)
                 (progn
                   (when (> (+ position 2) (1- end))
                     (%connection-string-parameter-error
                      parameter "Incomplete percent escape in URI component."))
                   (let ((high (%hex-digit-value (char string (1+ position))))
                         (low (%hex-digit-value (char string (+ position 2)))))
                     (unless (and high low)
                       (%connection-string-parameter-error
                        parameter "Invalid percent escape in URI component."))
                     (vector-push-extend (+ (* high 16) low) octets))
                   (incf position 3))
                 (let ((encoded (%string-to-octets
                                 (subseq string position (1+ position)))))
                   (loop for octet across encoded
                         do (vector-push-extend octet octets))
                   (incf position))))
    (let ((decoded (handler-case (%octets-to-string octets)
                     (error ()
                       (%connection-string-parameter-error
                        parameter "URI component is not valid UTF-8.")))))
      (when (%connection-string-contains-nul-p decoded)
        (%connection-string-parameter-error parameter "URI component contains NUL."))
      decoded)))

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

(defun %parse-connection-uri-hostport-list (hostport)
  (let ((length (length hostport))
        (start 0)
        (hosts nil)
        (ports nil))
    (when (zerop length)
      (return-from %parse-connection-uri-hostport-list (values nil nil)))
    (loop
      for separator = (position #\, hostport :start start)
      for end = (or separator length)
      do (when (= start end)
           (%connection-string-parameter-error
            "host" "URI host list cannot contain empty entries."))
         (let ((host nil)
               (port nil))
           (if (char= (char hostport start) #\[)
               (let ((closing-position
                       (position #\] hostport :start (1+ start) :end end)))
                 (unless closing-position
                   (%connection-string-parameter-error
                    "host" "Bracketed IPv6 host is missing ']'."))
                 (when (= closing-position (1+ start))
                   (%connection-string-parameter-error
                    "host" "IPv6 host cannot be empty."))
                 (setf host
                       (%percent-decode-connection-component
                        hostport :start (1+ start) :end closing-position
                        :parameter "host"))
                 (let ((suffix-start (1+ closing-position)))
                   (unless (or (= suffix-start end)
                               (and (char= (char hostport suffix-start) #\:)
                                    (< (1+ suffix-start) end)))
                     (%connection-string-parameter-error
                      "host" "Bracketed IPv6 host may only be followed by ':port'."))
                   (when (< suffix-start end)
                     (setf port (subseq hostport (1+ suffix-start))))))
               (let ((first-colon (position #\: hostport :start start :end end))
                     (last-colon (position #\: hostport :start start :end end
                                           :from-end t)))
                 (when (and first-colon (/= first-colon last-colon))
                   (%connection-string-parameter-error
                    "host" "IPv6 hosts must be enclosed in '[' and ']'."))
                 (if first-colon
                     (progn
                       (unless (= first-colon start)
                         (setf host
                               (%percent-decode-connection-component
                                hostport :start start :end first-colon
                                :parameter "host")))
                       (setf port (subseq hostport (1+ first-colon) end)))
                     (setf host
                           (%percent-decode-connection-component
                            hostport :start start :end end :parameter "host")))))
           (when (and port (zerop (length port)))
             (%connection-string-parameter-error
              "port" "URI port cannot be empty."))
           (push host hosts)
           (push port ports))
         (if separator
             (setf start (1+ separator))
             (return (values (nreverse hosts) (nreverse ports)))))))

(defun %parse-connection-nonnegative-integer (value parameter)
  (let ((number (handler-case (parse-integer value :junk-allowed nil)
                  (error () nil))))
    (unless (and (integerp number) (<= 0 number))
      (%connection-string-parameter-error
       parameter "Value must be a non-negative integer."))
    number))

(defun %connection-uri-scheme-end (string)
  (loop for scheme in '("postgresql://" "postgres://")
        for scheme-length = (length scheme)
        when (and (>= (length string) scheme-length)
                  (string-equal scheme string :end2 scheme-length))
          do (return scheme-length)))

(defun %parse-connection-uri-parameters (uri)
  (let* ((scheme-end (%connection-uri-scheme-end uri))
         (length (length uri)))
    (unless scheme-end
      (%connection-string-parameter-error
       "uri" "URI must use the postgresql:// or postgres:// scheme."))
    (when (position #\# uri :start scheme-end)
      (%connection-string-parameter-error
       "uri" "URI fragments are not valid connection parameters."))
    (let* ((query-position (position #\? uri :start scheme-end))
           (path-position (position #\/ uri :start scheme-end
                                    :end (or query-position length)))
           (authority-end (cond ((and query-position path-position)
                                 (min query-position path-position))
                                (query-position query-position)
                                (path-position path-position)
                                (t length)))
           (parameters nil)
           (authority (subseq uri scheme-end authority-end)))
      (let* ((at-position (position #\@ authority :from-end t))
             (hostport (if at-position
                           (subseq authority (1+ at-position))
                           authority)))
        (when at-position
          (let ((colon-position (position #\: authority :end at-position)))
            (if colon-position
                (progn
                  (setf parameters
                        (%connection-string-set-parameter
                         parameters "user"
                         (%percent-decode-connection-component
                          authority :start 0 :end colon-position :parameter "user")))
                  (setf parameters
                        (%connection-string-set-parameter
                         parameters "password"
                         (%percent-decode-connection-component
                          authority :start (1+ colon-position) :end at-position
                          :parameter "password"))))
                (setf parameters
                      (%connection-string-set-parameter
                       parameters "user"
                       (%percent-decode-connection-component
                        authority :start 0 :end at-position :parameter "user"))))))
        (unless (zerop (length hostport))
          (multiple-value-bind (hosts ports)
              (%parse-connection-uri-hostport-list hostport)
            (if (= (length hosts) 1)
                (when (first hosts)
                  (setf parameters
                        (%connection-string-set-parameter
                         parameters "host" (first hosts))))
                (progn
                  (when (some #'null hosts)
                    (%connection-string-parameter-error
                     "host" "Every host in a URI host list must be non-empty."))
                  (setf parameters
                        (%connection-string-set-parameter
                         parameters "host" (format nil "~{~A~^,~}" hosts)))))
            (when (some #'identity ports)
              (if (= (length ports) 1)
                  (setf parameters
                        (%connection-string-set-parameter
                         parameters "port" (first ports)))
                  (setf parameters
                        (%connection-string-set-parameter
                         parameters "port"
                         (format nil "~{~A~^,~}"
                                 (mapcar (lambda (port) (or port "5432"))
                                         ports))))))))
      (when path-position
        (let ((path-end (or query-position length)))
          (setf parameters
                (%connection-string-set-parameter
                 parameters "dbname"
                 (%percent-decode-connection-component
                  uri :start (1+ path-position) :end path-end :parameter "dbname")))))
      (when query-position
        (let ((query-start (1+ query-position)))
          (loop with pair-start = query-start
                for separator = (position #\& uri :start pair-start)
                for pair-end = (or separator length)
                do (unless (= pair-start pair-end)
                     (let ((equals-position
                             (position #\= uri :start pair-start :end pair-end)))
                       (unless equals-position
                         (%connection-string-parameter-error
                          "query" "Each URI query parameter must contain '='."))
                       (when (= equals-position pair-start)
                         (%connection-string-parameter-error
                          "query" "URI query parameter name cannot be empty."))
                       (let ((name (%percent-decode-connection-component
                                    uri :start pair-start :end equals-position
                                    :parameter "query"))
                             (value (%percent-decode-connection-component
                                     uri :start (1+ equals-position) :end pair-end
                                     :parameter "query")))
                         (setf parameters
                               (%connection-string-set-parameter
                                parameters (string-downcase name) value)))))
                when (null separator)
                  do (return)
                do (setf pair-start (1+ separator)))))
      parameters))))

(defun %connection-options-from-parameters (parameters)
  (dolist (parameter parameters)
    (unless (member (car parameter)
                    *connection-string-supported-parameters*
                    :test #'string=)
      (%connection-string-unsupported (car parameter))))
  (let ((options nil)
        (startup-parameters nil))
    (labels ((add-option (name value)
             (setf options (append options (list name value))))
           (add-tls-option (name value)
             (let ((entry (member :tls-options options :test #'eq)))
               (if entry
                   (setf (cadr entry)
                         (append (cadr entry) (list name value)))
                   (add-option :tls-options (list name value)))))
           (parameter (name)
             (assoc name parameters :test #'string=)))
      (let ((host (parameter "host"))
            (hostaddr (parameter "hostaddr")))
        (when (and host (plusp (length (cdr host))))
          (let ((hosts (%parse-connection-host-list (cdr host) "host")))
            (if (= (length hosts) 1)
                (add-option :host (first hosts))
                (add-option :hosts hosts))))
        (when (and hostaddr (plusp (length (cdr hostaddr))))
          (let ((hostaddrs
                  (%parse-connection-host-list (cdr hostaddr) "hostaddr")))
            (if (= (length hostaddrs) 1)
                (add-option :hostaddr (first hostaddrs))
                (add-option :hostaddrs hostaddrs))
            (unless host
              (if (= (length hostaddrs) 1)
                  (add-option :host (first hostaddrs))
                  (add-option :hosts (copy-list hostaddrs)))))))
      (let ((port (parameter "port")))
        (when port
          (let ((ports (%parse-connection-comma-list
                        (cdr port) "port" #'%parse-connection-port)))
            (if (= (length ports) 1)
                (add-option :port (first ports))
                (add-option :ports ports)))))
      (dolist (mapping '(("user" . :user)
                         ("password" . :password)
                         ("database" . :database)))
        (let ((entry (parameter (car mapping))))
          (when entry
            (add-option (cdr mapping) (cdr entry)))))
      (let ((dbname (parameter "dbname")))
        (when dbname
          (add-option :database (cdr dbname))))
      (let ((application-name (parameter "application_name")))
        (if application-name
            (add-option :application-name (cdr application-name))
            (let ((fallback (parameter "fallback_application_name")))
              (when fallback
                (add-option :application-name (cdr fallback))))))
      (let* ((ssl-mode (parameter "sslmode"))
             (ssl-root-cert (parameter "sslrootcert"))
             (ssl-mode-value
               (when ssl-mode
                 (cond ((string-equal (cdr ssl-mode) "disable") :disable)
                       ((string-equal (cdr ssl-mode) "allow") :allow)
                       ((string-equal (cdr ssl-mode) "prefer") :prefer)
                       ((string-equal (cdr ssl-mode) "require") :require)
                       ((string-equal (cdr ssl-mode) "verify-ca") :verify-ca)
                       ((string-equal (cdr ssl-mode) "verify-full") :verify-full)
                       (t
                        (%connection-string-parameter-error
                         "sslmode" "Unknown sslmode value ~A." (cdr ssl-mode)))))))
        (when (and ssl-root-cert
                   (string-equal (cdr ssl-root-cert) "system"))
          (when (and ssl-mode (not (eq ssl-mode-value :verify-full)))
            (%connection-string-parameter-error
             "sslrootcert"
             "sslrootcert=system requires sslmode=verify-full."))
          (unless ssl-mode
            (setf ssl-mode-value :verify-full)))
        (when ssl-mode-value
          (add-option :ssl-mode ssl-mode-value)))
      (dolist (mapping '( ("sslcert" . :certificate)
                         ("sslkey" . :key)
                         ("sslpassword" . :password)
                         ("sslrootcert" . :verify-location)))
        (let ((entry (parameter (car mapping))))
          (when entry
            (add-tls-option
             (cdr mapping)
             (if (and (string= (car mapping) "sslrootcert")
                      (string-equal (cdr entry) "system"))
                 :default
                 (cdr entry))))))
      (let ((channel-binding (parameter "channel_binding")))
        (when channel-binding
          (add-option :channel-binding
                      (%normalize-channel-binding (cdr channel-binding)))))
      (let ((connect-timeout (parameter "connect_timeout")))
        (when connect-timeout
          (add-option :connect-timeout
                      (%parse-connection-nonnegative-integer
                       (cdr connect-timeout) "connect_timeout"))))
      (let ((target-session-attrs (parameter "target_session_attrs")))
        (when target-session-attrs
          (add-option :target-session-attrs
                      (%normalize-target-session-attrs
                       (cdr target-session-attrs)))))
      (let ((load-balance-hosts (parameter "load_balance_hosts")))
        (when load-balance-hosts
          (add-option :load-balance-hosts
                      (%normalize-load-balance-hosts
                       (cdr load-balance-hosts)))))
      (dolist (name '("client_encoding" "options" "replication"))
        (let ((entry (parameter name)))
          (when entry
            (push (cons name (cdr entry)) startup-parameters))))
      (when startup-parameters
        (add-option :startup-parameters (nreverse startup-parameters)))
      options)))

(defun %merge-connection-option-plists (base overrides)
  (unless (evenp (length overrides))
    (%connection-string-parameter-error
     nil "Connection option overrides must be a property list."))
  (let ((merged (copy-list base)))
    (loop for tail on overrides by #'cddr
          for name = (car tail)
          for value = (cadr tail)
          do (unless (keywordp name)
               (%connection-string-parameter-error
                name "Connection option override names must be keywords."))
             (let ((entry (member name merged :test #'eq)))
               (if entry
                   (setf (cadr entry) value)
                   (setf merged (append merged (list name value))))))
    merged))

(defun parse-connection-uri (uri)
  "Parse a PostgreSQL URI into keyword options accepted by MAKE-CONNECTION.

Only options that this client can honor are accepted.  Unsupported libpq
options signal UNSUPPORTED-FEATURE instead of being silently ignored."
  (check-type uri string)
  (%connection-options-from-parameters
   (%parse-connection-uri-parameters uri)))

(defun parse-connection-string (connection-string)
  "Parse a libpq-style connection string or PostgreSQL URI.

Quoted values and backslash escapes follow libpq connection-string rules.
When CONNECTION-STRING contains a PostgreSQL URI scheme, URI parsing is used."
  (check-type connection-string string)
  (let ((scheme-end (%connection-uri-scheme-end connection-string))
        (scheme-position (search "://" connection-string :test #'char-equal)))
    (cond (scheme-end
           (parse-connection-uri connection-string))
          (scheme-position
           (%connection-string-parameter-error
            "uri" "Unsupported connection URI scheme."))
          (t
           (%connection-options-from-parameters
            (%parse-libpq-connection-string connection-string))))))

(defun make-connection-from-string (connection-string &rest overrides)
  "Create a connection from a libpq-style string, applying OVERRIDES last."
  (apply #'make-connection
         (%merge-connection-option-plists
          (parse-connection-string connection-string)
          overrides)))

(defun make-connection-from-uri (uri &rest overrides)
  "Create a connection from a PostgreSQL URI, applying OVERRIDES last."
  (apply #'make-connection
         (%merge-connection-option-plists
          (parse-connection-uri uri)
          overrides)))
