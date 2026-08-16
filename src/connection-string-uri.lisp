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
      do (let ((host "")
               (port nil))
           (unless (= start end)
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
                              hostport :start start :end end :parameter "host"))))))
           (push host hosts)
           (push port ports))
         (if separator
             (setf start (1+ separator))
             (return (values (nreverse hosts) (nreverse ports)))))))

(defun %connection-uri-scheme-end (string)
  (loop for scheme in '("postgresql://" "postgres://")
        for scheme-length = (length scheme)
        when (and (>= (length string) scheme-length)
                  (string-equal scheme string :end2 scheme-length))
          do (return scheme-length)))

(defun %parse-connection-uri-userinfo (authority)
  (let ((at-position (position #\@ authority :from-end t)))
    (values
     (when at-position
       (let ((colon-position (position #\: authority :end at-position)))
         (if colon-position
             (list (cons "user"
                         (%percent-decode-connection-component
                          authority :start 0 :end colon-position :parameter "user"))
                   (cons "password"
                         (%percent-decode-connection-component
                          authority :start (1+ colon-position) :end at-position
                          :parameter "password")))
             (list (cons "user"
                         (%percent-decode-connection-component
                          authority :start 0 :end at-position :parameter "user"))))))
     (if at-position
         (subseq authority (1+ at-position))
         authority))))

(defun %connection-uri-host-parameter-value (hosts)
  (if (= (length hosts) 1)
      (first hosts)
      (format nil "~{~A~^,~}" hosts)))

(defun %connection-uri-port-parameter-value (ports)
  (if (= (length ports) 1)
      (first ports)
      (format nil "~{~A~^,~}"
              (mapcar (lambda (port) (or port "5432")) ports))))

(defun %parse-connection-uri-authority-parameters (authority)
  (multiple-value-bind (userinfo-parameters hostport)
      (%parse-connection-uri-userinfo authority)
    (let ((parameters (copy-list userinfo-parameters)))
      (unless (zerop (length hostport))
        (multiple-value-bind (hosts ports)
            (%parse-connection-uri-hostport-list hostport)
          (let ((host-parameter
                  (%connection-uri-host-parameter-value hosts)))
            (when host-parameter
              (push (cons "host" host-parameter) parameters)))
          (when (some #'identity ports)
            (push (cons "port"
                        (%connection-uri-port-parameter-value ports))
                  parameters))))
      (nreverse parameters))))

(defun %parse-connection-uri-path-parameters (uri path-position query-position length)
  (when path-position
    (let ((path-end (or query-position length)))
      (list (cons "dbname"
                  (%percent-decode-connection-component
                   uri :start (1+ path-position) :end path-end
                   :parameter "dbname"))))))

(defun %parse-connection-uri-query-parameters (uri query-position length)
  (when query-position
    (loop with parameters = nil
          with pair-start = (1+ query-position)
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
                 (setf parameters
                       (%connection-string-set-parameter
                        parameters
                        (string-downcase
                         (%percent-decode-connection-component
                          uri :start pair-start :end equals-position
                          :parameter "query"))
                        (%percent-decode-connection-component
                         uri :start (1+ equals-position) :end pair-end
                         :parameter "query")))))
          when (null separator)
            do (return (nreverse parameters))
          do (setf pair-start (1+ separator)))))

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
           (authority (subseq uri scheme-end authority-end)))
      (append (%parse-connection-uri-authority-parameters authority)
              (%parse-connection-uri-path-parameters
               uri path-position query-position length)
              (%parse-connection-uri-query-parameters
               uri query-position length)))))

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
