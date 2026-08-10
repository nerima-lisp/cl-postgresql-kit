(in-package #:cl-postgresql-kit)

(defparameter *connection-string-supported-parameters*
  '("application_name" "client_encoding" "connect_timeout" "database"
    "dbname" "fallback_application_name" "host" "hostaddr" "options"
    "password" "port" "replication" "sslcert" "sslkey" "sslmode"
    "sslpassword" "sslrootcert" "channel_binding"
    "target_session_attrs"
    "load_balance_hosts" "user"))

(defun %connection-string-whitespace-p (character)
  (member character '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun %connection-string-contains-nul-p (string)
  (find (code-char 0) string :test #'char=))

(defun %connection-string-parameter-error (parameter format-control &rest arguments)
  (error 'parameter-error
         :parameter parameter
         :message (apply #'format nil format-control arguments)))

(defun %connection-string-unsupported (parameter)
  (error 'unsupported-feature
         :feature parameter
         :message (format nil
                          "Connection parameter ~A is not supported by this client."
                          parameter)))

(defun %connection-string-set-parameter (parameters name value)
  (let* ((canonical-name (if (string= name "dbname") "database" name))
         (entry (assoc canonical-name parameters :test #'string=)))
    (if entry
        (setf (cdr entry) value)
        (push (cons canonical-name value) parameters))
    parameters))

(defun %parse-libpq-connection-string (string)
  (check-type string string)
  (let ((length (length string))
        (position 0)
        (parameters nil))
    (labels ((skip-whitespace ()
               (loop while (and (< position length)
                                (%connection-string-whitespace-p
                                 (char string position)))
                     do (incf position)))
             (read-value (parameter)
               (if (and (< position length)
                        (char= (char string position) #\'))
                   (progn
                     (incf position)
                     (with-output-to-string (stream)
                       (loop
                         (when (>= position length)
                           (%connection-string-parameter-error
                            parameter "Unterminated quoted connection-string value."))
                         (let ((character (char string position)))
                           (cond
                             ((char= character #\\)
                              (incf position)
                              (when (>= position length)
                                (%connection-string-parameter-error
                                 parameter
                                 "A trailing escape has no character to escape."))
                              (write-char (char string position) stream)
                              (incf position))
                             ((char= character #\')
                              (incf position)
                              (when (and (< position length)
                                         (not (%connection-string-whitespace-p
                                               (char string position))))
                                (%connection-string-parameter-error
                                 parameter
                                 "Quoted value must be followed by whitespace or end of input."))
                              (return))
                             (t
                              (write-char character stream)
                              (incf position)))))))
                   (with-output-to-string (stream)
                     (loop while (and (< position length)
                                      (not (%connection-string-whitespace-p
                                            (char string position))))
                           do (let ((character (char string position)))
                                (if (char= character #\\)
                                    (progn
                                      (incf position)
                                      (when (>= position length)
                                        (%connection-string-parameter-error
                                         parameter
                                         "A trailing escape has no character to escape."))
                                      (write-char (char string position) stream)
                                      (incf position))
                                    (progn
                                      (write-char character stream)
                                      (incf position)))))))))
      (loop
        (skip-whitespace)
        (when (>= position length)
          (return (nreverse parameters)))
        (let ((key-start position))
          (loop while (and (< position length)
                           (not (%connection-string-whitespace-p
                                 (char string position)))
                           (not (char= (char string position) #\=)))
                do (incf position))
          (when (= key-start position)
            (%connection-string-parameter-error
             nil "Connection-string parameter name cannot be empty."))
          (let ((key (string-downcase (subseq string key-start position))))
            (when (%connection-string-contains-nul-p key)
              (%connection-string-parameter-error key "Parameter name contains NUL."))
            (skip-whitespace)
            (unless (and (< position length)
                         (char= (char string position) #\=))
              (%connection-string-parameter-error
               key "Connection-string parameter ~A must contain '='." key))
            (incf position)
            (skip-whitespace)
            (let ((value (read-value key)))
              (when (%connection-string-contains-nul-p value)
                (%connection-string-parameter-error
                 key "Connection-string parameter contains NUL."))
              (setf parameters
                    (%connection-string-set-parameter parameters key value)))))))))
