;;;; scripts/run-integration.lisp

(require :asdf)

(defun %integration-script-directory ()
  (make-pathname :name nil
                 :type nil
                 :defaults (or *load-truename*
                               *compile-file-truename*
                               (error "run-integration.lisp has no source pathname."))))

(defun %integration-project-directory ()
  (merge-pathnames "../" (%integration-script-directory)))

(defun %required-environment-variable (name)
  (let ((value (uiop:getenv name)))
    (if (and value (plusp (length value)))
        value
        (error "Environment variable ~A is required for live integration tests."
               name))))

(defun %required-function-symbol (package-name symbol-name)
  (let ((symbol (find-symbol symbol-name package-name)))
    (unless (and symbol (fboundp symbol))
      (error "Package ~A does not provide function ~A."
             package-name symbol-name))
    symbol))

(defun %integration-tls-mode ()
  (let ((value (string-downcase (or (uiop:getenv "PGKIT_TEST_TLS") "auto"))))
    (cond ((string= value "required") :required)
          ((string= value "disabled") :disabled)
          ((string= value "auto") :auto)
          (t
           (error "PGKIT_TEST_TLS must be required, disabled, or auto; got ~A."
                  value)))))

(defun %uri-option-value (uri option)
  (let ((query-position (position #\? uri)))
    (when query-position
      (let ((query (subseq uri (1+ query-position))))
        (loop with start = 0
              for end = (or (position #\& query :start start)
                            (length query))
              for field = (subseq query start end)
              for equals = (position #\= field)
              when (and equals
                        (string-equal option (subseq field 0 equals)))
                do (return (string-downcase (subseq field (1+ equals))))
              when (= end (length query))
                do (return nil)
              do (setf start (1+ end)))))))

(defun %uri-requests-tls-p (uri)
  (let ((ssl-mode (%uri-option-value uri "sslmode")))
    (and ssl-mode (not (string= ssl-mode "disable")))))

(defun %assert-integration (condition format-control &rest format-arguments)
  (unless condition
    (apply #'error format-control format-arguments)))

(let* ((root (%integration-project-directory))
       (asd (merge-pathnames "cl-postgresql-kit.asd" root))
       (uri (%required-environment-variable "PGKIT_TEST_URI"))
       (tls-mode (%integration-tls-mode))
       (uri-requests-tls-p (%uri-requests-tls-p uri))
       (tls-required-p (eq tls-mode :required)))
  (when (and tls-required-p
             (not uri-requests-tls-p))
    (error "PGKIT_TEST_TLS=required needs PGKIT_TEST_URI with sslmode=require, verify-ca, or verify-full."))
  (when (and (eq tls-mode :disabled)
             uri-requests-tls-p)
    (error "PGKIT_TEST_TLS=disabled conflicts with a TLS-enabled sslmode in PGKIT_TEST_URI."))
  (pushnew root asdf:*central-registry* :test #'equal)
  (asdf:load-asd asd)
  (asdf:load-system "cl-postgresql-kit")
  (when (or tls-required-p uri-requests-tls-p)
    (asdf:load-system "cl-postgresql-kit/tls"))
  (let* ((make-connection-from-uri
           (%required-function-symbol "CL-POSTGRESQL-KIT"
                                       "MAKE-CONNECTION-FROM-URI"))
         (connect (%required-function-symbol "CL-POSTGRESQL-KIT" "CONNECT"))
         (disconnect (%required-function-symbol "CL-POSTGRESQL-KIT" "DISCONNECT"))
         (connection-healthy-p
           (%required-function-symbol "CL-POSTGRESQL-KIT" "CONNECTION-HEALTHY-P"))
         (connection-tls-established-p
           (%required-function-symbol "CL-POSTGRESQL-KIT"
                                       "CONNECTION-TLS-ESTABLISHED-P"))
         (query (%required-function-symbol "CL-POSTGRESQL-KIT" "QUERY"))
         (result-row-count
           (%required-function-symbol "CL-POSTGRESQL-KIT" "RESULT-ROW-COUNT"))
         (row-value (%required-function-symbol "CL-POSTGRESQL-KIT" "ROW-VALUE"))
         (connection (funcall make-connection-from-uri uri)))
    (unwind-protect
         (progn
           (funcall connect connection)
           (%assert-integration (funcall connection-healthy-p connection)
                                "The live integration connection is not healthy after CONNECT.")
           (let ((result (funcall query connection "SELECT 1 AS answer")))
             (%assert-integration (= 1 (funcall result-row-count result))
                                  "SELECT 1 returned an unexpected row count.")
             (%assert-integration (= 1 (funcall row-value result 0 0))
                                  "SELECT 1 returned an unexpected value."))
           (when tls-required-p
             (%assert-integration
              (funcall connection-tls-established-p connection)
              "TLS was required but the PostgreSQL connection did not establish TLS."))
           (format t "INTEGRATION-OK tls-established=~:[no~;yes~]~%"
                   (funcall connection-tls-established-p connection)))
      (ignore-errors (funcall disconnect connection)))))
