(in-package #:cl-postgresql-kit/test)

(deftest unknown-error-fields-are-preserved
  (let* ((payload (join-octets
                   (octets (char-code #\S)) (cstring "ERROR")
                   (octets (char-code #\M)) (cstring "known")
                   (octets (char-code #\Z)) (cstring "future-field")
                   (octets 0)))
         (fields (parse-error-response payload))
         (unknown (cdr (assoc :unknown-fields fields))))
    (is (string= (cdr (assoc :message fields)) "known"))
    (is (string= (cdr (assoc #\Z unknown)) "future-field"))))

(deftest error-field-catalog-round-trips
  (let* ((specifications
           '((#\S :severity "ERROR")
             (#\V :severity-localized "LOCALIZED")
             (#\C :sqlstate "42601")
             (#\M :message "syntax error")
             (#\D :detail "detail")
             (#\H :hint "hint")
             (#\P :position "7")
             (#\p :internal-position "3")
             (#\q :internal-query "select")
             (#\W :where "executor")
             (#\s :schema "public")
             (#\t :table "items")
             (#\c :column "name")
             (#\d :datatype "text")
             (#\n :constraint "items_name_key")
             (#\F :file "postgres.c")
             (#\L :line "42")
             (#\R :routine "parser")))
         (payload
           (apply #'join-octets
                  (append
                   (loop for (code key value) in specifications
                         collect (join-octets
                                  (octets (char-code code))
                                  (cstring value)))
                   (list (octets 0)))))
         (fields (parse-error-response payload))
         (condition (cl-postgresql-kit::%server-error-from-fields fields)))
    (is-alist-values
        fields
      (:severity "ERROR" string=)
      (:severity-localized "LOCALIZED" string=)
      (:sqlstate "42601" string=)
      (:message "syntax error" string=)
      (:detail "detail" string=)
      (:hint "hint" string=)
      (:position "7" string=)
      (:internal-position "3" string=)
      (:internal-query "select" string=)
      (:where "executor" string=)
      (:schema "public" string=)
      (:table "items" string=)
      (:column "name" string=)
      (:datatype "text" string=)
      (:constraint "items_name_key" string=)
      (:file "postgres.c" string=)
      (:line "42" string=)
      (:routine "parser" string=))
    (is-connection-values
        condition
      (server-error-severity "LOCALIZED" string=)
      (server-error-message "syntax error" string=)
      (server-error-sqlstate "42601" string=)
      (server-error-detail "detail" string=)
      (server-error-hint "hint" string=)
      (server-error-position "7" string=)
      (server-error-where "executor" string=)
      (server-error-schema "public" string=)
      (server-error-table "items" string=)
      (server-error-column "name" string=)
      (server-error-datatype "text" string=)
      (server-error-constraint "items_name_key" string=)
      (server-error-file "postgres.c" string=)
      (server-error-line "42" string=)
      (server-error-routine "parser" string=))
    (is (null (server-error-unknown-fields condition)))))

(deftest condition-reports-and-accessors
  (let* ((protocol (make-condition 'protocol-error
                                   :message "bad wire"
                                   :context :wire
                                   :expected 2
                                   :actual 1))
         (transport (make-condition 'transport-error
                                    :message "read failed"
                                    :operation :read
                                    :cause :cause))
         (timeout (make-condition 'timeout-error
                                  :message "timed out"
                                  :cancel-sent-p t
                                  :connection-retired-p nil))
         (tls (make-condition 'tls-error
                              :message "tls failed"
                              :cause :cause))
         (pool (make-condition 'pool-exhausted
                               :message "busy"
                               :timeout 3))
         (feature (make-condition 'unsupported-feature
                                  :message "unsupported"
                                  :feature :tls))
         (parameter (make-condition 'parameter-error
                                    :message "bad parameter"
                                    :parameter 42))
         (server (make-condition 'server-error
                                 :fields '((:message . "syntax error")
                                           (:sqlstate . "42601")
                                           (:detail . "detail")
                                           (:hint . "hint"))
                                 :sqlstate "42601"
                                 :detail "detail"
                                 :hint "hint"))
         (notice (make-condition 'notice
                                 :fields '((:message . "notice")
                                           (:sqlstate . "01000")))))
    (is (string= "bad wire" (postgresql-condition-message protocol)))
    (is (eq :wire (protocol-error-context protocol)))
    (is (= 2 (protocol-error-expected protocol)))
    (is (= 1 (protocol-error-actual protocol)))
    (is (eq :read (transport-error-operation transport)))
    (is (eq :cause (transport-error-cause transport)))
    (is (timeout-error-cancel-sent-p timeout))
    (is (not (timeout-error-connection-retired-p timeout)))
    (is (eq :cause (tls-error-cause tls)))
    (is (= 3 (pool-exhausted-timeout pool)))
    (is (eq :tls (unsupported-feature-name feature)))
    (is (= 42 (parameter-error-parameter parameter)))
    (is (search "bad wire" (princ-to-string protocol)))
    (is (search "read failed" (princ-to-string transport)))
    (is (search "syntax error" (princ-to-string server)))
    (is (search "42601" (princ-to-string server)))
    (is (search "detail" (princ-to-string server)))
    (is (search "hint" (princ-to-string server)))
    (is (search "notice" (princ-to-string notice)))
    (is (search "01000" (princ-to-string notice)))
    (let ((default-server (make-condition 'server-error))
          (default-notice (make-condition 'notice)))
      (is (search "PostgreSQL server error"
                  (princ-to-string default-server)))
      (is (search "PostgreSQL notice"
                  (princ-to-string default-notice))))))
