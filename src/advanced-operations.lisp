(in-package #:cl-postgresql-kit)

(defun query-pipeline (connection requests &key max-result-rows max-result-bytes)
  "Execute REQUESTS in one PostgreSQL extended-query pipeline.

REQUESTS is a non-empty sequence of SQL strings or PIPELINE-REQUEST objects.
The return value is a list containing one QUERY-RESULT for each request in
input order.  Each request is terminated independently at a ReadyForQuery
boundary, while the frontend messages are flushed as one batch."
  (%query-pipeline connection requests
                   :max-result-rows max-result-rows
                   :max-result-bytes max-result-bytes))

(defun query-pipeline-async (connection requests &key max-result-rows max-result-bytes)
  "Execute QUERY-PIPELINE asynchronously using CL-CONCURRENT-KIT."
  (cl-concurrent-kit:future
    (%query-pipeline connection requests
                     :max-result-rows max-result-rows
                     :max-result-bytes max-result-bytes)))

(defun query-async (connection sql &rest arguments)
  "Execute QUERY asynchronously using CL-CONCURRENT-KIT's future API."
  (cl-concurrent-kit:future
    (apply #'query connection sql arguments)))

(defun query-cps (connection sql on-success
                 &key on-error parameters parameter-type-oids parameter-formats
                   result-formats statement-name portal-name max-rows
                   max-result-rows max-result-bytes)
  "Execute QUERY and compose its Promise with success and error continuations.

ON-SUCCESS receives the QUERY-RESULT.  When supplied, ON-ERROR receives the
condition signaled by QUERY.  The returned Promise represents the value
returned by the selected continuation."
  (check-type on-success function)
  (when on-error
    (check-type on-error function))
  (cl-concurrent-kit:promise-then
   (query-async connection sql
                :parameters parameters
                :parameter-type-oids parameter-type-oids
                :parameter-formats parameter-formats
                :result-formats result-formats
                :statement-name statement-name
                :portal-name portal-name
                :max-rows max-rows
                :max-result-rows max-result-rows
                :max-result-bytes max-result-bytes)
   on-success
   on-error))

(defun query-pipeline-cps (connection requests on-success
                           &key on-error max-result-rows max-result-bytes)
  "Execute QUERY-PIPELINE and compose its Promise with continuations.

ON-SUCCESS receives the list of QUERY-RESULT objects.  When supplied,
ON-ERROR receives a condition.  The returned Promise represents the value
returned by the selected continuation."
  (check-type on-success function)
  (when on-error
    (check-type on-error function))
  (cl-concurrent-kit:promise-then
   (query-pipeline-async connection requests
                         :max-result-rows max-result-rows
                         :max-result-bytes max-result-bytes)
   on-success
   on-error))

(defun function-call (connection function-oid arguments
                      &key argument-type-oids argument-formats
                        (result-format 0) result-type-oid)
  "Call a PostgreSQL function through the v3 FunctionCall protocol message.

ARGUMENTS are plain values, octet vectors, SQL-NULL, or TYPED-VALUE objects.
RESULT-TYPE-OID requests decoding of a non-NULL result through the connection
type registry; without it, the result is returned as an octet vector."
  (check-type connection connection)
  (unless (and (integerp function-oid) (<= 0 function-oid #xffffffff))
    (error 'parameter-error
           :parameter function-oid
           :message "Function OIDs must be unsigned 32-bit integers."))
  (let* ((argument-list (%parameter-sequence-list arguments :arguments))
         (argument-count (length argument-list))
         (argument-type-oids (%parameter-oids-list argument-type-oids argument-count))
         (argument-formats (%parameter-formats-list argument-formats argument-count))
         (prepared-parameters (%prepare-parameters connection argument-list
                                                    argument-type-oids
                                                    argument-formats))
         (wire-arguments (mapcar (lambda (original parameter)
                                   (if (sql-null-p original)
                                       +sql-null+
                                       (getf parameter :value)))
                                 argument-list prepared-parameters))
         (wire-formats (mapcar (lambda (parameter)
                                 (getf parameter :format))
                               prepared-parameters))
         (result-format (%protocol-format-code result-format))
         (result-type-oid (and result-type-oid
                               (first (%parameter-oids-list (list result-type-oid))))))
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%require-open-connection connection)
      (%require-no-active-copy connection)
      (%require-no-active-cursors connection)
      (%with-operation-boundary (connection :operation :function-call)
        (%send-frontend-message
         connection
         (encode-function-call-message function-oid wire-arguments
                                       :argument-formats wire-formats
                                       :result-format result-format))
        (%send-frontend-message connection (encode-sync-message))
        (let ((result nil)
              (received-p nil)
              (pending nil))
          (loop for message = (%read-backend-message connection)
                do (multiple-value-bind (kind value)
                       (%process-backend-message connection message)
                     (case kind
                       (:function-call-response
                        (when received-p
                          (error 'protocol-error
                                 :message "PostgreSQL returned multiple FunctionCallResponse messages."))
                        (setf result
                              (parse-function-call-response
                               (backend-message-payload message))
                              received-p t))
                       (:error-response (setf pending value))
                       (:ready-for-query
                        (when pending (error pending))
                        (unless received-p
                          (error 'protocol-error
                                 :message "PostgreSQL completed a function call without a result."))
                        (return
                          (if (or (null result-type-oid)
                                  (sql-null-p result))
                              result
                              (decode-value (connection-type-registry connection)
                                            result-type-oid result
                                            :format result-format))))
                       ((:notice-response :notification-response) nil)
                       (otherwise
                        (error 'protocol-error
                               :message "Unexpected PostgreSQL message during a function call."
                               :context kind))))))))))

(defun prepare (connection sql &key name parameter-type-oids)
  "Prepare SQL on CONNECTION and return a PREPARED-STATEMENT."
  (check-type connection connection)
  (check-type sql string)
  (let ((statement-name (or name
                            (format nil "pg_kit_stmt_~D"
                                    (incf *statement-counter*)))))
    (%query-string-or-nil statement-name :name)
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%require-open-connection connection)
      (%require-no-active-copy connection)
      (%require-no-active-cursors connection)
      (%with-operation-boundary (connection :operation :prepare)
        (%send-frontend-message
         connection
         (encode-parse-message sql :statement-name statement-name
                               :parameter-type-oids
                               (%parameter-oids-list parameter-type-oids)))
        (%send-frontend-message
         connection (encode-describe-message statement-name :kind :statement))
        (%send-frontend-message connection (encode-sync-message))
        (let ((columns #())
              (server-parameter-type-oids
                (%parameter-oids-list parameter-type-oids))
              (pending nil))
          (loop for message = (%read-backend-message connection)
                do (multiple-value-bind (kind value)
                       (%process-backend-message connection message)
                     (case kind
                       (:parameter-description
                        (setf server-parameter-type-oids
                              (coerce
                               (parse-parameter-description
                                (backend-message-payload message))
                               'list)))
                       (:row-description
                        (setf columns
                              (parse-row-description
                               (backend-message-payload message))))
                       (:error-response (setf pending value))
                       (:ready-for-query
                        (when pending (error pending))
                        (return)))))
          (let ((statement (%make-prepared-statement
                            :name statement-name
                            :sql sql
                            :parameter-type-oids server-parameter-type-oids
                            :columns columns
                            :connection connection)))
            (setf (gethash statement-name (connection--prepared-statements connection))
                  statement)
            statement))))))

(defun execute-prepared (statement &key parameters parameter-formats result-formats
                                  portal-name max-rows max-result-rows max-result-bytes)
  "Execute a PREPARED-STATEMENT."
  (check-type statement prepared-statement)
  (let ((connection (prepared-statement-connection statement)))
    (%query connection (prepared-statement-sql statement)
            :parameters parameters
            :parameter-type-oids (prepared-statement-parameter-type-oids statement)
            :parameter-formats parameter-formats
            :result-formats result-formats
            :statement-name (prepared-statement-name statement)
            :portal-name portal-name
            :max-rows max-rows
            :max-result-rows max-result-rows
            :max-result-bytes max-result-bytes
            :expected-prepared-statement statement)))

(defun close-prepared (statement)
  "Close a prepared statement on the server and remove it from its connection.

Closing an already-closed statement is idempotent.  A statement object that
has been replaced in the connection cache signals PARAMETER-ERROR without
closing the replacement."
  (check-type statement prepared-statement)
  (let ((connection (prepared-statement-connection statement)))
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%require-open-connection connection)
      (%require-no-active-copy connection)
      (%require-no-active-cursors connection)
      (let* ((statement-name (prepared-statement-name statement))
             (prepared-statements (connection--prepared-statements connection))
             (cached-statement (gethash statement-name prepared-statements)))
        (cond
          ((null cached-statement)
           t)
          ((not (eq cached-statement statement))
           (error 'parameter-error
                  :parameter statement-name
                  :message
                  "The prepared statement is no longer active in the connection cache."))
          (t
           (%with-operation-boundary (connection :operation :close-prepared)
             (%send-frontend-message
              connection (encode-close-message statement-name
                                               :kind :statement))
             (%send-frontend-message connection (encode-sync-message))
             (let ((pending nil))
               (loop for message = (%read-backend-message connection)
                     do (multiple-value-bind (kind value)
                            (%process-backend-message connection message)
                          (case kind
                            (:error-response (setf pending value))
                            (:ready-for-query
                             (when pending (error pending))
                             (return)))))
               (remhash statement-name prepared-statements)
               t))))))))

(defparameter *transaction-isolation-levels*
  '(("READ UNCOMMITTED" . "READ UNCOMMITTED")
    ("READ COMMITTED" . "READ COMMITTED")
    ("REPEATABLE READ" . "REPEATABLE READ")
    ("SERIALIZABLE" . "SERIALIZABLE")))

(defun %transaction-isolation-sql (isolation)
  (let* ((raw (cond ((symbolp isolation) (symbol-name isolation))
                    ((stringp isolation) isolation)))
         (normalized (and raw
                          (substitute #\Space #\-
                                      (string-upcase raw)))))
    (or (cdr (assoc normalized *transaction-isolation-levels*
                     :test #'string=))
        (error 'parameter-error
               :parameter isolation
               :message "Unsupported transaction isolation level."))))

(defun begin-transaction (connection &key isolation read-only deferrable)
  (let ((clauses (remove nil
                         (list (and isolation
                                    (format nil "ISOLATION LEVEL ~A"
                                            (%transaction-isolation-sql isolation)))
                               (and read-only "READ ONLY")
                               (and deferrable "DEFERRABLE")))))
    (query connection (format nil "BEGIN~@[ ~{~A~^ ~}~]" clauses))))

(defun commit-transaction (connection)
  (query connection "COMMIT"))

(defun rollback-transaction (connection)
  (query connection "ROLLBACK"))

(defun %quote-identifier (name)
  (check-type name string)
  (when (zerop (length name))
    (error 'parameter-error :parameter name
           :message "An SQL identifier must not be empty."))
  (when (find #\Null name)
    (error 'parameter-error :parameter name
           :message "An SQL identifier must not contain NUL."))
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop for character across name
          do (if (char= character #\")
                 (write-string "\"\"" stream)
                 (write-char character stream)))
    (write-char #\" stream)))

(defun %validate-notify-channel (channel)
  (check-type channel string)
  (when (zerop (length channel))
    (error 'parameter-error :parameter channel
           :message "A NOTIFY channel must not be empty."))
  (when (find #\Null channel)
    (error 'parameter-error :parameter channel
           :message "A NOTIFY channel must not contain NUL."))
  channel)

(defun listen (connection channel)
  "Subscribe CONNECTION to asynchronous NOTIFY messages on CHANNEL."
  (query connection
         (format nil "LISTEN ~A" (%quote-identifier channel))))

(defun unlisten (connection channel)
  "Remove CONNECTION's asynchronous NOTIFY subscription for CHANNEL."
  (query connection
         (format nil "UNLISTEN ~A" (%quote-identifier channel))))

(defun unlisten-all (connection)
  "Remove all asynchronous NOTIFY subscriptions from CONNECTION."
  (query connection "UNLISTEN *"))

(defun notify (connection channel &optional (payload nil payload-p))
  "Send a NOTIFY on CHANNEL, optionally carrying PAYLOAD."
  (%validate-notify-channel channel)
  (when payload-p
    (check-type payload string)
    (when (find #\Null payload)
      (error 'parameter-error :parameter payload
             :message "A NOTIFY payload must not contain NUL.")))
  (query connection
         "SELECT pg_notify($1, $2)"
         :parameters (list channel (if payload-p payload ""))
         :parameter-type-oids '(25 25)))

(defun savepoint (connection name)
  (query connection (format nil "SAVEPOINT ~A" (%quote-identifier name))))

(defun release-savepoint (connection name)
  (query connection (format nil "RELEASE SAVEPOINT ~A" (%quote-identifier name))))

(defun rollback-to-savepoint (connection name)
  (query connection (format nil "ROLLBACK TO SAVEPOINT ~A" (%quote-identifier name))))

(defmacro with-transaction ((connection &key isolation read-only deferrable)
                            &body body)
  (let ((connection-var (gensym "CONNECTION-"))
        (committed-var (gensym "COMMITTED-")))
    `(let ((,connection-var ,connection)
           (,committed-var nil))
       (begin-transaction ,connection-var
                          :isolation ,isolation
                          :read-only ,read-only
                          :deferrable ,deferrable)
       (unwind-protect
            (multiple-value-prog1 (progn ,@body)
              (commit-transaction ,connection-var)
              (setf ,committed-var t))
         (unless ,committed-var
           (ignore-errors (rollback-transaction ,connection-var)))))))
