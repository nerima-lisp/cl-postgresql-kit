(in-package #:cl-postgresql-kit)

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

(defun execute-prepared (statement &key parameters parameter-formats
                                        result-formats portal-name max-rows
                                        max-result-rows max-result-bytes)
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
