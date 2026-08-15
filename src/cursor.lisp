(in-package #:cl-postgresql-kit)

(defun %cursor-append-rows (cursor raw-rows)
  (let ((decoded
          (loop for raw-row in (nreverse raw-rows)
                collect (%decode-query-row
                         (cursor-connection cursor)
                         (cursor-columns cursor)
                         raw-row))))
    (setf (cursor--pending-rows cursor)
          (concatenate 'vector
                       (cursor--pending-rows cursor)
                       (coerce decoded 'vector)))))

(defun %cursor-signal-pending-error (connection)
  (when (connection--pending-error connection)
    (let ((condition (connection--pending-error connection)))
      (setf (connection--pending-error connection) nil)
      (error condition))))

(defun %cursor-finalize-close (cursor connection)
  (setf (cursor-closed-p cursor) t
        (connection--active-cursors connection)
        (delete cursor (connection--active-cursors connection)
                :test #'eq))
  t)

(defun %cursor-read-batch (cursor &optional execute-message)
  (let ((connection (cursor-connection cursor))
        (raw-rows nil)
        (command-tag nil)
        (command-complete-p nil)
        (portal-suspended-p nil))
    (labels ((account-result-payload (message)
               (let ((limit (cursor-max-result-bytes cursor)))
                 (when limit
                   (let ((total (+ (cursor--result-bytes cursor)
                                   (length (backend-message-payload message)))))
                     (when (> total limit)
                       (%retire-connection connection)
                       (error 'query-error
                              :message "The client-side cursor result byte limit was exceeded."))
                     (setf (cursor--result-bytes cursor) total)))))
             (account-result-row ()
               (let ((limit (cursor-max-result-rows cursor)))
                 (when (and limit
                            (>= (cursor--result-row-count cursor) limit))
                   (%retire-connection connection)
                   (error 'query-error
                          :message "The client-side cursor result row limit was exceeded."))
                 (incf (cursor--result-row-count cursor)))))
      (when execute-message
        (%send-frontend-message connection execute-message)
        (%send-frontend-message connection (encode-sync-message)))
      (loop for message = (%read-backend-message connection)
            do (multiple-value-bind (kind ignored)
                   (%process-backend-message connection message)
                 (declare (ignore ignored))
                 (case kind
                   (:row-description
                    (account-result-payload message)
                    (setf (cursor-columns cursor)
                          (parse-row-description
                           (backend-message-payload message))))
                   (:data-row
                    (account-result-payload message)
                    (account-result-row)
                    (push (parse-data-row (backend-message-payload message)) raw-rows))
                   (:command-complete
                    (account-result-payload message)
                    (setf command-complete-p t
                          command-tag
                          (parse-command-complete
                           (backend-message-payload message))))
                   (:empty-query-response
                    (account-result-payload message)
                    (setf command-complete-p t))
                   (:portal-suspended
                    (account-result-payload message)
                    (setf portal-suspended-p t))
                   (:error-response nil)
                   (:copy-in-response
                    (error 'copy-error
                           :message "COPY IN cannot be consumed by a cursor."))
                   (:copy-out-response
                   (error 'copy-error
                           :message "COPY OUT cannot be consumed by a cursor."))
                   (:copy-both-response
                    (error 'copy-error
                           :message "COPY BOTH cannot be consumed by a cursor."))
                   (:ready-for-query
                    (%cursor-signal-pending-error connection)
                    (return)))))
      (%cursor-append-rows cursor raw-rows)
      (when command-complete-p
        (setf (cursor-done-p cursor) t
              portal-suspended-p nil))
      (setf (cursor--suspended-p cursor) portal-suspended-p)
      (when command-complete-p
        (setf (cursor-command-tag cursor) command-tag))
      cursor)))

(defun %cursor-take-rows (cursor count)
  (let* ((pending (cursor--pending-rows cursor))
         (returned-count (min count (length pending)))
         (rows (subseq pending 0 returned-count)))
    (setf (cursor--pending-rows cursor)
          (subseq pending returned-count))
    (values rows
            (and (cursor-done-p cursor)
                 (zerop (length (cursor--pending-rows cursor)))))))

(defun %cursor-fetch-count (value default)
  (let ((count (or value default)))
    (unless (and (integerp count) (plusp count))
      (error 'parameter-error
             :parameter value
             :message "Cursor fetch size must be a positive integer."))
    (when (> count #xffffffff)
      (error 'parameter-error
             :parameter value
             :message "Cursor fetch size must fit in an unsigned 32-bit integer."))
    count))

(defun open-cursor (connection sql &key parameters parameter-type-oids
                                     parameter-formats result-formats
                                     statement-name portal-name (fetch-size 100)
                                     max-result-rows max-result-bytes)
  "Open a named portal and return a CURSOR for incremental result fetching.

The first batch is fetched immediately.  CURSOR-FETCH returns a vector of
decoded rows and a second value that is true once no more rows remain.  When
specified, MAX-RESULT-ROWS and MAX-RESULT-BYTES apply to the aggregate stream
across all fetches."
  (check-type connection connection)
  (check-type sql string)
  (check-type max-result-rows (or null (integer 0 *)))
  (check-type max-result-bytes (or null (integer 0 *)))
  (let ((fetch-size (%cursor-fetch-count fetch-size 100))
        (max-result-bytes (%max-result-bytes-value max-result-bytes))
        (statement-name (or statement-name
                            (format nil "pg_kit_cursor_stmt_~D"
                                    (incf *statement-counter*))))
        (portal-name (or portal-name
                         (format nil "pg_kit_cursor_portal_~D"
                                 (incf *statement-counter*)))))
    (%query-string-or-nil statement-name :statement-name)
    (%query-string-or-nil portal-name :portal-name)
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%require-open-connection connection)
      (%require-no-active-copy connection)
      (%require-no-active-cursors connection)
      (%call-with-query-timeout
       connection
       (lambda ()
         (%with-exchange-failure-retirement (connection)
           (let* ((parameter-list (%parameter-sequence-list parameters :parameters))
                  (parameter-count (length parameter-list))
                  (parameter-type-oids
                    (%parameter-oids-list parameter-type-oids parameter-count))
                  (parameter-formats
                    (%parameter-formats-list parameter-formats parameter-count)))
             (multiple-value-bind (prepared-statement effective-parameter-type-oids)
                 (%resolve-prepared-statement
                  connection statement-name sql parameter-count parameter-type-oids)
               (let* ((prepared-parameters
                        (%prepare-parameters connection parameter-list
                                              effective-parameter-type-oids
                                              parameter-formats))
                      (cursor (make-instance 'cursor
                                             :connection connection
                                             :statement-name statement-name
                                             :close-statement-p (not prepared-statement)
                                             :portal-name portal-name
                                             :fetch-size fetch-size
                                             :max-result-rows max-result-rows
                                             :max-result-bytes max-result-bytes)))
                 (setf (connection--pending-error connection) nil)
                 (unless prepared-statement
                   (%send-frontend-message
                    connection
                    (encode-parse-message
                     sql
                     :statement-name statement-name
                     :parameter-type-oids
                     (or effective-parameter-type-oids
                         (mapcar (lambda (parameter) (getf parameter :oid))
                                 prepared-parameters)))))
                 (%send-frontend-message
                  connection
                  (encode-bind-message
                   (mapcar (lambda (parameter) (getf parameter :value))
                           prepared-parameters)
                   :portal-name portal-name
                   :statement-name statement-name
                   :parameter-formats
                   (mapcar (lambda (parameter) (getf parameter :format))
                           prepared-parameters)
                   :result-formats result-formats))
                 (%send-frontend-message
                  connection (encode-describe-message portal-name :kind :portal))
                 (%cursor-read-batch
                  cursor
                  (encode-execute-message :portal-name portal-name
                                          :max-rows fetch-size))
                 (push cursor (connection--active-cursors connection))
                 cursor)))))))))

(defun cursor-fetch (cursor &optional max-rows)
  "Fetch up to MAX-ROWS from CURSOR and return rows plus an end-of-stream flag."
  (check-type cursor cursor)
  (let ((connection (cursor-connection cursor)))
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%require-open-connection connection)
      (when (cursor-closed-p cursor)
        (error 'query-error :message "The cursor is closed."))
      (let ((count (%cursor-fetch-count max-rows (cursor-fetch-size cursor))))
        (if (plusp (length (cursor--pending-rows cursor)))
            (%cursor-take-rows cursor count)
            (if (cursor-done-p cursor)
                (values #() t)
                (%call-with-query-timeout
                 connection
                 (lambda ()
                   (%with-exchange-failure-retirement (connection)
                     (%cursor-read-batch
                      cursor
                      (encode-execute-message
                       :portal-name (cursor-portal-name cursor)
                       :max-rows count))
                     (%cursor-take-rows cursor count))))))))))

(defun cursor-close (cursor)
  "Close CURSOR and release its server-side portal and owned statement."
  (check-type cursor cursor)
  (let ((connection (cursor-connection cursor)))
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (if (cursor-closed-p cursor)
          t
          (progn
            (unless (connection-open connection)
              (return-from cursor-close
                (%cursor-finalize-close cursor connection)))
            (%call-with-query-timeout
             connection
             (lambda ()
               (%with-exchange-failure-retirement (connection)
                 (unless (cursor-done-p cursor)
                   (%send-frontend-message
                    connection
                    (encode-close-message
                     (cursor-portal-name cursor)
                     :kind :portal)))
                 (when (cursor--close-statement-p cursor)
                   (%send-frontend-message
                    connection
                    (encode-close-message
                     (cursor-statement-name cursor)
                     :kind :statement)))
                 (%send-frontend-message connection (encode-sync-message))
                 (loop for message = (%read-backend-message connection)
                       do (multiple-value-bind (kind value)
                              (%process-backend-message connection message)
                            (declare (ignore value))
                            (case kind
                              (:error-response nil)
                              (:ready-for-query
                               (%cursor-signal-pending-error connection)
                               (return)))))
                 (%cursor-finalize-close cursor connection)))))))))
