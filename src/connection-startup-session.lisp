(in-package #:cl-postgresql-kit)

(defun %startup-parameters (connection)
  (append
   (remove nil
           (list (cons "user" (connection-user connection))
                 (and (connection-database connection)
                      (cons "database" (connection-database connection)))
                 (cons "application_name" (connection-application-name connection))))
   (connection-startup-parameters connection)))

(defun %connection-transaction-read-only-p (connection)
  (labels ((parse-value (value)
             (cond ((and (stringp value) (string-equal value "on")) t)
                   ((and (stringp value) (string-equal value "off")) nil)
                   (t
                    (error 'protocol-error
                           :message
                           (format nil
                                   "Invalid transaction_read_only value ~S."
                                   value))))))
    (let ((parameter
            (gethash "transaction_read_only"
                     (connection-parameters connection))))
      (if parameter
          (parse-value parameter)
          (let* ((result (%run-exchange connection
                                        :sql "SHOW transaction_read_only"))
                 (rows (query-result-rows result)))
            (unless (= (length rows) 1)
              (error 'protocol-error
                     :message
                     "SHOW transaction_read_only returned an unexpected row count."))
            (let ((row (aref rows 0)))
              (unless (= (length row) 1)
                (error 'protocol-error
                       :message
                       "SHOW transaction_read_only returned an unexpected column count."))
              (parse-value (aref row 0))))))))

(defun %target-session-status (connection)
  (case (connection-target-session-attrs connection)
    (:any :accept)
    ((:read-write :primary)
     (if (%connection-transaction-read-only-p connection)
         :mismatch
         :accept))
    ((:read-only :standby)
     (if (%connection-transaction-read-only-p connection)
         :accept
         :mismatch))
    (:prefer-standby
     (if (%connection-transaction-read-only-p connection)
         :accept
         :fallback))))

(defun %connect-handle-target-session-status (connection index
                                              preferred-endpoint-index)
  (case (%target-session-status connection)
    (:accept
     (values :accept preferred-endpoint-index))
    (:fallback
     (unless preferred-endpoint-index
       (setf preferred-endpoint-index index))
     (%connect-reset-for-retry connection)
     (values :continue preferred-endpoint-index))
    (:mismatch
     (%connect-reset-for-retry connection)
     (values :continue preferred-endpoint-index))))
