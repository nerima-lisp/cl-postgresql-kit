(in-package #:cl-postgresql-kit)

(defun connect (connection)
  "Open CONNECTION, negotiate SSL if requested, and authenticate it."
  (check-type connection connection)
  (cl-concurrent-kit:with-lock-held ((connection--lock connection))
    (%connect-implementation connection)))

(defun disconnect (connection)
  "Close CONNECTION.  Disconnect is idempotent."
  (check-type connection connection)
  (cl-concurrent-kit:with-lock-held ((connection--lock connection))
    (when (connection-open connection)
      (ignore-errors
        (%send-frontend-message connection (encode-terminate-message))))
    (ignore-errors (transport-close (connection-transport connection)))
  (%clear-connection-session-state connection)
    (%clear-connection-secrets connection)
    (setf (connection-open connection) nil
          (connection-state connection) :closed
          (connection--initial-transport-used-p connection) nil)
    connection))

(defun connection-healthy-p (connection)
  (and (connection-open connection)
       (transport-alive-p (connection-transport connection))
       (member (connection-state connection) '(:ready :in-transaction
                                               :failed-transaction))))

(defun connection-notifications (connection)
  "Return queued notices and notifications, newest first."
  (cl-concurrent-kit:with-lock-held ((connection--notifications-lock connection))
    (copy-list (connection--notifications connection))))

(defun %poll-notification-locked (connection)
  (loop
    (when (connection--pending-backend-messages connection)
      (return))
    (%connection-fill-read-buffer connection)
    (let ((message (%connection-buffered-backend-message connection)))
      (unless message
        (return))
      (case (backend-message-kind (backend-message-type message))
        ((:notice-response :notification-response)
         (%process-backend-message connection message))
        (otherwise
         (push message (connection--pending-backend-messages connection))
         (return)))))
  (cl-concurrent-kit:with-lock-held ((connection--notifications-lock connection))
    (pop (connection--notifications connection))))

(defun poll-notification (connection)
  "Return one already-available notice or notification, or NIL.

POLL-NOTIFICATION never waits for socket input.  Use WAIT-FOR-NOTIFICATION
when a blocking wait with an optional timeout is required."
  (check-type connection connection)
  (cl-concurrent-kit:with-lock-held ((connection--lock connection))
    (%require-open-connection connection)
    (%poll-notification-locked connection)))

(defun %notification-timeout-deadline (timeout)
  (and timeout
       (cl-date-kit:instant-plus-duration
        (cl-date-kit:instant-now)
        (cl-date-kit:duration-of-nanos
         (round (* timeout 1000000000))))))

(defun %notification-timeout-remaining (deadline)
  (and deadline
       (max 0
            (cl-date-kit:duration-to-seconds
             (cl-date-kit:duration-between
              (cl-date-kit:instant-now)
              deadline)))))

(defun wait-for-notification (connection &optional timeout)
  "Wait for and return one notice or notification.

TIMEOUT is a non-negative number of seconds, or NIL for no deadline.  NIL is
returned when the deadline expires or when a non-notification backend message
is already pending for the caller that owns the connection exchange."
  (check-type connection connection)
  (unless (or (null timeout)
              (and (realp timeout) (not (minusp timeout))))
    (error 'parameter-error :parameter timeout
           :message "Notification timeout must be a non-negative number or NIL."))
  (let ((deadline (%notification-timeout-deadline timeout)))
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%require-open-connection connection)
      (loop
        (let ((notification (%poll-notification-locked connection)))
          (when notification
            (return notification)))
        (when (connection--pending-backend-messages connection)
          (return nil))
        (let ((remaining (%notification-timeout-remaining deadline)))
          (when (and remaining (<= remaining 0))
            (return nil))
          (unless (transport-wait-readable
                   (connection-transport connection)
                   remaining)
            (return nil)))))))

(defun result-row (result index)
  (aref (query-result-rows result) index))

(defun %result-column-index (result column)
  (if (integerp column)
      column
      (or (position column (query-result-columns result)
                    :key #'column-name :test #'string-equal)
          (error 'parameter-error :parameter column
                 :message "Unknown result column."))))

(defun result-column (result column)
  (let ((index (%result-column-index result column)))
    (coerce (loop for row across (query-result-rows result)
                  collect (aref row index)) 'vector)))

(defun result-row-alist (result index)
  (let ((row (result-row result index)))
    (loop for column across (query-result-columns result)
          for value across row
          collect (cons (column-name column) value))))

(defun result-rows-as-alists (result)
  (loop for index below (query-result-row-count result)
        collect (result-row-alist result index)))

(defun result-columns (result)
  (query-result-columns result))

(defun result-rows (result)
  (query-result-rows result))

(defun result-command-tag (result)
  (query-result-command-tag result))

(defun result-row-count (result)
  (query-result-row-count result))

(defun result-transaction-status (result)
  (query-result-transaction-status result))

(defun result-notices (result)
  (query-result-notices result))

(defun result-portal-suspended-p (result)
  (query-result-portal-suspended-p result))

(defun result-row-alists (result)
  (result-rows-as-alists result))

(defun flush (connection)
  "Send a PostgreSQL protocol Flush message for CONNECTION.

This makes already-sent frontend messages available to the server without
waiting for an extended-query Sync boundary."
  (check-type connection connection)
  (cl-concurrent-kit:with-lock-held ((connection--lock connection))
    (%require-open-connection connection)
    (%send-frontend-message connection (encode-flush-message))
    t))

(defun row-value (result row-index column)
  "Return one value from RESULT's ROW-INDEX and COLUMN.

COLUMN may be a zero-based integer or a column name string."
  (aref (result-row result row-index)
        (%result-column-index result column)))

(defun %decode-query-row (connection columns raw-row)
  (unless (= (length columns) (length raw-row))
    (error 'protocol-error
           :message "PostgreSQL DataRow column count does not match RowDescription."
           :expected (length columns)
           :actual (length raw-row)))
  (let ((decoded (make-array (length raw-row))))
    (loop for index below (length raw-row)
          for column = (and (< index (length columns)) (aref columns index))
          for cell = (aref raw-row index)
          do (setf (aref decoded index)
                   (if (sql-null-p cell)
                       +sql-null+
                       (decode-value (connection-type-registry connection)
                                     (column-type-oid column)
                                     cell
                                     :format (column-format-code column)))))
    decoded))
