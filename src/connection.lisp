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

(defun %execute-identifier-command (connection command identifier)
  (query connection
         (format nil "~A ~A"
                 command
                 (%quote-identifier (%validate-notify-channel identifier)))))

(defmacro %define-identifier-command (name command documentation)
  `(defun ,name (connection identifier)
     ,documentation
     (%execute-identifier-command connection ,command identifier)))

(%define-identifier-command
 listen
 "LISTEN"
 "Register CONNECTION to receive notifications on IDENTIFIER.")

(%define-identifier-command
 unlisten
 "UNLISTEN"
 "Remove CONNECTION from the notification channel IDENTIFIER.")

(defun unlisten-all (connection)
  "Remove CONNECTION from all notification channels."
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

(defun flush (connection)
  "Send a PostgreSQL protocol Flush message for CONNECTION.

This makes already-sent frontend messages available to the server without
waiting for an extended-query Sync boundary."
  (check-type connection connection)
  (cl-concurrent-kit:with-lock-held ((connection--lock connection))
    (%require-open-connection connection)
    (%send-frontend-message connection (encode-flush-message))
    t))
