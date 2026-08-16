(in-package #:cl-postgresql-kit)

(defun %make-cancel-transport (connection)
  (or (when (connection-cancel-transport-factory connection)
        (funcall (connection-cancel-transport-factory connection) connection))
      (when (typep (connection-transport connection) 'socket-transport)
        (make-socket-transport :host (or (connection-hostaddr connection)
                                         (connection-host connection))
                               :port (connection-port connection)
                               :timeout (connection-connect-timeout connection)))))

(defun %cancel-request (connection &optional transport)
  (unless (and (connection-open connection)
               (connection-backend-process-id connection)
               (connection-backend-secret-key connection))
    (return-from %cancel-request nil))
  (let ((cancel-transport (or transport (%make-cancel-transport connection))))
    (unless cancel-transport
      (return-from %cancel-request nil))
    (unwind-protect
         (progn
           (transport-open cancel-transport)
           (transport-write-all
            cancel-transport
            (encode-cancel-request (connection-backend-process-id connection)
                                   (connection-backend-secret-key connection)))
           (transport-flush cancel-transport)
           t)
      (ignore-errors (transport-close cancel-transport)))))

(defun cancel-request (connection &key transport)
  "Ask the backend to cancel the currently running query.

TRANSPORT, when supplied, is opened and closed by this call.  A socket
connection uses a short-lived control connection automatically; custom
transports should provide CONNECTION-CANCEL-TRANSPORT-FACTORY.  This control
request deliberately does not acquire the connection's query lock, so it can
be issued by another thread while the connection is blocked reading a query."
  (check-type connection connection)
  (%cancel-request connection transport))

(defun %effective-query-timeout
       (connection &optional (remaining (cl-resilience-kit:deadline-remaining)))
  "Return the positive query timeout capped by the active deadline.

  A configured timeout of NIL or zero retains its existing meaning: no client
  query timeout.  An active resilience deadline still applies in that case."
  (let* ((configured (connection-query-timeout connection))
         (query-timeout (when (and configured (plusp configured))
                          configured)))
    (cond
      ((and query-timeout remaining) (min query-timeout remaining))
      (query-timeout query-timeout)
      (remaining remaining)
      (t nil))))

(defun %signal-query-timeout (connection)
  (let ((cancel-sent (ignore-errors (%cancel-request connection)))
        (connection-retired-p (%retire-connection connection)))
    (error 'timeout-error
           :message "The PostgreSQL query timed out; the connection was retired."
           :cancel-sent-p cancel-sent
           :connection-retired-p connection-retired-p)))

(defun %call-with-query-timeout (connection thunk)
  (let* ((remaining (cl-resilience-kit:deadline-remaining))
         (timeout (%effective-query-timeout connection remaining)))
    (handler-case
        (cond
          ((and remaining (not (plusp remaining)))
           (%signal-query-timeout connection))
          ((and timeout (plusp timeout))
           (cl-concurrent-kit:with-timeout
               (cl-date-kit:duration-of-nanos
                (round (* timeout 1000000000)))
             (funcall thunk)))
          (t (funcall thunk)))
      (cl-concurrent-kit:operation-timed-out ()
        (%signal-query-timeout connection)))))
