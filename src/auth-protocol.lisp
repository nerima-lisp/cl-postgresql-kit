(in-package #:cl-postgresql-kit)

(defun %send-frontend-message (connection message &key (flush-p t))
  (unless (member (connection-state connection)
                  '(:connecting :ready :in-transaction :failed-transaction))
    (error 'connection-error :message "The PostgreSQL connection is not usable."))
  (%connection-log connection "Sending PostgreSQL frontend message"
                   :type (and (plusp (length message))
                               (code-char (aref message 0))))
  (transport-write-all (connection-transport connection)
                       message)
  (when flush-p
    (transport-flush (connection-transport connection))))

(defun %connection-buffer-append (connection octets)
  (loop for octet across octets
        do (vector-push-extend octet (connection--read-buffer connection)))
  connection)

(defun %connection-buffer-take (connection octet-count)
  (let* ((buffer (connection--read-buffer connection))
         (available (fill-pointer buffer))
         (from-buffer (min available octet-count))
         (result (make-array octet-count :element-type '(unsigned-byte 8))))
    (replace result buffer :end2 from-buffer)
    (when (plusp from-buffer)
      (let ((remaining (subseq buffer from-buffer available)))
        (replace buffer remaining)
        (setf (fill-pointer buffer) (length remaining))))
    (when (< from-buffer octet-count)
      (replace result
               (transport-read-exactly
                (connection-transport connection)
                (- octet-count from-buffer))
               :start1 from-buffer))
    result))

(defun %connection-fill-read-buffer (connection)
  (let ((available (transport-read-available (connection-transport connection))))
    (when (plusp (length available))
      (%connection-buffer-append connection available))
    (length available)))

(defun %connection-buffered-backend-message (connection)
  (let* ((buffer (connection--read-buffer connection))
         (available (fill-pointer buffer)))
    (when (>= available 5)
      (multiple-value-bind (length ignored) (%read-u32 buffer 1)
        (declare (ignore ignored))
        (when (< length 4)
          (error 'protocol-error :message "Invalid PostgreSQL backend message length"
                 :context :wire :expected ">= 4" :actual length))
        (when (> length *maximum-frame-size*)
          (error 'protocol-error
                 :message "PostgreSQL backend message exceeds configured limit"
                 :context :wire :expected *maximum-frame-size* :actual length))
        (let ((frame-size (1+ length)))
          (when (<= frame-size available)
            (let ((message (parse-frame (subseq buffer 0 frame-size))))
              (%connection-buffer-take connection frame-size)
              message)))))))

(defun %read-backend-message (connection)
  (or (pop (connection--pending-backend-messages connection))
      (%connection-buffered-backend-message connection)
      (let ((type (aref (%connection-buffer-take connection 1) 0))
            (header (%connection-buffer-take connection 4)))
        (multiple-value-bind (length ignored) (%read-u32 header 0)
          (declare (ignore ignored))
          (when (< length 4)
            (error 'protocol-error
                   :message "Invalid PostgreSQL backend message length"
                   :context :wire :expected ">= 4" :actual length))
          (when (> length *maximum-frame-size*)
            (error 'protocol-error
                   :message "PostgreSQL backend message exceeds configured limit"
                   :context :wire :expected *maximum-frame-size* :actual length))
          (%make-backend-message
           type
           (%connection-buffer-take connection (- length 4)))))))

(defun %signal-server-fields (fields)
  (error (%server-error-from-fields fields)))

(defun %record-notification (connection value)
  (cl-concurrent-kit:with-lock-held ((connection--notifications-lock connection))
    (let ((limit (connection-max-notifications connection)))
      (if (zerop limit)
          (setf (connection--notifications connection) nil)
          (progn
            (push value (connection--notifications connection))
            (when (> (length (connection--notifications connection)) limit)
              (setf (cdr (nthcdr (1- limit)
                                 (connection--notifications connection)))
                    nil))))))
  value)

(defun %handle-notice-fields (connection fields)
  (let ((notice (make-instance 'notice
                               :message (or (%field fields :message) "")
                               :fields fields)))
    (%record-notification connection notice)
    (when (connection-notice-handler connection)
      (funcall (connection-notice-handler connection) connection notice))
    notice))

(defun %handle-notification (connection payload)
  (let* ((notification (parse-notification-response payload))
         (process-id (getf notification :pid))
         (channel (getf notification :channel))
         (payload-text (getf notification :payload)))
    (%record-notification connection notification)
    (when (connection-notification-handler connection)
      (funcall (connection-notification-handler connection)
               connection process-id channel payload-text))
    notification))

(defun %unsupported-backend-message (kind)
  (error 'protocol-error
         :message (format nil "Unsupported PostgreSQL backend message kind: ~S" kind)))

(defun %process-backend-message (connection message)
  (let ((kind (backend-message-kind (backend-message-type message))))
    (case kind
      (:parameter-status
       (let ((parameter (parse-parameter-status (backend-message-payload message))))
         (setf (gethash (car parameter)
                        (connection-parameters connection))
               (cdr parameter))
         (values kind parameter)))
      (:backend-key-data
       (multiple-value-bind (process-id secret-key)
           (parse-backend-key-data
            (backend-message-payload message)
            :protocol-version
            (or (connection-negotiated-protocol-version connection)
                (connection-protocol-version connection)))
         (setf (connection-backend-process-id connection) process-id
               (connection-backend-secret-key connection) secret-key)
         (values kind (list :process-id process-id :secret-key secret-key))))
      (:negotiate-protocol-version
       (let* ((negotiation
                (parse-negotiate-protocol-version
                 (backend-message-payload message)))
              (minor (getf negotiation :newest-minor-version)))
         (setf (connection-negotiated-protocol-version connection)
               (if (>= minor 2)
                   +protocol-version-3.2+
                   +protocol-version-3.0+))
         (values kind negotiation)))
      (:ready-for-query
       (let ((status (parse-ready-for-query (backend-message-payload message))))
         (setf (connection-transaction-status connection)
               (ecase status
                 (#\I :idle)
                 (#\T :in-transaction)
                 (#\E :failed-transaction)))
         (values kind status)))
      (:notice-response
       (values kind (%handle-notice-fields
                     connection (parse-error-response
                                 (backend-message-payload message)))))
      (:notification-response
       (values kind (%handle-notification connection
                                          (backend-message-payload message))))
      (:error-response
       (let ((condition (%server-error-from-fields
                         (parse-error-response
                          (backend-message-payload message)))))
         (setf (connection--pending-error connection) condition)
         (values kind condition)))
      ((:authentication :row-description :data-row :command-complete
        :copy-in-response :copy-out-response :copy-both-response
        :copy-data :copy-done :copy-fail :empty-query-response
        :parse-complete :bind-complete :close-complete :no-data
        :portal-suspended :parameter-description :function-call-response)
       (values kind nil))
      (otherwise (%unsupported-backend-message kind)))))

(defun %read-startup-response (connection)
  (loop for message = (%read-backend-message connection)
        do (multiple-value-bind (kind value)
               (%process-backend-message connection message)
             (case kind
               (:authentication (return (values kind
                                               (parse-authentication
                                                (backend-message-payload message)))))
               (:error-response (%signal-server-fields
                                 (server-error-fields value)))
               (otherwise (return (values kind value)))))))
