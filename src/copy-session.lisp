(in-package #:cl-postgresql-kit)

(defclass %copy-operation ()
  ((connection :initarg :connection :reader %copy-connection)
   (kind :initarg :kind :reader %copy-kind)
   (response :initarg :response :reader %copy-response)
   (state :initform :open :accessor %copy-state)
   (server-done-p :initform nil :accessor %copy-server-done-p)
   (client-done-p :initform nil :accessor %copy-client-done-p)
   (command-tag :initform nil :accessor %copy-command-tag)))

(defun %copy-pending-error (connection)
  (prog1 (connection--pending-error connection)
    (setf (connection--pending-error connection) nil)))

(defun %copy-failure-message (payload)
  (handler-case
      (multiple-value-bind (message ignored)
          (%read-cstring payload 0)
        (declare (ignore ignored))
        message)
    (error () "The PostgreSQL server reported a COPY failure.")))

(defun %copy-protocol-error (message)
  (error 'copy-error :message message))

(defun %copy-start (connection sql expected-kind)
  (check-type connection connection)
  (check-type sql string)
  (%with-exchange-failure-retirement (connection)
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%require-open-connection connection)
      (%require-no-active-copy connection)
      (%require-no-active-cursors connection)
      (setf (connection--pending-error connection) nil)
      (%send-frontend-message connection (encode-query-message sql))
      (loop for message = (%read-backend-message connection)
            do (multiple-value-bind (kind ignored)
                   (%process-backend-message connection message)
                 (declare (ignore ignored))
                 (case kind
                   ((:copy-in-response :copy-out-response :copy-both-response)
                    (if (eq kind expected-kind)
                        (let ((operation
                                (make-instance '%copy-operation
                                               :connection connection
                                               :kind kind
                                               :response
                                               (parse-copy-response
                                                (backend-message-payload message)))))
                          (setf (connection--active-copy connection) operation)
                          (return operation))
                        (%copy-protocol-error
                         "The PostgreSQL server returned the wrong COPY direction.")))
                   (:error-response nil)
                   (:ready-for-query
                    (let ((condition (%copy-pending-error connection)))
                      (if condition
                          (error condition)
                          (%copy-protocol-error
                           "The PostgreSQL server did not start a COPY operation."))))
                   (otherwise nil)))))))

(defun copy-in-start (connection sql)
  "Start a COPY FROM STDIN operation and return an opaque COPY handle."
  (%copy-start connection sql :copy-in-response))

(defun copy-out-start (connection sql)
  "Start a COPY TO STDOUT operation and return an opaque COPY handle."
  (%copy-start connection sql :copy-out-response))

(defun copy-both-start (connection sql)
  "Start a bidirectional COPY operation and return an opaque COPY handle."
  (%copy-start connection sql :copy-both-response))

(defun copy-in (connection sql)
  "Start a COPY FROM STDIN operation and return an opaque COPY handle."
  (copy-in-start connection sql))

(defun copy-out (connection sql)
  "Start a COPY TO STDOUT operation and return an opaque COPY handle."
  (copy-out-start connection sql))

(defun copy-both-write (operation data)
  "Send one DATA chunk to an active COPY BOTH operation."
  (check-type operation %copy-operation)
  (let ((connection (%copy-connection operation)))
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%copy-check-operation operation :copy-both-response)
      (when (%copy-client-done-p operation)
        (error 'copy-error :message "COPY BOTH input has already been finished."))
      (handler-case
          (progn
            (%send-frontend-message connection
                                    (encode-copy-data-message data))
            t)
        (error (condition)
          (%copy-abort operation)
          (error condition))))))

(defun %copy-check-operation (operation kind)
  (unless (and (typep operation '%copy-operation)
               (eq (%copy-kind operation) kind)
               (eq (%copy-state operation) :open))
    (error 'copy-error :message "The COPY handle is not active."))
  (let ((connection (%copy-connection operation)))
    (unless (eq (connection--active-copy connection) operation)
      (error 'copy-error :message "The COPY handle is not active on its connection."))
    connection))

(defun %copy-abort (operation)
  (let ((connection (%copy-connection operation)))
    (setf (%copy-state operation) :failed)
    (when (eq (connection--active-copy connection) operation)
      (setf (connection--active-copy connection) nil))
    (%retire-connection connection)))

(defun copy-in-write (operation data)
  "Send one DATA chunk to an active COPY FROM STDIN operation."
  (check-type operation %copy-operation)
  (let ((connection (%copy-connection operation)))
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%copy-check-operation operation :copy-in-response)
      (handler-case
          (progn
            (%send-frontend-message connection
                                    (encode-copy-data-message data))
            t)
        (error (condition)
          (%copy-abort operation)
          (error condition))))))

(defun copy-in-write-row (operation values column-type-oids
                          &key (type-registry (default-type-registry))
                            (format :text)
                            (delimiter #\Tab)
                            (null-string "\\N"))
  "Encode and send one logical row to an active COPY FROM STDIN operation.

TEXT rows are sent as one data chunk and include their terminating newline.
BINARY rows are sent as one data chunk containing only the row payload."
  (copy-in-write
   operation
   (encode-copy-row values column-type-oids
                    :type-registry type-registry
                    :format format
                    :delimiter delimiter
                    :null-string null-string)))

(defun copy-both-write-row (operation values column-type-oids
                            &key (type-registry (default-type-registry))
                              (format :text)
                              (delimiter #\Tab)
                              (null-string "\\N"))
  "Encode and send one logical row to an active COPY BOTH operation."
  (copy-both-write
   operation
   (encode-copy-row values column-type-oids
                    :type-registry type-registry
                    :format format
                    :delimiter delimiter
                    :null-string null-string)))

(defun copy-in-write-text-stream (operation rows column-type-oids
                                  &key
                                    (type-registry (default-type-registry))
                                    (delimiter #\Tab)
                                    (null-string "\\N"))
  "Encode and send a complete text COPY stream to COPY FROM STDIN.

The encoded stream is sent as one COPY data chunk.  Use COPY-IN-WRITE for
large streams that must be produced incrementally."
  (copy-in-write
   operation
   (encode-copy-text-stream rows column-type-oids
                            :type-registry type-registry
                            :delimiter delimiter
                            :null-string null-string)))

(defun copy-both-write-text-stream (operation rows column-type-oids
                                    &key
                                      (type-registry (default-type-registry))
                                      (delimiter #\Tab)
                                      (null-string "\\N"))
  "Encode and send a complete text COPY stream to COPY BOTH."
  (copy-both-write
   operation
   (encode-copy-text-stream rows column-type-oids
                            :type-registry type-registry
                            :delimiter delimiter
                            :null-string null-string)))

(defun copy-in-write-binary-stream (operation rows column-type-oids
                                    &key
                                      (type-registry (default-type-registry))
                                      (flags 0)
                                      (extension #()))
  "Encode and send a complete binary COPY stream to COPY FROM STDIN.

The encoded stream is sent as one COPY data chunk.  Use COPY-IN-WRITE for
large streams that must be produced incrementally."
  (copy-in-write
   operation
   (encode-copy-binary-stream rows column-type-oids
                              :type-registry type-registry
                              :flags flags
                              :extension extension)))

(defun copy-both-write-binary-stream (operation rows column-type-oids
                                      &key
                                        (type-registry (default-type-registry))
                                        (flags 0)
                                        (extension #()))
  "Encode and send a complete binary COPY stream to COPY BOTH."
  (copy-both-write
   operation
   (encode-copy-binary-stream rows column-type-oids
                              :type-registry type-registry
                              :flags flags
                              :extension extension)))

(defun %copy-read-until-ready (connection)
  (let ((command-tag nil))
    (loop for message = (%read-backend-message connection)
          do (multiple-value-bind (kind ignored)
                 (%process-backend-message connection message)
               (declare (ignore ignored))
               (case kind
                 (:command-complete
                  (setf command-tag
                        (parse-command-complete
                         (backend-message-payload message))))
                 (:error-response nil)
                 (:copy-fail
                  (%copy-protocol-error
                   (%copy-failure-message (backend-message-payload message))))
                 (:ready-for-query
                  (return (values command-tag (%copy-pending-error connection))))
                 (otherwise nil))))))

(defun copy-in-abort (operation &optional (message "COPY cancelled by client"))
  "Cancel an active COPY FROM STDIN operation and keep its connection usable.

MESSAGE is sent in the frontend CopyFail message.  PostgreSQL responds with an
ErrorResponse followed by ReadyForQuery; that expected server error is
consumed and is not re-signaled to the caller.  A transport or protocol error
retires the connection, just like the other COPY lifecycle operations."
  (check-type operation %copy-operation)
  (check-type message string)
  (let ((connection (%copy-connection operation)))
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%copy-check-operation operation :copy-in-response)
      (let ((completed-p nil))
        (unwind-protect
             (progn
               (%send-frontend-message connection
                                       (encode-copy-fail-message message))
               (multiple-value-bind (ignored condition)
                   (%copy-read-until-ready connection)
                 (declare (ignore ignored))
                 (unless condition
                   (%copy-protocol-error
                    "The PostgreSQL server did not report COPY cancellation."))
                 (setf (%copy-state operation) :failed
                       (connection--active-copy connection) nil
                       (connection--pending-error connection) nil
                       completed-p t)
                 t))
          (unless completed-p
            (%copy-abort operation)))))))

(defun %copy-both-drain-to-ready (operation)
  (let ((command-tag (%copy-command-tag operation)))
    (loop for message = (%read-backend-message (%copy-connection operation))
          do (multiple-value-bind (kind ignored)
                 (%process-backend-message (%copy-connection operation) message)
               (declare (ignore ignored))
               (case kind
                 (:copy-data nil)
                 (:copy-done
                  (setf (%copy-server-done-p operation) t))
                 (:command-complete
                  (setf command-tag
                        (parse-command-complete
                         (backend-message-payload message))
                        (%copy-command-tag operation) command-tag))
                 (:copy-fail
                  (%copy-protocol-error
                   (%copy-failure-message (backend-message-payload message))))
                 (:error-response nil)
                 (:ready-for-query
                  (unless (%copy-server-done-p operation)
                    (%copy-protocol-error
                     "The PostgreSQL server ended COPY BOTH without CopyDone."))
                  (return (values command-tag (%copy-pending-error
                                                (%copy-connection operation))))
                  )
                 (otherwise nil))))))

(defun copy-both-read (operation)
  "Read the next server-to-client COPY BOTH chunk, or NIL after CopyDone.

The operation remains open after NIL so that the client can send its own
CopyDone with COPY-BOTH-FINISH."
  (check-type operation %copy-operation)
  (let ((connection (%copy-connection operation)))
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%copy-check-operation operation :copy-both-response)
      (when (%copy-server-done-p operation)
        (return-from copy-both-read nil))
      (let ((returned-data-p nil)
            (completed-p nil))
        (unwind-protect
             (loop for message = (%read-backend-message connection)
                   do (multiple-value-bind (kind ignored)
                          (%process-backend-message connection message)
                        (declare (ignore ignored))
                        (case kind
                          (:copy-data
                           (let ((data (parse-copy-data
                                        (backend-message-payload message))))
                             (setf returned-data-p t)
                             (return data)))
                          (:copy-done
                           (setf (%copy-server-done-p operation) t)
                           (if (%copy-client-done-p operation)
                               (multiple-value-bind (command-tag condition)
                                   (%copy-both-drain-to-ready operation)
                                 (setf (%copy-command-tag operation) command-tag
                                       (%copy-state operation) :finished
                                       (connection--active-copy connection) nil
                                       completed-p t)
                                 (when condition
                                   (error condition))
                                 (return nil))
                               (return nil)))
                          (:copy-fail
                           (%copy-protocol-error
                            (%copy-failure-message
                             (backend-message-payload message))))
                          (:error-response nil)
                          (:ready-for-query
                           (let ((condition (%copy-pending-error connection)))
                             (if condition
                                 (error condition)
                                 (%copy-protocol-error
                                  "The PostgreSQL server ended COPY BOTH without CopyDone."))))
                          (otherwise nil))))
          (unless (or returned-data-p
                      (%copy-server-done-p operation)
                      completed-p)
            (%copy-abort operation)))))))

(defun copy-both-read-all (operation)
  "Read and concatenate all server-to-client COPY BOTH data chunks.

The operation remains open after this function returns, so the caller must
still call COPY-BOTH-FINISH to send the client CopyDone message."
  (let ((builder (make-octet-builder)))
    (loop for data = (copy-both-read operation)
          while data
          do (append-octets builder data))
    (builder-octets builder)))

(defun %copy-decode-stream-rows (data column-type-oids type-registry format
                                 delimiter null-string)
  (if (zerop (%copy-row-format-code format))
      (decode-copy-text-stream data column-type-oids
                               :type-registry type-registry
                               :delimiter delimiter
                               :null-string null-string)
      (nth-value 0
                 (decode-copy-binary-stream data column-type-oids
                                            :type-registry type-registry))))

(defun copy-both-read-rows (operation column-type-oids
                            &key (type-registry (default-type-registry))
                              (format :text)
                              (delimiter #\Tab)
                              (null-string "\\N"))
  "Read and decode the complete server-to-client COPY BOTH stream.

The operation remains open after this function returns, so the caller must
still call COPY-BOTH-FINISH.  FORMAT may be :TEXT, :BINARY, 0, or 1."
  (%copy-decode-stream-rows
   (copy-both-read-all operation)
   column-type-oids type-registry format delimiter null-string))

(defun copy-both-finish (operation)
  "Finish COPY BOTH and return the server command tag.

Any unread server COPY data is drained while completing the protocol."
  (check-type operation %copy-operation)
  (let ((connection (%copy-connection operation)))
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%copy-check-operation operation :copy-both-response)
      (let ((completed-p nil))
        (unwind-protect
             (progn
               (unless (%copy-client-done-p operation)
                 (%send-frontend-message connection
                                         (encode-copy-done-message))
                 (setf (%copy-client-done-p operation) t))
               (multiple-value-bind (command-tag condition)
                   (%copy-both-drain-to-ready operation)
                 (setf (%copy-command-tag operation) command-tag
                       (%copy-state operation) :finished
                       (connection--active-copy connection) nil
                       completed-p t)
                 (when condition
                   (error condition))
                 command-tag))
          (unless completed-p
            (%copy-abort operation)))))))

(defun copy-in-finish (operation)
  "Finish an active COPY FROM STDIN operation and return its command tag."
  (check-type operation %copy-operation)
  (let ((connection (%copy-connection operation)))
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%copy-check-operation operation :copy-in-response)
      (let ((completed-p nil))
        (unwind-protect
             (progn
               (%send-frontend-message connection (encode-copy-done-message))
               (multiple-value-bind (command-tag condition)
                   (%copy-read-until-ready connection)
                 (setf (%copy-state operation) :finished
                       (connection--active-copy connection) nil
                       completed-p t)
                 (when condition
                   (error condition))
                 command-tag))
          (unless completed-p
            (%copy-abort operation)))))))

(defun copy-out-read (operation)
  "Read the next COPY TO STDOUT chunk, or NIL after the stream is complete."
  (check-type operation %copy-operation)
  (let ((connection (%copy-connection operation)))
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%copy-check-operation operation :copy-out-response)
      (let ((returned-data-p nil)
            (completed-p nil))
        (unwind-protect
             (loop for message = (%read-backend-message connection)
                   do (multiple-value-bind (kind ignored)
                          (%process-backend-message connection message)
                        (declare (ignore ignored))
                        (case kind
                          (:copy-data
                           (let ((data (parse-copy-data
                                        (backend-message-payload message))))
                             (setf returned-data-p t)
                             (return data)))
                          (:copy-done
                           (multiple-value-bind (command-tag condition)
                               (%copy-read-until-ready connection)
                             (declare (ignore command-tag))
                             (setf (%copy-state operation) :finished
                                   (connection--active-copy connection) nil
                                   completed-p t)
                             (when condition
                               (error condition))
                             (return nil)))
                          (:copy-fail
                           (%copy-protocol-error
                            (%copy-failure-message
                             (backend-message-payload message))))
                          (:error-response nil)
                          (:ready-for-query
                           (let ((condition (%copy-pending-error connection)))
                             (if condition
                                 (error condition)
                                 (%copy-protocol-error
                                  "The PostgreSQL server ended COPY without CopyDone."))))
                          (otherwise nil))))
          (unless (or returned-data-p completed-p)
            (%copy-abort operation)))))))

(defun copy-out-read-all (operation)
  "Read and concatenate all COPY TO STDOUT data chunks.

The returned octets preserve the server's COPY stream representation and can
be passed to DECODE-COPY-BINARY-STREAM when the COPY format is binary."
  (let ((builder (make-octet-builder)))
    (loop for data = (copy-out-read operation)
          while data
          do (append-octets builder data))
    (builder-octets builder)))

(defun copy-out-read-rows (operation column-type-oids
                           &key (type-registry (default-type-registry))
                             (format :text)
                             (delimiter #\Tab)
                             (null-string "\\N"))
  "Read and decode the complete server-to-client COPY TO STDOUT stream.

The operation is finished before this function returns.  FORMAT may be
:TEXT, :BINARY, 0, or 1."
  (%copy-decode-stream-rows
   (copy-out-read-all operation)
   column-type-oids type-registry format delimiter null-string))

(defun replication-start (connection start-replication-sql)
  "Start a physical or logical PostgreSQL replication stream.

START-REPLICATION-SQL is passed to PostgreSQL as-is and must be a valid
START_REPLICATION command.  The returned operation uses the COPY BOTH
lifecycle."
  (copy-both-start connection start-replication-sql))

(defun replication-read (operation)
  "Read and parse the next replication message, or NIL after CopyDone."
  (let ((payload (copy-both-read operation)))
    (and payload
         (handler-case
             (parse-replication-message payload)
           (error (condition)
             (%copy-abort operation)
             (error condition))))))

(defun replication-send-status (operation write-lsn flush-lsn apply-lsn
                                &key client-time (reply-requested nil))
  "Send a standby status update on an active replication stream."
  (copy-both-write
   operation
   (encode-standby-status-update
    write-lsn flush-lsn apply-lsn
    :client-time client-time
    :reply-requested reply-requested)))

(defun replication-send-hot-standby-feedback
    (operation xmin xmin-epoch catalog-xmin catalog-xmin-epoch
     &key client-time)
  "Send hot standby feedback on an active replication stream."
  (copy-both-write
   operation
   (encode-hot-standby-feedback
    xmin xmin-epoch catalog-xmin catalog-xmin-epoch
    :client-time client-time)))

(defun replication-finish (operation)
  "Finish an active replication stream and return its server command tag."
  (copy-both-finish operation))
