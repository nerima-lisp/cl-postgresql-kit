(in-package #:cl-postgresql-kit)

(defun %copy-read-until-ready (connection)
  (let ((command-tag nil))
    (%copy-read-message-loop (connection message kind)
      (:command-complete
       (setf command-tag
             (parse-command-complete
              (backend-message-payload message))))
      (:ready-for-query
       (return (values command-tag (%copy-pending-error connection)))))))

(defun copy-in-abort (operation &optional (message "COPY cancelled by client"))
  "Cancel an active COPY FROM STDIN operation and keep its connection usable.

MESSAGE is sent in the frontend CopyFail message.  PostgreSQL responds with an
  ErrorResponse followed by ReadyForQuery; that expected server error is
  consumed and is not re-signaled to the caller.  A transport or protocol error
  retires the connection, just like the other COPY lifecycle operations."
  (check-type message string)
  (%with-copy-operation-lock (operation :copy-in-response connection)
    (%copy-abort-unless-completed (operation completed-p)
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
        t))))

(defun %copy-both-drain-to-ready (operation)
  (let ((command-tag (%copy-command-tag operation)))
    (%copy-read-message-loop ((%copy-connection operation) message kind)
      (:copy-data nil)
      (:copy-done
       (setf (%copy-server-done-p operation) t))
      (:command-complete
       (setf command-tag
             (parse-command-complete
              (backend-message-payload message))
             (%copy-command-tag operation) command-tag))
      (:ready-for-query
       (unless (%copy-server-done-p operation)
         (%copy-protocol-error
          "The PostgreSQL server ended COPY BOTH without CopyDone."))
       (return (values command-tag (%copy-pending-error
                                     (%copy-connection operation))))))))

(defun copy-both-read (operation)
  "Read the next server-to-client COPY BOTH chunk, or NIL after CopyDone.

The operation remains open after NIL so that the client can send its own
CopyDone with COPY-BOTH-FINISH."
  (%with-copy-operation-lock (operation :copy-both-response connection)
    (when (%copy-server-done-p operation)
      (return-from copy-both-read nil))
    (let ((returned-data-p nil)
          (completed-p nil))
      (unwind-protect
           (%copy-read-message-loop (connection message kind)
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
                    (%copy-finish-operation operation :command-tag command-tag)
                    (setf completed-p t)
                    (when condition
                      (error condition))
                    (return nil))
                  (progn
                    (setf completed-p t)
                    (return nil))))
             (:ready-for-query
              (%copy-ready-or-pending-error
               connection
               "The PostgreSQL server ended COPY BOTH without CopyDone.")))
        (unless (or returned-data-p
                    (%copy-server-done-p operation)
                    completed-p)
          (%copy-abort operation))))))

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
  (%with-copy-operation-lock (operation :copy-both-response connection)
    (%copy-abort-unless-completed (operation completed-p)
      (unless (%copy-client-done-p operation)
        (%send-frontend-message connection
                                (encode-copy-done-message))
        (setf (%copy-client-done-p operation) t))
      (multiple-value-bind (command-tag condition)
          (%copy-both-drain-to-ready operation)
        (%copy-finish-operation operation :command-tag command-tag)
        (setf completed-p t)
        (when condition
          (error condition))
        command-tag))))

(defun copy-in-finish (operation)
  "Finish an active COPY FROM STDIN operation and return its command tag."
  (%with-copy-operation-lock (operation :copy-in-response connection)
    (%copy-abort-unless-completed (operation completed-p)
      (%send-frontend-message connection (encode-copy-done-message))
      (multiple-value-bind (command-tag condition)
          (%copy-read-until-ready connection)
        (%copy-finish-operation operation)
        (setf completed-p t)
        (when condition
          (error condition))
        command-tag))))

(defun copy-out-read (operation)
  "Read the next COPY TO STDOUT chunk, or NIL after the stream is complete."
  (%with-copy-operation-lock (operation :copy-out-response connection)
    (let ((returned-data-p nil)
          (completed-p nil))
      (unwind-protect
           (%copy-read-message-loop (connection message kind)
             (:copy-data
              (let ((data (parse-copy-data
                           (backend-message-payload message))))
                (setf returned-data-p t)
                (return data)))
             (:copy-done
              (multiple-value-bind (command-tag condition)
                  (%copy-read-until-ready connection)
                (declare (ignore command-tag))
                (%copy-finish-operation operation)
                (setf completed-p t)
                (when condition
                  (error condition))
                (return nil)))
             (:ready-for-query
              (%copy-ready-or-pending-error
               connection
               "The PostgreSQL server ended COPY without CopyDone.")))
        (unless (or returned-data-p completed-p)
          (%copy-abort operation))))))

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
