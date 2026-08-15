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

(defun %copy-finish-operation (operation &key command-tag)
  (let ((connection (%copy-connection operation)))
    (when command-tag
      (setf (%copy-command-tag operation) command-tag))
    (setf (%copy-state operation) :finished
          (connection--active-copy connection) nil)
    command-tag))

(defun %copy-ready-or-pending-error (connection protocol-message)
  (let ((condition (%copy-pending-error connection)))
    (if condition
        (error condition)
        (%copy-protocol-error protocol-message))))

(defmacro %copy-handle-backend-message ((connection message kind) &body clauses)
  `(multiple-value-bind (,kind ignored)
       (%process-backend-message ,connection ,message)
     (declare (ignore ignored))
     (case ,kind
       (:copy-fail
        (%copy-protocol-error
         (%copy-failure-message (backend-message-payload ,message))))
       (:error-response nil)
       ,@clauses
       (otherwise nil))))

(defmacro %copy-read-message-loop ((connection message kind) &body clauses)
  `(loop for ,message = (%read-backend-message ,connection)
         do (%copy-handle-backend-message (,connection ,message ,kind)
              ,@clauses)))

(defmacro %with-copy-operation-lock ((operation kind connection-var) &body body)
  `(progn
     (check-type ,operation %copy-operation)
     (let ((,connection-var (%copy-connection ,operation)))
       (cl-concurrent-kit:with-lock-held ((connection--lock ,connection-var))
         (%copy-check-operation ,operation ,kind)
         ,@body))))

(defmacro %copy-abort-unless-completed ((operation completed-p) &body body)
  `(let ((,completed-p nil))
     (unwind-protect
          (progn
            ,@body)
       (unless ,completed-p
         (%copy-abort ,operation)))))

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
      (%copy-read-message-loop (connection message kind)
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
        (:ready-for-query
         (let ((condition (%copy-pending-error connection)))
           (if condition
               (error condition)
               (%copy-protocol-error
                "The PostgreSQL server did not start a COPY operation."))))))))

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

(defun %copy-write-data (operation kind data &key client-done-message)
  (check-type operation %copy-operation)
  (let ((connection (%copy-connection operation)))
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%copy-check-operation operation kind)
      (when (and client-done-message
                 (%copy-client-done-p operation))
        (error 'copy-error :message client-done-message))
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

(defun copy-both-write (operation data)
  "Send one DATA chunk to an active COPY BOTH operation."
  (%copy-write-data operation :copy-both-response data
                    :client-done-message
                    "COPY BOTH input has already been finished."))

(defun copy-in-write (operation data)
  "Send one DATA chunk to an active COPY FROM STDIN operation."
  (%copy-write-data operation :copy-in-response data))

(defmacro %define-copy-encoded-writer
    (name writer lambda-list docstring encoder-form)
  `(defun ,name ,lambda-list
     ,docstring
     (,writer operation ,encoder-form)))

(%define-copy-encoded-writer
 copy-in-write-row
 copy-in-write
 (operation values column-type-oids
  &key (type-registry (default-type-registry))
    (format :text)
    (delimiter #\Tab)
    (null-string "\\N"))
 "Encode and send one logical row to an active COPY FROM STDIN operation.

TEXT rows are sent as one data chunk and include their terminating newline.
BINARY rows are sent as one data chunk containing only the row payload."
 (encode-copy-row values column-type-oids
                  :type-registry type-registry
                  :format format
                  :delimiter delimiter
                  :null-string null-string))

(%define-copy-encoded-writer
 copy-both-write-row
 copy-both-write
 (operation values column-type-oids
  &key (type-registry (default-type-registry))
    (format :text)
    (delimiter #\Tab)
    (null-string "\\N"))
 "Encode and send one logical row to an active COPY BOTH operation."
 (encode-copy-row values column-type-oids
                  :type-registry type-registry
                  :format format
                  :delimiter delimiter
                  :null-string null-string))

(%define-copy-encoded-writer
 copy-in-write-text-stream
 copy-in-write
 (operation rows column-type-oids
  &key
    (type-registry (default-type-registry))
    (delimiter #\Tab)
    (null-string "\\N"))
 "Encode and send a complete text COPY stream to COPY FROM STDIN.

The encoded stream is sent as one COPY data chunk.  Use COPY-IN-WRITE for
large streams that must be produced incrementally."
 (encode-copy-text-stream rows column-type-oids
                          :type-registry type-registry
                          :delimiter delimiter
                          :null-string null-string))

(%define-copy-encoded-writer
 copy-both-write-text-stream
 copy-both-write
 (operation rows column-type-oids
  &key
    (type-registry (default-type-registry))
    (delimiter #\Tab)
    (null-string "\\N"))
 "Encode and send a complete text COPY stream to COPY BOTH."
 (encode-copy-text-stream rows column-type-oids
                          :type-registry type-registry
                          :delimiter delimiter
                          :null-string null-string))

(%define-copy-encoded-writer
 copy-in-write-binary-stream
 copy-in-write
 (operation rows column-type-oids
  &key
    (type-registry (default-type-registry))
    (flags 0)
    (extension #()))
 "Encode and send a complete binary COPY stream to COPY FROM STDIN.

The encoded stream is sent as one COPY data chunk.  Use COPY-IN-WRITE for
large streams that must be produced incrementally."
 (encode-copy-binary-stream rows column-type-oids
                            :type-registry type-registry
                            :flags flags
                            :extension extension))

(%define-copy-encoded-writer
 copy-both-write-binary-stream
 copy-both-write
 (operation rows column-type-oids
  &key
    (type-registry (default-type-registry))
    (flags 0)
    (extension #()))
 "Encode and send a complete binary COPY stream to COPY BOTH."
 (encode-copy-binary-stream rows column-type-oids
                            :type-registry type-registry
                            :flags flags
                            :extension extension))
