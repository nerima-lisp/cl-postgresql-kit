(in-package #:cl-postgresql-kit)

(defclass %base-backup-result-state ()
  ((columns :initform #() :accessor %base-backup-result-state-columns)
   (raw-rows :initform nil :accessor %base-backup-result-state-raw-rows)
   (command-tag :initform nil :accessor %base-backup-result-state-command-tag)
   (portal-suspended-p
    :initform nil :accessor %base-backup-result-state-portal-suspended-p)
   (notices :initform nil :accessor %base-backup-result-state-notices)
   (active-p :initform nil :accessor %base-backup-result-state-active-p)))

(defclass %base-backup-operation (%copy-operation)
  ((start-results :initarg :start-results :reader base-backup-start-results)
   (final-results :initform nil :accessor base-backup-final-results)
   (phase :initform :copy :accessor %base-backup-phase)
   (result-state
    :initform (make-instance '%base-backup-result-state)
    :reader %base-backup-result-state)
   (results :initform nil :accessor %base-backup-results)))

(defun %make-base-backup-query-result
    (connection columns raw-rows command-tag portal-suspended-p notices)
  (let ((decoded-rows
          (coerce
           (loop for raw-row in (nreverse raw-rows)
                 collect (%decode-query-row connection columns raw-row))
           'vector)))
    (%make-query-result
     :columns columns
     :rows decoded-rows
     :command-tag command-tag
     :row-count (length decoded-rows)
     :transaction-status (connection-transaction-status connection)
     :notices (%query-notice-list notices)
     :portal-suspended-p portal-suspended-p)))

(defun %base-backup-activate-result-state (state)
  (setf (%base-backup-result-state-active-p state) t)
  state)

(defun %base-backup-push-notice (state notice)
  (push notice (%base-backup-result-state-notices state))
  state)

(defun %base-backup-set-result-columns (state columns)
  (setf (%base-backup-result-state-columns state) columns)
  (%base-backup-activate-result-state state))

(defun %base-backup-push-result-row (state raw-row)
  (push raw-row (%base-backup-result-state-raw-rows state))
  (%base-backup-activate-result-state state))

(defun %base-backup-set-result-command-tag (state command-tag)
  (setf (%base-backup-result-state-command-tag state) command-tag)
  (%base-backup-activate-result-state state))

(defun %base-backup-set-result-portal-suspended (state)
  (setf (%base-backup-result-state-portal-suspended-p state) t)
  (%base-backup-activate-result-state state))

(defun %base-backup-finish-result-state (connection state)
  (when (%base-backup-result-state-active-p state)
    (prog1
        (%make-base-backup-query-result
         connection
         (%base-backup-result-state-columns state)
         (%base-backup-result-state-raw-rows state)
         (%base-backup-result-state-command-tag state)
         (%base-backup-result-state-portal-suspended-p state)
         (%base-backup-result-state-notices state))
      (setf (%base-backup-result-state-columns state) #()
            (%base-backup-result-state-raw-rows state) nil
            (%base-backup-result-state-command-tag state) nil
            (%base-backup-result-state-portal-suspended-p state) nil
            (%base-backup-result-state-notices state) nil
            (%base-backup-result-state-active-p state) nil))))

(defun %base-backup-finish-current-result (operation)
  (let* ((connection (%copy-connection operation))
         (result
           (%base-backup-finish-result-state
            connection
            (%base-backup-result-state operation))))
    (when result
      (push result (%base-backup-results operation))
      (setf (connection-last-result connection) result)
      result)))

(defmacro %base-backup-handle-query-result-message
    ((kind message state notice-value) &body finish-forms)
  `(case ,kind
     (:row-description
      (%base-backup-set-result-columns
       ,state
       (parse-row-description
        (backend-message-payload ,message))))
     (:data-row
      (%base-backup-push-result-row
       ,state
       (parse-data-row
        (backend-message-payload ,message))))
     (:command-complete
      (%base-backup-set-result-command-tag
       ,state
       (parse-command-complete
        (backend-message-payload ,message)))
      ,@finish-forms)
     (:empty-query-response
      (%base-backup-activate-result-state ,state)
      ,@finish-forms)
     (:portal-suspended
      (%base-backup-set-result-portal-suspended ,state))
     (:notice-response
      (%base-backup-push-notice ,state ,notice-value))))

(defun %base-backup-copy-operation
    (connection message start-results)
  (make-instance
   '%base-backup-operation
   :connection connection
   :kind :copy-out-response
   :response
   (parse-copy-response
    (backend-message-payload message))
   :start-results (nreverse start-results)))

(defun %base-backup-ready-or-start-error (connection)
  (let ((condition (%copy-pending-error connection)))
    (if condition
        (error condition)
        (%copy-protocol-error
         "The PostgreSQL server did not start BASE_BACKUP."))))

(defun %base-backup-require-between-copy (operation)
  (unless (eq (%base-backup-phase operation) :between-copy)
    (%copy-protocol-error
     "The PostgreSQL server ended BASE_BACKUP without CopyDone.")))

(defun %base-backup-finish-operation (operation)
  (let ((connection (%copy-connection operation)))
    (%base-backup-require-between-copy operation)
    (%base-backup-finish-current-result operation)
    (setf (base-backup-final-results operation)
          (nreverse (%base-backup-results operation))
          (%copy-state operation) :finished
          (connection--active-copy connection) nil)
    (let ((condition (%copy-pending-error connection)))
      (when condition
        (error condition)))
    nil))
