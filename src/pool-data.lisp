(in-package #:cl-postgresql-kit)

(defclass connection-pool ()
  ((max-size :initarg :max-size :reader pool-max-size)
   (min-size :initarg :min-size :reader pool-min-size)
   (max-idle-time :initarg :max-idle-time :reader pool-max-idle-time
                  :initform nil)
   (max-connection-age :initarg :max-connection-age
                       :reader pool-max-connection-age
                       :initform nil)
   (validation-query :initarg :validation-query
                     :reader pool-validation-query
                     :initform nil)
   (connection-factory :initarg :connection-factory
                       :reader pool-connection-factory)
   (available :initform nil :accessor %pool-available)
   (in-use :initform nil :accessor %pool-in-use)
   (created-at :initform (make-hash-table :test #'eq)
               :accessor %pool-created-at)
   (idle-since :initform (make-hash-table :test #'eq)
               :accessor %pool-idle-since)
   (creating-count :initform 0 :accessor %pool-creating-count)
   (resetting-count :initform 0 :accessor %pool-resetting-count)
   (lock :initform (cl-concurrent-kit:make-lock :name "postgresql-pool")
         :reader %pool-lock)
   (condition :initform
              (cl-concurrent-kit:make-condition-variable
               :name "postgresql-pool-availability")
              :reader %pool-condition)
   (closed-p :initform nil :accessor pool-closed-p)
   (logger :initarg :logger :reader %pool-logger :initform nil)))

(defun %pool-now ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun %pool-deadline (timeout)
  (and timeout (+ (%pool-now) timeout)))

(defun %pool-remaining (deadline)
  (and deadline (- deadline (%pool-now))))

(defun %pool-validate-nonnegative-real-option (value parameter message)
  (unless (or (null value)
              (and (realp value) (>= value 0)))
    (error 'parameter-error :parameter parameter
           :message message))
  value)

(defun %pool-validate-non-empty-string-option (value parameter message)
  (unless (or (null value)
              (and (stringp value)
                   (plusp (length value))))
    (error 'parameter-error :parameter parameter
           :message message))
  value)

(defun %pool-total-count (pool)
  (+ (length (%pool-available pool))
     (length (%pool-in-use pool))
     (%pool-creating-count pool)
     (%pool-resetting-count pool)))

(defun %pool-log (pool message &rest fields)
  (when (%pool-logger pool)
    (log-kit:emit-log (%pool-logger pool)
                      log-kit:+level-debug+
                      message
                      fields)))

(defun %pool-usable-p (connection)
  (and (connection-healthy-p connection)
       (not (eq (connection-transaction-status connection)
                :failed-transaction))))

(defun %pool-record-new-connection (pool connection)
  (setf (gethash connection (%pool-created-at pool)) (%pool-now))
  (remhash connection (%pool-idle-since pool))
  connection)

(defun %pool-record-idle-connection (pool connection)
  (setf (gethash connection (%pool-idle-since pool)) (%pool-now))
  connection)

(defun %pool-forget-connection (pool connection)
  (remhash connection (%pool-created-at pool))
  (remhash connection (%pool-idle-since pool))
  connection)

(defun %pool-expired-p (pool connection &optional (now (%pool-now)))
  (let ((created-at (gethash connection (%pool-created-at pool)))
        (idle-since (gethash connection (%pool-idle-since pool)))
        (max-age (pool-max-connection-age pool))
        (max-idle (pool-max-idle-time pool)))
    (or (and max-age created-at
             (>= (- now created-at) max-age))
        (and max-idle idle-since
             (>= (- now idle-since) max-idle)))))

(defun %pool-validate-connection (pool connection)
  (let ((validation-query (pool-validation-query pool)))
    (if (null validation-query)
        t
        (handler-case
            (progn
              (query connection validation-query)
              (%pool-usable-p connection))
          (error () nil)))))

(defun %pool-disconnect (connection)
  (ignore-errors (disconnect connection))
  nil)

(defun %pool-drop-in-use (pool connection)
  (setf (%pool-in-use pool)
        (delete connection (%pool-in-use pool) :test #'eq)))

(defun %pool-checkout-available-connection (pool)
  (let ((connection (pop (%pool-available pool))))
    (remhash connection (%pool-idle-since pool))
    (push connection (%pool-in-use pool))
    connection))

(defun %pool-reserve-create-slot (pool)
  (incf (%pool-creating-count pool))
  nil)

(defun %pool-release-create-reservation (pool)
  (decf (%pool-creating-count pool))
  (cl-concurrent-kit:condition-broadcast (%pool-condition pool)))

(defun %pool-adopt-created-connection (pool connection destination)
  (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
    (%pool-release-create-reservation pool)
    (unless (pool-closed-p pool)
      (%pool-record-new-connection pool connection)
      (ecase destination
        (:in-use
         (push connection (%pool-in-use pool)))
        (:idle
         (%pool-record-idle-connection pool connection)
         (push connection (%pool-available pool))))
      t)))

(defmacro %with-pool-created-connection
    ((connection-var keep-p-var pool destination) &body body)
  (let ((pool-var (gensym "POOL-"))
        (destination-var (gensym "DESTINATION-"))
        (reservation-released-p-var (gensym "RESERVATION-RELEASED-")))
    `(let* ((,pool-var ,pool)
            (,destination-var ,destination)
            (,connection-var nil)
            (,reservation-released-p-var nil)
            (,keep-p-var nil))
       (handler-case
           (progn
             (setf ,connection-var (%pool-new-connection ,pool-var))
             (setf ,keep-p-var
                   (%pool-adopt-created-connection
                    ,pool-var ,connection-var ,destination-var)
                   ,reservation-released-p-var t)
             (if ,keep-p-var
                 (progn
                   ,@body)
                 (progn
                   (%pool-disconnect ,connection-var)
                   nil)))
         (error (condition)
           (unless ,reservation-released-p-var
             (cl-concurrent-kit:with-lock-held ((%pool-lock ,pool-var))
               (%pool-release-create-reservation ,pool-var)))
           (when (and ,connection-var (not ,keep-p-var))
             (%pool-disconnect ,connection-var))
           (error condition))))))

(defun %pool-reset-connection (connection)
  "Return CONNECTION to a clean, idle session or report failure.

DISCARD ALL also invalidates server-side prepared statements, so the local
statement cache is cleared only after the reset completes successfully."
  (unless (connection-healthy-p connection)
    (return-from %pool-reset-connection nil))
  (handler-case
      (progn
        (when (member (connection-transaction-status connection)
                      '(:in-transaction :failed-transaction))
          (rollback-transaction connection))
        (unless (eq (connection-transaction-status connection) :idle)
          (return-from %pool-reset-connection nil))
        (query connection "DISCARD ALL")
        (clrhash (connection--prepared-statements connection))
        (setf (connection-last-result connection) nil)
        (%pool-usable-p connection))
    (error ()
      nil)))

(defun %pool-new-connection (pool)
  (let ((connection (funcall (pool-connection-factory pool))))
    (unless (typep connection 'connection)
      (error 'pool-error
             :message "The pool connection factory did not return a CONNECTION."))
    (handler-case
        (progn
          (unless (%pool-usable-p connection)
            (when (connection-open connection)
              (%pool-disconnect connection))
            (connect connection))
          (unless (%pool-usable-p connection)
            (error 'pool-error
                   :message "The pool connection factory returned an unusable connection."))
          connection)
      (error (condition)
        (%pool-disconnect connection)
        (error condition)))))

(defun %pool-remove-in-use (pool connection)
  (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
    (%pool-drop-in-use pool connection)
    (cl-concurrent-kit:condition-broadcast (%pool-condition pool)))
  t)

(defun %pool-retire-in-use (pool connection)
  (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
    (%pool-drop-in-use pool connection)
    (%pool-forget-connection pool connection)
    (cl-concurrent-kit:condition-broadcast (%pool-condition pool)))
  (%pool-disconnect connection))

(defun %pool-acquirable-p (pool connection)
  (and (not (%pool-expired-p pool connection))
       (%pool-usable-p connection)
       (%pool-validate-connection pool connection)))

(defun %pool-begin-reset (pool connection)
  (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
    (when (member connection (%pool-in-use pool) :test #'eq)
      (let ((closed-p (pool-closed-p pool)))
        (%pool-drop-in-use pool connection)
        (incf (%pool-resetting-count pool))
        (cl-concurrent-kit:condition-broadcast (%pool-condition pool))
        (values t closed-p)))))

(defun %pool-return-idle (pool connection)
  (%pool-record-idle-connection pool connection)
  (push connection (%pool-available pool))
  t)

(defun %pool-finish-reset (pool connection reset-p)
  (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
    (decf (%pool-resetting-count pool))
    (let ((keep-p nil))
      (unless (or (pool-closed-p pool)
                  (not reset-p)
                  (%pool-expired-p pool connection))
        (setf keep-p (%pool-return-idle pool connection)))
      (unless keep-p
        (%pool-forget-connection pool connection))
      (cl-concurrent-kit:condition-broadcast (%pool-condition pool))
      keep-p)))

(defun %pool-reserve-action (pool deadline)
  (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
    (when (pool-closed-p pool)
      (error 'pool-error :message "The connection pool is closed."))
    (cond
      ((%pool-available pool)
       (values :available (%pool-checkout-available-connection pool)))
      ((< (%pool-total-count pool) (pool-max-size pool))
       (%pool-reserve-create-slot pool)
       (values :create nil))
      (t
       (let ((remaining (%pool-remaining deadline)))
         (if (and remaining (<= remaining 0))
             (values :exhausted nil)
             (progn
               (cl-concurrent-kit:condition-wait
                (%pool-condition pool)
                (%pool-lock pool)
                :timeout remaining)
               (values :retry nil))))))))
