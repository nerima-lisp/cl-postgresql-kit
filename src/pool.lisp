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
    (setf (%pool-in-use pool)
          (delete connection (%pool-in-use pool) :test #'eq))
    (cl-concurrent-kit:condition-broadcast (%pool-condition pool)))
  t)

(defun %pool-retire-in-use (pool connection)
  (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
    (setf (%pool-in-use pool)
          (delete connection (%pool-in-use pool) :test #'eq))
    (%pool-forget-connection pool connection)
    (cl-concurrent-kit:condition-broadcast (%pool-condition pool)))
  (%pool-disconnect connection))

(defun %pool-reserve-action (pool deadline)
  (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
    (when (pool-closed-p pool)
      (error 'pool-error :message "The connection pool is closed."))
    (cond
      ((%pool-available pool)
       (let ((connection (pop (%pool-available pool))))
         (remhash connection (%pool-idle-since pool))
         (push connection (%pool-in-use pool))
         (values :available connection)))
      ((< (%pool-total-count pool) (pool-max-size pool))
       (incf (%pool-creating-count pool))
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

(defun make-pool (&key (max-size 10) (min-size 0) max-idle-time
                       max-connection-age validation-query
                       connection-factory logger)
  "Create a connection pool.

CONNECTION-FACTORY is called without arguments whenever the pool needs a new
CONNECTION.  The pool connects the returned object when necessary and never
returns an unhealthy or failed-transaction connection to its caller.

MAX-IDLE-TIME and MAX-CONNECTION-AGE are lazy eviction limits in seconds.
Idle connections are evicted on acquisition and by POOL-REAP; connections
which exceed their maximum age are retired when returned as well.  When
VALIDATION-QUERY is non-NIL, it is executed whenever an idle connection is
acquired."
  (check-type max-size (integer 1 *))
  (check-type min-size (integer 0 *))
  (unless (or (null max-idle-time)
              (and (realp max-idle-time) (>= max-idle-time 0)))
    (error 'parameter-error :parameter max-idle-time
           :message "MAX-IDLE-TIME must be NIL or a non-negative real."))
  (unless (or (null max-connection-age)
              (and (realp max-connection-age) (>= max-connection-age 0)))
    (error 'parameter-error :parameter max-connection-age
           :message
           "MAX-CONNECTION-AGE must be NIL or a non-negative real."))
  (unless (or (null validation-query)
              (and (stringp validation-query)
                   (plusp (length validation-query))))
    (error 'parameter-error :parameter validation-query
           :message "VALIDATION-QUERY must be NIL or a non-empty string."))
  (unless (<= min-size max-size)
    (error 'parameter-error :parameter min-size
           :message "MIN-SIZE must not exceed MAX-SIZE."))
  (unless (functionp connection-factory)
    (error 'parameter-error :parameter connection-factory
           :message "CONNECTION-FACTORY must be a function."))
    (let ((pool (make-instance 'connection-pool
                             :max-size max-size
                             :min-size min-size
                             :max-idle-time max-idle-time
                             :max-connection-age max-connection-age
                             :validation-query validation-query
                             :connection-factory connection-factory
                             :logger logger)))
    (handler-case
        (dotimes (index min-size)
          (let ((connection (%pool-new-connection pool)))
            (%pool-record-new-connection pool connection)
            (%pool-record-idle-connection pool connection)
            (push connection (%pool-available pool))))
      (error (condition)
        (pool-close pool)
        (error condition)))
    (%pool-log pool "PostgreSQL connection pool created"
               :max-size max-size :min-size min-size)
    pool))

(defun pool-acquire (pool &key timeout)
  "Acquire a healthy connection from POOL.

TIMEOUT is measured in seconds.  NIL waits indefinitely; zero performs an
immediate, non-blocking acquisition attempt."
  (check-type pool connection-pool)
  (unless (or (null timeout) (and (realp timeout) (>= timeout 0)))
    (error 'parameter-error :parameter timeout
           :message "Pool acquisition timeout must be NIL or a non-negative real."))
  (let ((deadline (%pool-deadline timeout)))
    (loop
      (multiple-value-bind (action candidate)
          (%pool-reserve-action pool deadline)
        (case action
          (:available
           (if (and (not (%pool-expired-p pool candidate))
                    (%pool-usable-p candidate)
                    (%pool-validate-connection pool candidate))
               (progn
                 (%pool-log pool "PostgreSQL connection acquired" :reused t)
                 (return candidate))
               (progn
                 (%pool-retire-in-use pool candidate))))
          (:create
           (let ((reservation-released-p nil))
             (handler-case
                 (let ((connection (%pool-new-connection pool)))
                   (let ((keep-p nil))
                     (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
                       (decf (%pool-creating-count pool))
                       (setf reservation-released-p t)
                       (if (pool-closed-p pool)
                           (cl-concurrent-kit:condition-broadcast
                            (%pool-condition pool))
                           (progn
                             (%pool-record-new-connection pool connection)
                             (push connection (%pool-in-use pool))
                             (setf keep-p t)
                             (cl-concurrent-kit:condition-broadcast
                              (%pool-condition pool)))))
                     (if keep-p
                         (progn
                           (%pool-log pool "PostgreSQL connection acquired" :reused nil)
                           (return connection))
                         (progn
                           (%pool-disconnect connection)
                           (error 'pool-error :message "The connection pool was closed.")))))
               (error (condition)
                 (unless reservation-released-p
                   (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
                     (decf (%pool-creating-count pool))
                     (cl-concurrent-kit:condition-broadcast (%pool-condition pool))))
                 (error condition)))))
          (:exhausted
           (error 'pool-exhausted :timeout timeout))
          (:retry nil))))))

(defun pool-release (pool connection)
  "Return CONNECTION to POOL, or close it when it is unhealthy."
  (check-type pool connection-pool)
  (check-type connection connection)
  (let ((known-p nil)
        (closed-p nil))
    (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
      (when (member connection (%pool-in-use pool) :test #'eq)
        (setf known-p t
              closed-p (pool-closed-p pool)
              (%pool-in-use pool)
              (delete connection (%pool-in-use pool) :test #'eq))
        (incf (%pool-resetting-count pool))
        (cl-concurrent-kit:condition-broadcast (%pool-condition pool))))
    (unless known-p
      (error 'pool-error
             :message "The connection does not belong to the pool."))
    (let ((reset-p nil)
          (keep-p nil))
      (unwind-protect
           (unless (or closed-p (%pool-expired-p pool connection))
             (handler-case
                 (setf reset-p (%pool-reset-connection connection))
               (error ()
                 (setf reset-p nil))))
        (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
          (decf (%pool-resetting-count pool))
          (unless (or (pool-closed-p pool)
                      (not reset-p)
                      (%pool-expired-p pool connection))
            (%pool-record-idle-connection pool connection)
            (push connection (%pool-available pool))
            (setf keep-p t))
          (unless keep-p
            (%pool-forget-connection pool connection))
          (cl-concurrent-kit:condition-broadcast (%pool-condition pool))))
      (unless keep-p
        (%pool-disconnect connection))
      (%pool-log pool "PostgreSQL connection released" :kept keep-p)
      t)))

(defun pool-close (pool)
  "Close POOL and all idle connections.  In-use connections close on release."
  (check-type pool connection-pool)
  (let (connections)
    (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
      (unless (pool-closed-p pool)
        (setf (pool-closed-p pool) t
              connections (%pool-available pool)
              (%pool-available pool) nil)
        (dolist (connection connections)
          (%pool-forget-connection pool connection))
        (cl-concurrent-kit:condition-broadcast (%pool-condition pool))))
    (dolist (connection connections)
      (%pool-disconnect connection))
    (%pool-log pool "PostgreSQL connection pool closed")
    pool))

(defun pool-reap (pool)
  "Retire expired or unhealthy idle connections and return the count.

This operation never runs a validation query and never touches connections
currently checked out by callers.  It is safe to call from a maintenance
loop, while POOL-ACQUIRE also performs the same lazy checks for one candidate
at a time."
  (check-type pool connection-pool)
  (let ((retired nil))
    (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
      (let ((kept nil))
        (dolist (connection (%pool-available pool))
          (if (or (%pool-expired-p pool connection)
                  (not (%pool-usable-p connection)))
              (progn
                (%pool-forget-connection pool connection)
                (push connection retired))
              (push connection kept)))
        (setf (%pool-available pool) (nreverse kept))
        (when retired
          (cl-concurrent-kit:condition-broadcast (%pool-condition pool)))))
    (dolist (connection retired)
      (%pool-disconnect connection))
    (length retired)))

(defun %pool-create-idle (pool)
  (let ((connection nil)
        (reservation-released-p nil)
        (keep-p nil))
    (handler-case
        (progn
          (setf connection (%pool-new-connection pool))
          (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
            (decf (%pool-creating-count pool))
            (setf reservation-released-p t)
            (if (pool-closed-p pool)
                (cl-concurrent-kit:condition-broadcast (%pool-condition pool))
                (progn
                  (%pool-record-new-connection pool connection)
                  (%pool-record-idle-connection pool connection)
                  (push connection (%pool-available pool))
                  (setf keep-p t)
                  (cl-concurrent-kit:condition-broadcast (%pool-condition pool)))))
          (unless keep-p
            (%pool-disconnect connection))
          keep-p)
      (error (condition)
        (unless reservation-released-p
          (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
            (decf (%pool-creating-count pool))
            (cl-concurrent-kit:condition-broadcast (%pool-condition pool))))
        (when (and connection (not keep-p))
          (%pool-disconnect connection))
        (error condition)))))

(defun pool-refill (pool)
  "Create idle connections until POOL reaches POOL-MIN-SIZE.

The operation is safe to call from a maintenance loop after POOL-REAP.  It
returns the number of connections created by this invocation and counts
connections currently being created or reset toward the minimum." 
  (check-type pool connection-pool)
  (let ((created 0))
    (loop
      (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
        (when (pool-closed-p pool)
          (error 'pool-error :message "The connection pool is closed."))
        (if (>= (%pool-total-count pool) (pool-min-size pool))
            (return-from pool-refill created)
            (incf (%pool-creating-count pool))))
      (when (%pool-create-idle pool)
        (incf created)))))

(defun pool-size (pool)
  "Return the number of idle, in-use, creating, and resetting connections."
  (check-type pool connection-pool)
  (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
    (%pool-total-count pool)))

(defun pool-available-count (pool)
  "Return the number of idle connections available for acquisition."
  (check-type pool connection-pool)
  (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
    (length (%pool-available pool))))

(defun pool-in-use-count (pool)
  "Return the number of connections currently held by callers."
  (check-type pool connection-pool)
  (cl-concurrent-kit:with-lock-held ((%pool-lock pool))
    (length (%pool-in-use pool))))

(defmacro with-connection ((variable pool &key timeout) &body body)
  "Acquire a connection for BODY and release it even when BODY signals."
  (let ((pool-var (gensym "POOL-")))
    `(let ((,pool-var ,pool))
       (let ((,variable (pool-acquire ,pool-var :timeout ,timeout)))
         (unwind-protect
              (progn ,@body)
           (pool-release ,pool-var ,variable))))))
