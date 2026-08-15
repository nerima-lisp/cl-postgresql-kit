(in-package #:cl-postgresql-kit)

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
  (%pool-validate-nonnegative-real-option
   max-idle-time
   max-idle-time
   "MAX-IDLE-TIME must be NIL or a non-negative real.")
  (%pool-validate-nonnegative-real-option
   max-connection-age
   max-connection-age
   "MAX-CONNECTION-AGE must be NIL or a non-negative real.")
  (%pool-validate-non-empty-string-option
   validation-query
   validation-query
   "VALIDATION-QUERY must be NIL or a non-empty string.")
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
           (if (%pool-acquirable-p pool candidate)
               (progn
                 (%pool-log pool "PostgreSQL connection acquired" :reused t)
                 (return candidate))
               (progn
                 (%pool-retire-in-use pool candidate))))
          (:create
           (%with-pool-created-connection
               (connection keep-p pool :in-use)
             keep-p
             (%pool-log pool "PostgreSQL connection acquired" :reused nil)
             (return connection))
           (error 'pool-error :message "The connection pool was closed."))
          (:exhausted
           (error 'pool-exhausted :timeout timeout))
          (:retry nil))))))

(defun pool-release (pool connection)
  "Return CONNECTION to POOL, or close it when it is unhealthy."
  (check-type pool connection-pool)
  (check-type connection connection)
  (multiple-value-bind (known-p closed-p)
      (%pool-begin-reset pool connection)
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
        (setf keep-p (%pool-finish-reset pool connection reset-p)))
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
  (%with-pool-created-connection (connection keep-p pool :idle)
    connection
    keep-p))

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
