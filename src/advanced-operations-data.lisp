(in-package #:cl-postgresql-kit)

(defmacro %query-cps (promise-form on-success on-error)
  "Compose a query PROMISE-FORM with validated CPS continuations.

The promise expression is evaluated once, after the continuation contracts
have been checked.  Keeping that boundary in one macro makes the asynchronous
entry points agree on validation and preserves the data/logic split: callers
construct the operation, while this macro owns continuation composition."
  (let ((success-var (gensym "SUCCESS-"))
        (error-handler-var (gensym "ERROR-HANDLER-")))
    `(let ((,success-var ,on-success)
           (,error-handler-var ,on-error))
       (check-type ,success-var function)
       (when ,error-handler-var
         (check-type ,error-handler-var function))
       (cl-concurrent-kit:promise-then
        ,promise-form
        ,success-var
        ,error-handler-var))))

(defmacro %with-connection-operation ((connection operation) &body body)
  "Run BODY while holding CONNECTION's lock inside a validated operation boundary."
  `(cl-concurrent-kit:with-lock-held ((connection--lock ,connection))
     (%require-open-connection ,connection)
     (%require-no-active-copy ,connection)
     (%require-no-active-cursors ,connection)
     (%with-operation-boundary (,connection :operation ,operation)
       ,@body)))

(defun %read-until-ready-for-query (connection &optional handler)
  (let ((pending nil))
    (loop for message = (%read-backend-message connection)
          do (multiple-value-bind (kind value)
                 (%process-backend-message connection message)
               (case kind
                 (:error-response
                  (setf pending value))
                 (:ready-for-query
                  (when pending
                    (error pending))
                  (return))
                 (otherwise
                  (when handler
                    (funcall handler kind message value))))))))
