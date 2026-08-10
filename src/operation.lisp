(in-package #:cl-postgresql-kit)

(defmacro %with-operation-boundary ((connection &key (operation :operation))
                                    &body body)
  "Run BODY under the shared timeout and exchange-retirement boundary.

CONNECTION and OPERATION are evaluated once.  Locking and operation-specific
state guards remain at each call site because their ordering is part of the
operation's protocol semantics."
  (let ((connection-var (gensym "CONNECTION-"))
        (operation-var (gensym "OPERATION-")))
    `(let ((,connection-var ,connection)
           (,operation-var ,operation))
       (%call-with-operation-metrics
        ,connection-var
        ,operation-var
        (lambda ()
          (%call-with-query-timeout
           ,connection-var
           (lambda ()
             (%with-exchange-failure-retirement (,connection-var)
               ,@body))))))))
