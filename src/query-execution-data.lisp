(in-package #:cl-postgresql-kit)

(defstruct (query-exchange-state
             (:conc-name %query-exchange-state-)
             (:constructor %make-query-exchange-state
                 (&key connection max-result-rows max-result-bytes)))
  "Mutable data collected while one query exchange is in flight."
  connection
  max-result-rows
  max-result-bytes
  (columns #())
  (raw-rows nil)
  (command-tag nil)
  (portal-suspended-p nil)
  (notices nil)
  (results nil)
  (current-result-p nil)
  (total-row-count 0)
  (total-result-bytes 0))

(defmacro %with-query-exchange-state
    ((state connection &key max-result-rows max-result-bytes) &body body)
  "Bind a fresh exchange STATE and evaluate BODY.

CONNECTION and the limit expressions are evaluated exactly once.  The macro
keeps the transient protocol data in one object while the exchange logic
remains in ordinary functions that can receive the object as an argument."
  (let ((state-var (gensym "QUERY-EXCHANGE-")))
    `(let ((,state-var
             (%make-query-exchange-state
              :connection ,connection
              :max-result-rows ,max-result-rows
              :max-result-bytes ,max-result-bytes)))
       (let ((,state ,state-var))
         ,@body))))
