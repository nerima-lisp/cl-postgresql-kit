(in-package #:cl-postgresql-kit)

(defun %run-exchange (connection &key sql parameters parameter-type-oids
                                  parameter-formats result-formats statement-name
                                  portal-name max-rows max-result-rows
                                  max-result-bytes
                                  collect-results-p
                                  expected-prepared-statement)
  (setf (connection--pending-error connection) nil)
  (let ((max-result-bytes (%max-result-bytes-value max-result-bytes)))
    (%with-query-exchange-state
        (state connection
          :max-result-rows max-result-rows
          :max-result-bytes max-result-bytes)
      (dolist (frame
                (%query-exchange-frames
                 connection sql parameters parameter-type-oids parameter-formats
                 result-formats statement-name portal-name max-rows
                 expected-prepared-statement))
        (%send-frontend-message connection frame))
      (%consume-query-exchange-cps
       connection state collect-results-p
       (lambda (ordered-results)
         (if collect-results-p
             (progn
               (setf (connection-last-result connection)
                     (car (last ordered-results)))
               ordered-results)
             (let ((result (first ordered-results)))
               (setf (connection-last-result connection) result)
               result)))))))
