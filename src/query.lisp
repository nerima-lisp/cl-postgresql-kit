(in-package #:cl-postgresql-kit)

(defun %query (connection sql &key parameters parameter-type-oids parameter-formats
                               result-formats statement-name portal-name max-rows
                               max-result-rows max-result-bytes collect-results-p
                               expected-prepared-statement)
  (check-type connection connection)
  (check-type sql string)
  (check-type max-result-rows (or null (integer 0 *)))
  (check-type max-result-bytes (or null (integer 0 *)))
  (cl-concurrent-kit:with-lock-held ((connection--lock connection))
    (%require-open-connection connection)
    (%require-no-active-copy connection)
    (%require-no-active-cursors connection)
    (%with-operation-boundary (connection :operation :query)
      (%run-exchange connection
                     :sql sql
                     :parameters parameters
                     :parameter-type-oids parameter-type-oids
                     :parameter-formats parameter-formats
                     :result-formats result-formats
                     :statement-name statement-name
                     :portal-name portal-name
                     :max-rows max-rows
                     :max-result-rows max-result-rows
                     :max-result-bytes max-result-bytes
                     :collect-results-p collect-results-p
                     :expected-prepared-statement expected-prepared-statement))))

(defun query (connection sql &key parameters parameter-type-oids parameter-formats
                               result-formats statement-name portal-name max-rows
                               max-result-rows max-result-bytes)
  "Execute SQL and return a QUERY-RESULT.

With PARAMETERS present, the extended query protocol is used.  Values may be
plain Common Lisp values or TYPED-VALUE objects.  A simple-query string that
produces more than one result signals MULTIPLE-RESULTS-ERROR; use QUERY-ALL
when all results are wanted."
  (%query connection sql
          :parameters parameters
          :parameter-type-oids parameter-type-oids
          :parameter-formats parameter-formats
          :result-formats result-formats
          :statement-name statement-name
          :portal-name portal-name
          :max-rows max-rows
          :max-result-rows max-result-rows
          :max-result-bytes max-result-bytes
          :collect-results-p nil))

(defun query-all (connection sql &key parameters parameter-type-oids parameter-formats
                                   result-formats statement-name portal-name max-rows
                                   max-result-rows max-result-bytes)
  "Execute SQL and return all QUERY-RESULT objects in server order."
  (%query connection sql
          :parameters parameters
          :parameter-type-oids parameter-type-oids
          :parameter-formats parameter-formats
          :result-formats result-formats
          :statement-name statement-name
          :portal-name portal-name
          :max-rows max-rows
          :max-result-rows max-result-rows
          :max-result-bytes max-result-bytes
          :collect-results-p t))
