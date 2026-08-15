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

(defun %function-call-result-type-oid (result-type-oid)
  (or result-type-oid
      0))

(defun %function-call-wire-arguments (arguments prepared-parameters)
  (loop for parameter in prepared-parameters
        collect (let ((value (getf parameter :value))
                      (format (getf parameter :format)))
                  (list (or value +sql-null+)
                        :format (if (or (null value)
                                        (= format 0))
                                    :text
                                    :binary)))))

(defun function-call (connection function-oid arguments
                      &key argument-type-oids argument-formats
                        (result-format 0) result-type-oid)
  "Call a PostgreSQL function through the v3 FunctionCall protocol message.

ARGUMENTS are plain values, octet vectors, SQL-NULL, or TYPED-VALUE objects.
RESULT-TYPE-OID requests decoding of a non-NULL result through the connection
type registry; without it, the result is returned as an octet vector."
  (check-type connection connection)
  (unless (and (integerp function-oid) (<= 0 function-oid #xffffffff))
    (error 'parameter-error
           :parameter function-oid
           :message "Function OIDs must be unsigned 32-bit integers."))
  (let* ((argument-list (%parameter-sequence-list arguments :arguments))
         (argument-count (length argument-list))
         (argument-type-oids (%parameter-oids-list argument-type-oids argument-count))
         (argument-formats (%parameter-formats-list argument-formats argument-count))
         (prepared-parameters (%prepare-parameters connection argument-list
                                                    argument-type-oids
                                                    argument-formats))
         (wire-arguments (mapcar (lambda (original parameter)
                                   (if (sql-null-p original)
                                       +sql-null+
                                       (getf parameter :value)))
                                 argument-list prepared-parameters))
         (wire-formats (mapcar (lambda (parameter)
                                 (getf parameter :format))
                               prepared-parameters))
         (result-format (%protocol-format-code result-format))
         (result-type-oid (and result-type-oid
                               (first (%parameter-oids-list (list result-type-oid))))))
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%require-open-connection connection)
      (%require-no-active-copy connection)
      (%require-no-active-cursors connection)
      (%with-operation-boundary (connection :operation :function-call)
        (%send-frontend-message
         connection
         (encode-function-call-message function-oid wire-arguments
                                       :argument-formats wire-formats
                                       :result-format result-format))
        (%send-frontend-message connection (encode-sync-message))
        (let ((result nil)
              (received-p nil)
              (pending nil))
          (loop for message = (%read-backend-message connection)
                do (multiple-value-bind (kind value)
                       (%process-backend-message connection message)
                     (case kind
                       (:function-call-response
                        (when received-p
                          (error 'protocol-error
                                 :message "PostgreSQL returned multiple FunctionCallResponse messages."))
                        (setf result
                              (parse-function-call-response
                               (backend-message-payload message))
                              received-p t))
                       (:error-response (setf pending value))
                       (:ready-for-query
                        (when pending (error pending))
                        (unless received-p
                          (error 'protocol-error
                                 :message "PostgreSQL completed a function call without a result."))
                        (return
                          (if (or (null result-type-oid)
                                  (sql-null-p result))
                              result
                              (decode-value (connection-type-registry connection)
                                            result-type-oid result
                                            :format result-format))))
                       ((:notice-response :notification-response) nil)
                       (otherwise
                        (error 'protocol-error
                               :message "Unexpected PostgreSQL message during a function call."
                               :context kind))))))))))
