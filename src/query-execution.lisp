(in-package #:cl-postgresql-kit)

(defun %run-exchange (connection &key sql parameters parameter-type-oids
                                  parameter-formats result-formats statement-name
                                  portal-name max-rows max-result-rows
                                  max-result-bytes
                                  collect-results-p
                                  expected-prepared-statement)
  (setf (connection--pending-error connection) nil)
  (let* ((requested-extended-p
           (or parameters parameter-type-oids parameter-formats result-formats
               statement-name portal-name max-rows))
         (parameter-list (%parameter-sequence-list parameters :parameters))
         (parameter-count (length parameter-list))
         (parameter-type-oids (%parameter-oids-list parameter-type-oids
                                                    parameter-count))
         (parameter-formats (%parameter-formats-list parameter-formats
                                                      parameter-count))
         (statement-name (%query-string-or-nil statement-name :statement-name))
         (portal-name (%query-string-or-nil portal-name :portal-name))
         (max-rows (%max-rows-value max-rows))
         (max-result-bytes (%max-result-bytes-value max-result-bytes))
         (extended-p requested-extended-p)
         (prepared-statement nil)
         (effective-parameter-type-oids parameter-type-oids)
         (prepared-parameters nil)
         (columns #())
         (raw-rows nil)
         (command-tag nil)
         (portal-suspended-p nil)
         (notices nil)
         (results nil)
         (current-result-p nil)
         (total-row-count 0)
         (total-result-bytes 0))
    (multiple-value-setq (prepared-statement effective-parameter-type-oids)
      (%resolve-prepared-statement connection statement-name sql
                                    parameter-count parameter-type-oids
                                    expected-prepared-statement))
    (setf prepared-parameters
          (and extended-p
               (%prepare-parameters connection parameter-list
                                     effective-parameter-type-oids
                                     parameter-formats)))
    (labels ((account-result-payload (message)
               (when max-result-bytes
                 (let ((payload-bytes
                         (length (backend-message-payload message))))
                   (when (> (+ total-result-bytes payload-bytes)
                            max-result-bytes)
                     (%retire-connection connection)
                     (error 'query-error
                            :message "The client-side result byte limit was exceeded."))
                   (incf total-result-bytes payload-bytes))))
             (make-current-result ()
               (let* ((result-notices (%query-notice-list notices))
                      (decoded-rows
                        (coerce
                         (loop for raw-row in (nreverse raw-rows)
                               collect (%decode-query-row
                                        connection columns raw-row))
                         'vector))
                      (result (%make-query-result
                               :columns columns
                               :rows decoded-rows
                               :command-tag command-tag
                               :row-count (length decoded-rows)
                               :transaction-status
                               (connection-transaction-status connection)
                               :notices result-notices
                               :portal-suspended-p portal-suspended-p)))
                  (setf notices nil)
                  (push result results)))
             (finish-current-result ()
               (when current-result-p
                 (make-current-result)
                 (setf columns #()
                       raw-rows nil
                       command-tag nil
                       portal-suspended-p nil
                       current-result-p nil))))
    ;; Build every frame before writing the first one.  A malformed parameter
    ;; must not leave a half-written extended-query exchange on the socket.
    (let* ((parse-message
             (and extended-p
                  (unless prepared-statement
                    (encode-parse-message
                     sql
                     :statement-name (or statement-name "")
                     :parameter-type-oids
                     (or effective-parameter-type-oids
                         (mapcar (lambda (parameter) (getf parameter :oid))
                                 prepared-parameters))))))
           (bind-message
             (and extended-p
                  (encode-bind-message
                   (mapcar (lambda (parameter) (getf parameter :value))
                           prepared-parameters)
                   :portal-name (or portal-name "")
                   :statement-name (or statement-name "")
                   :parameter-formats
                   (mapcar (lambda (parameter) (getf parameter :format))
                           prepared-parameters)
                   :result-formats result-formats)))
           (describe-message
             (and extended-p
                  (encode-describe-message (or portal-name "") :kind :portal)))
           (execute-message
             (and extended-p
                  (encode-execute-message :portal-name (or portal-name "")
                                          :max-rows (or max-rows 0))))
           (sync-message (and extended-p (encode-sync-message)))
           (query-message (and (not extended-p) (encode-query-message sql))))
      (if extended-p
          (progn
            (when parse-message
              (%send-frontend-message connection parse-message))
            (%send-frontend-message connection bind-message)
            (%send-frontend-message connection describe-message)
            (%send-frontend-message connection execute-message)
            (%send-frontend-message connection sync-message))
          (%send-frontend-message connection query-message))
      (loop for message = (%read-backend-message connection)
            do (multiple-value-bind (kind value)
                   (%process-backend-message connection message)
                 (case kind
                   (:row-description
                    (account-result-payload message)
                    (setf columns (parse-row-description
                                   (backend-message-payload message))
                          current-result-p t))
                   (:data-row
                    (account-result-payload message)
                    (when (and max-result-rows
                               (>= total-row-count max-result-rows))
                      (%retire-connection connection)
                      (error 'query-error
                             :message "The client-side result row limit was exceeded."))
                    (incf total-row-count)
                    (push (parse-data-row (backend-message-payload message)) raw-rows)
                    (setf current-result-p t))
                   (:command-complete
                    (account-result-payload message)
                    (setf command-tag
                          (parse-command-complete
                           (backend-message-payload message))
                          current-result-p t)
                    (finish-current-result))
                   (:empty-query-response
                    (account-result-payload message)
                    (setf current-result-p t)
                    (finish-current-result))
                   (:portal-suspended
                    (account-result-payload message)
                    (setf portal-suspended-p t
                          current-result-p t))
                   (:notice-response (push value notices))
                   (:error-response nil)
                   (:ready-for-query
                    (when (connection--pending-error connection)
                      (let ((condition (connection--pending-error connection)))
                        (setf (connection--pending-error connection) nil)
                        (error condition)))
                    (finish-current-result)
                     (unless results
                       (setf current-result-p t)
                       (finish-current-result))
                     (when (and results notices)
                       (setf (query-result-notices (first results))
                             (append (query-result-notices (first results))
                                     (%query-notice-list notices))
                             notices nil))
                     (unless collect-results-p
                      (when (> (length results) 1)
                        (error 'multiple-results-error
                               :message "QUERY supports only one SQL result per call.")))
                    (return))
                   (:copy-in-response
                    (error 'copy-error
                           :message "COPY IN cannot be consumed by QUERY."))
                   (:copy-out-response
                    (error 'copy-error
                           :message "COPY OUT cannot be consumed by QUERY."))
                   (:copy-both-response
                    (error 'copy-error
                           :message "COPY BOTH cannot be consumed by QUERY."))
                   (otherwise nil))))
      (let ((ordered-results (nreverse results)))
        (when (and (not collect-results-p)
                   (> (length ordered-results) 1))
          (error 'multiple-results-error
                 :message "QUERY supports only one SQL result per call."))
        (if collect-results-p
            (progn
              (setf (connection-last-result connection)
                    (car (last ordered-results)))
              ordered-results)
            (let ((result (first ordered-results)))
              (setf (connection-last-result connection) result)
              result)))))))

(defun %retire-connection (connection)
  (ignore-errors (transport-close (connection-transport connection)))
  (%clear-connection-session-state connection)
  (%clear-connection-secrets connection)
  (setf (connection-open connection) nil
        (connection-state connection) :failed
        (connection-transaction-status connection) :idle
        (connection--initial-transport-used-p connection) nil)
  t)

(defun %exchange-condition-requires-retirement-p (condition)
  (not (or (typep condition 'server-error)
           (typep condition 'parameter-error)
           (typep condition 'multiple-results-error))))

(defmacro %with-exchange-failure-retirement ((connection) &body body)
  (let ((retired-p (gensym "RETIRED-")))
    `(let ((,retired-p nil))
       (handler-bind
           ((error
              (lambda (condition)
                (when (and (not ,retired-p)
                           (%exchange-condition-requires-retirement-p condition))
                  (setf ,retired-p t)
                  (%retire-connection ,connection)))))
         ,@body))))
