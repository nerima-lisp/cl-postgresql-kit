(in-package #:cl-postgresql-kit)

(defun %retire-connection (connection)
  (ignore-errors (transport-close (connection-transport connection)))
  (%clear-connection-session-state connection)
  (%clear-connection-secrets connection)
  (setf (connection-open connection) nil
        (connection-state connection) :failed
        (connection-transaction-status connection) :idle
        (connection--initial-transport-used-p connection) nil)
  t)

(defun %account-query-result-payload (state message)
  (let ((limit (%query-exchange-state-max-result-bytes state)))
    (when limit
      (let* ((payload (backend-message-payload message))
             (payload-bytes (length payload))
             (total-bytes (%query-exchange-state-total-result-bytes state)))
        (when (> (+ total-bytes payload-bytes) limit)
          (%retire-connection (%query-exchange-state-connection state))
          (error 'query-error
                 :message "The client-side result byte limit was exceeded."))
        (incf (%query-exchange-state-total-result-bytes state)
              payload-bytes))))
  state)

(defun %make-current-query-result (state)
  (let* ((connection (%query-exchange-state-connection state))
         (columns (%query-exchange-state-columns state))
         (raw-rows (%query-exchange-state-raw-rows state))
         (decoded-rows
           (coerce
            (loop for raw-row in (nreverse raw-rows)
                  collect (%decode-query-row connection columns raw-row))
            'vector))
         (result
           (%make-query-result
            :columns columns
            :rows decoded-rows
            :command-tag (%query-exchange-state-command-tag state)
            :row-count (length decoded-rows)
            :transaction-status (connection-transaction-status connection)
            :notices (%query-notice-list
                      (%query-exchange-state-notices state))
            :portal-suspended-p
            (%query-exchange-state-portal-suspended-p state))))
    (setf (%query-exchange-state-notices state) nil)
    (push result (%query-exchange-state-results state))
    result))

(defun %finish-current-query-result (state)
  (when (%query-exchange-state-current-result-p state)
    (%make-current-query-result state)
    (setf (%query-exchange-state-columns state) #()
          (%query-exchange-state-raw-rows state) nil
          (%query-exchange-state-command-tag state) nil
          (%query-exchange-state-portal-suspended-p state) nil
          (%query-exchange-state-current-result-p state) nil))
  state)

(defun %raise-pending-query-error (connection)
  (let ((condition (connection--pending-error connection)))
    (when condition
      (setf (connection--pending-error connection) nil)
      (error condition))))

(defun %finalize-query-exchange-results (state collect-results-p)
  (%finish-current-query-result state)
  (unless (%query-exchange-state-results state)
    (setf (%query-exchange-state-current-result-p state) t)
    (%finish-current-query-result state))
  (when (and (%query-exchange-state-results state)
             (%query-exchange-state-notices state))
    (let ((first-result (first (%query-exchange-state-results state))))
      (setf (query-result-notices first-result)
            (append (query-result-notices first-result)
                    (%query-notice-list
                     (%query-exchange-state-notices state)))
            (%query-exchange-state-notices state) nil)))
  (let ((ordered-results (nreverse (%query-exchange-state-results state))))
    (unless collect-results-p
      (when (> (length ordered-results) 1)
        (error 'multiple-results-error
               :message "QUERY supports only one SQL result per call.")))
      ordered-results))

(defun %consume-query-row-description (state message)
  (%account-query-result-payload state message)
  (setf (%query-exchange-state-columns state)
        (parse-row-description (backend-message-payload message))
        (%query-exchange-state-current-result-p state) t))

(defun %consume-query-data-row (connection state message)
  (%account-query-result-payload state message)
  (when (and (%query-exchange-state-max-result-rows state)
             (>= (%query-exchange-state-total-row-count state)
                 (%query-exchange-state-max-result-rows state)))
    (%retire-connection connection)
    (error 'query-error
           :message "The client-side result row limit was exceeded."))
  (incf (%query-exchange-state-total-row-count state))
  (push (parse-data-row (backend-message-payload message))
        (%query-exchange-state-raw-rows state))
  (setf (%query-exchange-state-current-result-p state) t))

(defun %consume-query-result-message (state kind message)
  (%account-query-result-payload state message)
  (case kind
    (:command-complete
     (setf (%query-exchange-state-command-tag state)
           (parse-command-complete (backend-message-payload message))
           (%query-exchange-state-current-result-p state) t)
     (%finish-current-query-result state))
    (:empty-query-response
     (setf (%query-exchange-state-current-result-p state) t)
     (%finish-current-query-result state))
    (:portal-suspended
     (setf (%query-exchange-state-portal-suspended-p state) t
           (%query-exchange-state-current-result-p state) t))))

(defun %reject-query-copy-response (kind)
  (error 'copy-error
         :message
         (case kind
           (:copy-in-response "COPY IN cannot be consumed by QUERY.")
           (:copy-out-response "COPY OUT cannot be consumed by QUERY.")
           (:copy-both-response "COPY BOTH cannot be consumed by QUERY."))))

(defun %consume-query-exchange-cps
    (connection state collect-results-p continuation)
  "Consume backend messages and pass final results to CONTINUATION.

The callback is invoked only after ReadyForQuery has been received and all
pending server errors have been signaled."
  (loop for message = (%read-backend-message connection)
        do (multiple-value-bind (kind value)
             (%process-backend-message connection message)
             (case kind
               (:row-description
                (%consume-query-row-description state message))
               (:data-row
                (%consume-query-data-row connection state message))
               ((:command-complete :empty-query-response :portal-suspended)
                (%consume-query-result-message state kind message))
               (:notice-response
                (push value (%query-exchange-state-notices state)))
               (:error-response nil)
               (:ready-for-query
                (%raise-pending-query-error connection)
                (return
                  (funcall continuation
                           (%finalize-query-exchange-results
                            state collect-results-p))))
               ((:copy-in-response :copy-out-response :copy-both-response)
                (%reject-query-copy-response kind))
               (otherwise nil)))))

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
