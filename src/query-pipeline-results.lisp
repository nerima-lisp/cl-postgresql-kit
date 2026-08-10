(in-package #:cl-postgresql-kit)

(defun %read-pipeline-results
    (connection entries max-result-rows max-result-bytes)
  "Read backend messages for ENTRIES and assemble one result per request."
  (let ((entry-index 0)
        (total-row-count 0)
        (total-result-bytes 0))
    (labels ((account-result-payload (message)
               (when max-result-bytes
                 (let ((payload-bytes
                         (length (backend-message-payload message))))
                   (when (> (+ total-result-bytes payload-bytes)
                            max-result-bytes)
                     (%retire-connection connection)
                     (error 'query-error
                            :message
                            "The client-side result byte limit was exceeded."))
                   (incf total-result-bytes payload-bytes))))
             (current-entry ()
               (aref entries entry-index))
             (make-current-result (entry)
               (let* ((result-notices
                        (%query-notice-list
                         (pipeline-entry-notices entry)))
                      (decoded-rows
                        (coerce
                         (loop for raw-row
                                 in (nreverse
                                     (pipeline-entry-raw-rows entry))
                               collect (%decode-query-row
                                        connection
                                        (pipeline-entry-columns entry)
                                        raw-row))
                         'vector))
                      (result
                        (%make-query-result
                         :columns (pipeline-entry-columns entry)
                         :rows decoded-rows
                         :command-tag (pipeline-entry-command-tag entry)
                         :row-count (length decoded-rows)
                         :transaction-status
                         (connection-transaction-status connection)
                         :notices result-notices
                         :portal-suspended-p
                         (pipeline-entry-portal-suspended-p entry))))
                 (setf (pipeline-entry-notices entry) nil)
                 (push result (pipeline-entry-results entry))))
             (finish-current-result (entry)
               (when (pipeline-entry-current-result-p entry)
                 (make-current-result entry)
                 (setf (pipeline-entry-columns entry) #()
                       (pipeline-entry-raw-rows entry) nil
                       (pipeline-entry-command-tag entry) nil
                       (pipeline-entry-portal-suspended-p entry) nil
                       (pipeline-entry-current-result-p entry) nil)))
             (finish-entry (entry)
               (finish-current-result entry)
               (unless (pipeline-entry-results entry)
                 (setf (pipeline-entry-current-result-p entry) t)
                 (finish-current-result entry))
               (when (and (pipeline-entry-results entry)
                          (pipeline-entry-notices entry))
                 (setf (query-result-notices
                        (first (pipeline-entry-results entry)))
                       (append
                        (query-result-notices
                         (first (pipeline-entry-results entry)))
                        (%query-notice-list
                         (pipeline-entry-notices entry)))
                       (pipeline-entry-notices entry) nil))
               (setf (pipeline-entry-results entry)
                     (nreverse (pipeline-entry-results entry)))))
      (loop while (< entry-index (length entries))
            for message = (%read-backend-message connection)
            do (multiple-value-bind (kind value)
                   (%process-backend-message connection message)
                 (let ((entry (current-entry)))
                   (case kind
                     (:row-description
                      (account-result-payload message)
                      (setf (pipeline-entry-columns entry)
                            (parse-row-description
                             (backend-message-payload message))
                            (pipeline-entry-current-result-p entry) t))
                     (:data-row
                      (account-result-payload message)
                      (when (and max-result-rows
                                 (>= total-row-count max-result-rows))
                        (%retire-connection connection)
                        (error 'query-error
                               :message
                               "The client-side result row limit was exceeded."))
                      (incf total-row-count)
                      (push (parse-data-row
                             (backend-message-payload message))
                            (pipeline-entry-raw-rows entry))
                      (setf (pipeline-entry-current-result-p entry) t))
                     (:command-complete
                      (account-result-payload message)
                      (setf (pipeline-entry-command-tag entry)
                            (parse-command-complete
                             (backend-message-payload message))
                            (pipeline-entry-current-result-p entry) t)
                      (finish-current-result entry))
                     (:empty-query-response
                      (account-result-payload message)
                      (setf (pipeline-entry-current-result-p entry) t)
                      (finish-current-result entry))
                     (:portal-suspended
                      (account-result-payload message)
                      (setf (pipeline-entry-portal-suspended-p entry) t
                            (pipeline-entry-current-result-p entry) t))
                     (:notice-response
                      (push value (pipeline-entry-notices entry)))
                     (:error-response
                      (unless (pipeline-entry-pending-error entry)
                        (setf (pipeline-entry-pending-error entry) value)))
                     (:ready-for-query
                      (when (connection--pending-error connection)
                        (unless (pipeline-entry-pending-error entry)
                          (setf (pipeline-entry-pending-error entry)
                                (connection--pending-error connection)))
                        (setf (connection--pending-error connection) nil))
                      (finish-entry entry)
                      (incf entry-index))
                     ((:copy-in-response :copy-out-response
                       :copy-both-response :copy-data :copy-done
                       :copy-fail)
                      (error 'copy-error
                             :message
                             "COPY is not supported inside QUERY-PIPELINE."))
                     (otherwise nil)))))
      (let ((query-results nil)
            (server-condition nil)
            (multiple-condition nil))
        (loop for index below (length entries)
              for entry = (aref entries index)
              do (when (and (pipeline-entry-pending-error entry)
                            (not server-condition))
                   (setf server-condition
                         (pipeline-entry-pending-error entry)))
                 (when (> (length (pipeline-entry-results entry)) 1)
                   (unless multiple-condition
                     (setf multiple-condition
                           (make-condition
                            'multiple-results-error
                            :message
                            "QUERY-PIPELINE requests must produce one SQL result each."))))
                 (push (first (pipeline-entry-results entry)) query-results))
        (setf query-results (nreverse query-results))
        (when server-condition
          (error server-condition))
        (when multiple-condition
          (error multiple-condition))
        query-results))))
