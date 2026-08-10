(in-package #:cl-postgresql-kit)

(defun %query-pipeline (connection requests &key max-result-rows max-result-bytes)
  (check-type connection connection)
  (check-type max-result-rows (or null (integer 0 *)))
  (check-type max-result-bytes (or null (integer 0 *)))
  (let ((request-list (%pipeline-request-list requests))
        (max-result-rows (%max-rows-value max-result-rows))
        (max-result-bytes (%max-result-bytes-value max-result-bytes)))
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%require-open-connection connection)
      (%require-no-active-copy connection)
      (%require-no-active-cursors connection)
      (%with-operation-boundary (connection :operation :query-pipeline)
        (setf (connection--pending-error connection) nil)
        (let ((entries nil)
              (frames nil))
          ;; Build the complete pipeline before touching the transport.  This
          ;; keeps a malformed later request from leaving earlier requests
          ;; half-written on the connection.
          (dolist (request request-list)
            (multiple-value-bind (entry request-frames)
                (%pipeline-request-frames connection request)
              (push entry entries)
              (dolist (frame request-frames)
                (push frame frames))))
          (let ((entries (coerce (nreverse entries) 'vector)))
            (dolist (frame (nreverse frames))
              (%send-frontend-message connection frame :flush-p nil))
            (%send-frontend-message connection (encode-flush-message))
            (let ((query-results
                    (%read-pipeline-results connection entries
                                            max-result-rows
                                            max-result-bytes)))
              (setf (connection-last-result connection)
                    (car (last query-results)))
              query-results)))))))
