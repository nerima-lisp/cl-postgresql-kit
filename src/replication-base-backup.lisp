(in-package #:cl-postgresql-kit)

(defun %base-backup-invalid-start-copy-direction ()
  (%copy-protocol-error
   "BASE_BACKUP must start with COPY TO STDOUT."))

(defun %base-backup-invalid-stream-copy-direction ()
  (%copy-protocol-error
   "The PostgreSQL server returned an invalid BASE_BACKUP COPY direction."))

(defmacro %base-backup-read-or-abort ((operation) &body body)
  `(let ((returned-event-p nil)
         (completed-p nil))
     (unwind-protect
          (progn ,@body)
       (unless (or returned-event-p completed-p)
         (%copy-abort ,operation)))))

(defun replication-base-backup-start
    (connection &key label target target-detail
                   (progress-p nil progress-supplied-p) checkpoint
                   (wal-p nil wal-supplied-p) (wait-p nil wait-supplied-p)
                   compression compression-detail max-rate
                   (tablespace-map-p nil tablespace-map-supplied-p)
                   (verify-checksums-p nil verify-checksums-supplied-p)
                   manifest manifest-checksums incremental-p options)
  "Start BASE_BACKUP and return a stream handle.

BASE-BACKUP-START-RESULTS contains the ordinary result sets that precede the
COPY stream (notably the two result sets required by INCREMENTAL).  Call
BASE-BACKUP-READ until it returns NIL, then inspect BASE-BACKUP-FINAL-RESULTS
for the ordinary result sets that follow the backup stream."
  (check-type connection connection)
  (let ((sql
          (%replication-base-backup-sql
           :label label :target target :target-detail target-detail
           :progress-p progress-p :progress-supplied-p progress-supplied-p
           :checkpoint checkpoint :wal-p wal-p :wal-supplied-p wal-supplied-p
           :wait-p wait-p :wait-supplied-p wait-supplied-p
           :compression compression :compression-detail compression-detail
           :max-rate max-rate :tablespace-map-p tablespace-map-p
           :tablespace-map-supplied-p tablespace-map-supplied-p
           :verify-checksums-p verify-checksums-p
           :verify-checksums-supplied-p verify-checksums-supplied-p
           :manifest manifest :manifest-checksums manifest-checksums
           :incremental-p incremental-p :options options)))
    (%with-exchange-failure-retirement (connection)
      (cl-concurrent-kit:with-lock-held ((connection--lock connection))
        (%require-open-connection connection)
        (%require-no-active-copy connection)
        (%require-no-active-cursors connection)
        (setf (connection--pending-error connection) nil)
        (%send-frontend-message connection (encode-query-message sql))
        (let ((result-state (make-instance '%base-backup-result-state))
              (results nil)
              (current-result nil))
          (labels ((finish-current-result ()
                     (setf current-result
                           (%base-backup-finish-result-state
                            connection result-state))
                     (when current-result
                       (push current-result results))))
            (loop for message = (%read-backend-message connection)
                  do (multiple-value-bind (kind value)
                         (%process-backend-message connection message)
                       (case kind
                         ((:row-description :data-row :command-complete
                           :empty-query-response :portal-suspended
                           :notice-response)
                          (%base-backup-handle-query-result-message
                              (kind message result-state value)
                            (finish-current-result)))
                         (:error-response nil)
                         (:copy-out-response
                          (finish-current-result)
                          (let ((operation
                                  (%base-backup-copy-operation
                                   connection
                                   message
                                   results)))
                            (setf (connection--active-copy connection) operation)
                            (return operation)))
                         ((:copy-in-response :copy-both-response)
                          (%base-backup-invalid-start-copy-direction))
                         (:ready-for-query
                          (%base-backup-ready-or-start-error connection))
                         (otherwise nil))))))))))

(defun base-backup-read (operation)
  "Read the next BASE_BACKUP stream event, or NIL after final results.

The returned event is a BASE-BACKUP-MESSAGE.  Final ordinary result sets are
stored in BASE-BACKUP-FINAL-RESULTS after NIL is returned."
  (check-type operation %base-backup-operation)
  (let ((connection (%copy-connection operation)))
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%copy-check-operation operation :copy-out-response)
      (%base-backup-read-or-abort (operation)
        (loop for message = (%read-backend-message connection)
              do (multiple-value-bind (kind value)
                     (%process-backend-message connection message)
                   (case kind
                     (:copy-data
                      (setf returned-event-p t)
                      (return
                        (parse-base-backup-message
                         (backend-message-payload message))))
                     (:copy-done
                      (setf (%base-backup-phase operation) :between-copy))
                     (:copy-out-response
                      (parse-copy-response
                       (backend-message-payload message))
                      (setf (%base-backup-phase operation) :copy))
                     ((:row-description :data-row :command-complete
                       :empty-query-response :portal-suspended
                       :notice-response)
                      (%base-backup-handle-query-result-message
                          (kind
                           message
                           (%base-backup-result-state operation)
                           value)
                        (%base-backup-finish-current-result operation)))
                     (:error-response nil)
                     (:copy-fail
                      (%copy-protocol-error
                       (%copy-failure-message
                        (backend-message-payload message))))
                     (:ready-for-query
                      (setf completed-p t)
                      (return (%base-backup-finish-operation operation)))
                     ((:copy-in-response :copy-both-response)
                      (%base-backup-invalid-stream-copy-direction))
                     (otherwise nil))))))))

(defun base-backup-finish (operation)
  "Drain a BASE_BACKUP operation and return its final result sets."
  (loop while (base-backup-read operation)
        finally (return (base-backup-final-results operation))))
