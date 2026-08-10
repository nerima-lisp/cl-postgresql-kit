(in-package #:cl-postgresql-kit)

(defstruct (connection-metrics
             (:constructor %make-connection-metrics-record
                 (&key registry operations-total operation-duration)))
  registry
  operations-total
  operation-duration)

(defun %build-connection-metrics (registry)
  (when registry
    (%make-connection-metrics-record
     :registry registry
     :operations-total
     (cl-observability-kit:define-counter
      registry postgresql_operations_total
      :help "Completed PostgreSQL wire-protocol operations."
      :label-names '("operation" "status"))
     :operation-duration
     (cl-observability-kit:define-histogram
      registry postgresql_operation_duration_seconds
      :help "Duration of PostgreSQL wire-protocol operations."
      :unit "seconds"
      :label-names '("operation")
      :buckets '(0.001d0 0.005d0 0.01d0 0.025d0 0.05d0 0.1d0
                 0.25d0 0.5d0 1.0d0 2.5d0 5.0d0 10.0d0)))))

(defun connection-metric-registry (connection)
  "Return CONNECTION's optional CL-OBSERVABILITY-KIT registry, or NIL."
  (check-type connection connection)
  (let ((metrics (connection--metrics connection)))
    (and metrics (connection-metrics-registry metrics))))

(defun %connection-operation-labels (operation &optional status)
  (let ((operation-name (string-downcase (string operation))))
    (if status
        (list (cons "operation" operation-name)
              (cons "status" (string-downcase (string status))))
        (list (cons "operation" operation-name)))))

(defun %record-operation-metrics (metrics operation status started-at)
  (when metrics
    (let ((labels (%connection-operation-labels operation status))
          (duration-labels (%connection-operation-labels operation))
          (duration (/ (- (get-internal-real-time) started-at)
                       (float internal-time-units-per-second 1d0))))
      ;; Observability must not turn a completed database operation into an
      ;; application error.  The registry was validated when the connection
      ;; was built, so these guards only protect the operation from a runtime
      ;; instrumentation failure.
      (ignore-errors
        (cl-observability-kit:metric-inc
         (connection-metrics-operations-total metrics)
         1
         :labels labels))
      (ignore-errors
        (cl-observability-kit:metric-observe
         (connection-metrics-operation-duration metrics)
         duration
         :labels duration-labels)))))

(defun %call-with-operation-metrics (connection operation thunk)
  (let ((metrics (connection--metrics connection))
        (started-at (get-internal-real-time))
        (completed-p nil))
    (unwind-protect
         (multiple-value-prog1 (funcall thunk)
           (setf completed-p t))
      (%record-operation-metrics
       metrics operation (if completed-p :success :error) started-at))))
