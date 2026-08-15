(in-package #:cl-postgresql-kit/test)

(deftest cps-callback-validation
  (with-test-connection
      (connection (ready-memory-connection))
    (it-signals-each 'type-error
        ((:query-cps)
         (:query-pipeline-cps))
      "rejects non-callable CPS error callbacks"
      (label)
      (case label
        (:query-cps
         (query-cps connection "select 1"
                    (lambda (result) result)
                    :on-error 1))
        (:query-pipeline-cps
         (query-pipeline-cps connection '()
                             (lambda (results) results)
                             :on-error 1))))))

(deftest pipeline-prepared-statement-cache-validation
  (with-test-connection
      (connection (ready-memory-connection))
    (it-signals-each 'parameter-error
        ((:invalid-cache-entry)
         (:cached-sql-mismatch)
         (:cached-parameter-type-mismatch))
      "rejects inconsistent prepared statement cache entries"
      (label)
      (case label
        (:invalid-cache-entry
         (setf (gethash "invalid"
                        (cl-postgresql-kit::connection--prepared-statements
                         connection))
               :invalid)
         (cl-postgresql-kit::%pipeline-request-frames
          connection
          (make-pipeline-request
           "select 1"
           :statement-name "invalid")))
        (:cached-sql-mismatch
         (setf (gethash "cached"
                        (cl-postgresql-kit::connection--prepared-statements
                         connection))
               (cl-postgresql-kit::%make-prepared-statement
                :name "cached"
                :sql "select 2"
                :connection connection))
         (cl-postgresql-kit::%pipeline-request-frames
          connection
          (make-pipeline-request
           "select 1"
           :statement-name "cached")))
        (:cached-parameter-type-mismatch
         (setf (gethash "cached"
                        (cl-postgresql-kit::connection--prepared-statements
                         connection))
               (cl-postgresql-kit::%make-prepared-statement
                :name "cached"
                :sql "select $1"
                :parameter-type-oids '(23)
                :connection connection))
         (cl-postgresql-kit::%pipeline-request-frames
          connection
          (make-pipeline-request
           "select $1"
           :parameters '(1)
           :parameter-type-oids '(25)
           :statement-name "cached")))))))

(deftest invalid-parameter-cardinality-writes-nothing
  (with-test-transport-connection
      (connection transport (ready-memory-connection))
    (assert-signals 'parameter-error
                    (lambda ()
                      (query connection "select $1"
                             :parameters (list "x")
                             :parameter-type-oids (list 23 25))))
    (is (zerop (length (memory-transport-output transport))))
    (is (connection-open connection))
    (is (eq (connection-state connection) :ready))))

(deftest async-query-uses-concurrent-kit
  (let* ((input
           (join-octets
            (make-frame #\C (cstring "SELECT 1"))
            (make-frame #\Z (octets (char-code #\I))))))
    (with-ready-memory-test-connection (connection :input input)
      (let ((promise (query-async connection "SELECT 1")))
        (let ((result (cl-concurrent-kit:await promise)))
          (is (typep result 'query-result))
          (is (string= "SELECT 1" (result-command-tag result))))))))

(deftest cps-query-composes-concurrent-kit-promise
  (let* ((input
           (join-octets
            (make-frame #\C (cstring "SELECT 1"))
            (make-frame #\Z (octets (char-code #\I)))))
         (callback-result nil))
    (with-ready-memory-test-connection (connection :input input)
      (let ((promise
              (query-cps connection "SELECT 1"
                         (lambda (result)
                           (setf callback-result result)
                           result))))
        (let ((result (cl-concurrent-kit:await promise)))
          (is (eq result callback-result))
          (is (typep result 'query-result))
          (is (string= "SELECT 1" (result-command-tag result))))))))

(deftest cps-composition-evaluates-promise-and-continuations-once
  (let ((promise-evaluations 0)
        (success-evaluations 0)
        (error-evaluations 0))
    (let ((success (lambda (value)
                     (incf success-evaluations)
                     (* value 2))))
      (let ((promise
              (cl-postgresql-kit::%query-cps
               (progn
                 (incf promise-evaluations)
                 (cl-concurrent-kit:future 7))
               success
               nil)))
        (is (= 1 promise-evaluations))
        (is (zerop error-evaluations))
        (is (= 14 (cl-concurrent-kit:await promise)))
        (is (= 1 success-evaluations))))))

(deftest query-records-observability-metrics
  (let* ((success-input
           (join-octets
            (make-frame #\C (cstring "SELECT 1"))
            (make-frame #\Z (octets (char-code #\I)))))
         (failure-input
           (join-octets
            (make-frame #\D (octets 0 0))
            (make-frame #\D (octets 0 0))))
         (registry (cl-observability-kit:make-metric-registry))
         (success-connection
           (ready-memory-connection :input success-input
                                    :metric-registry registry))
         (failure-connection
           (ready-memory-connection :input failure-input
                                    :metric-registry registry)))
    (unwind-protect
         (progn
           (is (eq registry
                   (connection-metric-registry success-connection)))
           (is (typep (query success-connection "SELECT 1")
                      'query-result))
           (assert-signals 'query-error
                           (lambda ()
                             (query failure-connection "SELECT 1"
                                    :max-result-rows 1)))
           (let* ((snapshots
                    (cl-observability-kit:metric-snapshot registry))
                  (counter
                    (find "postgresql_operations_total"
                          snapshots
                          :key #'cl-observability-kit:metric-snapshot-name
                          :test #'string=))
                  (duration
                    (find "postgresql_operation_duration_seconds"
                          snapshots
                          :key #'cl-observability-kit:metric-snapshot-name
                          :test #'string=))
                  (counter-samples
                    (cl-observability-kit:metric-snapshot-samples counter))
                  (duration-sample
                    (first
                     (cl-observability-kit:metric-snapshot-samples
                      duration))))
             (is counter)
             (is duration)
             (is (= 2 (length counter-samples)))
             (is (= 2 (cl-observability-kit:metric-sample-count
                       duration-sample)))
             (is-each (status '("error" "success"))
               (let ((sample
                       (find status counter-samples
                             :key (lambda (sample)
                                    (cdr (assoc "status"
                                                (cl-observability-kit:metric-sample-labels
                                                 sample)
                                                :test #'string=)))
                             :test #'string=)))
                 (is sample)
                 (is (= 1
                        (cl-observability-kit:metric-sample-value
                         sample)))
                 (is-alist-values
                     (cl-observability-kit:metric-sample-labels sample)
                   ("operation" "query" string= #'string=))))))
      (disconnect success-connection)
      (disconnect failure-connection))))

(deftest connection-metrics-are-opt-in
  (with-ready-memory-test-connection (connection)
    (is (null (connection-metric-registry connection)))))

(deftest cps-pipeline-delivers-validation-errors
  (let ((callback-condition nil))
    (with-ready-memory-test-connection (connection)
      (let ((promise
              (query-pipeline-cps
               connection '()
               (lambda (results) results)
               :on-error (lambda (condition)
                           (setf callback-condition condition)
                           :handled))))
        (is (eq :handled (cl-concurrent-kit:await promise)))
        (is (typep callback-condition 'parameter-error))))))

(deftest query-pipeline-batches-extended-requests
  (labels ((row-description-frame ()
             (make-frame
              #\T
              (join-octets
               (octets 0 1)
               (cstring "n")
               (octets 0 0 0 0)
               (octets 0 0)
               (octets 0 0 0 23)
               (octets 0 4)
               (octets #xff #xff #xff #xff)
               (octets 0 0))))
           (data-row-frame (value)
             (make-frame #\D (octets 0 1 0 0 0 1 value)))
           (result-input (tag value)
             (join-octets
              (make-frame #\1 #())
              (make-frame #\2 #())
              (row-description-frame)
              (data-row-frame value)
              (make-frame #\C (cstring tag))
              (make-frame #\3 #())
              (make-frame #\3 #())
              (make-frame #\Z (octets (char-code #\I))))))
    (let* ((input (join-octets (result-input "SELECT 1" 49)
                               (result-input "SELECT 2" 50)))
           (connection (ready-memory-connection :input input))
           (transport (connection-transport connection)))
      (unwind-protect
           (let* ((results
                    (query-pipeline
                     connection
                     (list
                      (make-pipeline-request
                       "select $1"
                       :parameters '(1)
                       :parameter-type-oids '(23)
                       :statement-name "pipeline-s1"
                       :portal-name "pipeline-p1")
                      (make-pipeline-request
                       "select $1"
                       :parameters '(2)
                       :parameter-type-oids '(23)
                       :statement-name "pipeline-s2"
                       :portal-name "pipeline-p2"))))
                  (types (memory-transport-frame-types transport)))
             (is (= 2 (length results)))
             (is (string= "SELECT 1" (result-command-tag (first results))))
             (is (string= "SELECT 2" (result-command-tag (second results))))
             (is (= 1 (result-row-count (first results))))
             (is (= 1 (result-row-count (second results))))
             (is (= 1 (row-value (first results) 0 "n")))
             (is (= 2 (row-value (second results) 0 "n")))
             (is (equal
                  '(#\P #\B #\D #\E #\C #\C #\S
                    #\P #\B #\D #\E #\C #\C #\S #\H)
                  types))
             (is (= 1 (count #\H types))))
        (disconnect connection)))))
