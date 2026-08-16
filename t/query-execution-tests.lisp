(in-package #:cl-postgresql-kit/test)

(deftest query-timeout-respects-resilience-deadline
  (with-ready-memory-connection (:query-timeout 30)
    (is (= (cl-postgresql-kit::%effective-query-timeout connection)
           30))
    (let ((effective
            (cl-postgresql-kit::%effective-query-timeout connection 0.1d0)))
      (is (= effective 0.1d0))
      (is (<= effective
              (connection-query-timeout connection))))))

(deftest cancel-transport-prefers-active-hostaddr
  (with-test-connection
      (connection
       (make-connection :host "db.example.test"
                        :hostaddr "192.0.2.44"
                        :port 5544
                        :ssl-mode :disable))
    (let ((transport (cl-postgresql-kit::%make-cancel-transport connection)))
      (unwind-protect
           (progn
             (is (equal "192.0.2.44"
                        (cl-postgresql-kit::socket-transport-host transport)))
             (is (= 5544
                    (cl-postgresql-kit::socket-transport-port transport))))
        (transport-close transport)))))

(deftest query-timeout-uses-resilience-deadline-when-unconfigured
  (with-ready-memory-connection ()
    (is (= (cl-postgresql-kit::%effective-query-timeout connection 0.1d0)
           0.1d0))))

(deftest zero-query-timeout-remains-unbounded-without-deadline
  (with-ready-memory-connection (:query-timeout 0)
    (is (null (cl-postgresql-kit::%effective-query-timeout connection)))))

(deftest query-exchange-state-macro-binds-a-fresh-state
  (let ((connection (gensym "CONNECTION-"))
        (connection-evaluations 0)
        (observed-state nil))
    (cl-postgresql-kit::%with-query-exchange-state
        (state (progn (incf connection-evaluations) connection)
          :max-result-rows 3
          :max-result-bytes 4096)
      (setf observed-state state)
      (is (eq connection
              (cl-postgresql-kit::%query-exchange-state-connection state)))
      (is (= 3
             (cl-postgresql-kit::%query-exchange-state-max-result-rows state)))
      (is (= 4096
             (cl-postgresql-kit::%query-exchange-state-max-result-bytes state)))
      (is (equalp #() (cl-postgresql-kit::%query-exchange-state-columns state)))
      (is (null (cl-postgresql-kit::%query-exchange-state-results state))))
    (is (= 1 connection-evaluations))
    (is (cl-postgresql-kit::query-exchange-state-p observed-state))))

(deftest operation-boundary-macro-evaluates-inputs-once
  (let ((connection-evaluations 0)
        (operation-evaluations 0)
        (body-evaluations 0))
    (with-ready-memory-connection ()
      (is (eql :completed
               (cl-postgresql-kit::%with-operation-boundary
                   ((progn (incf connection-evaluations) connection)
                    :operation
                    (progn (incf operation-evaluations) :test-operation))
                 (incf body-evaluations)
                 :completed)))
      (is (= 1 connection-evaluations))
      (is (= 1 operation-evaluations))
      (is (= 1 body-evaluations)))))

(deftest query-timeout-retires-connection-and-sends-cancel
  (let ((cancel-transport nil))
    (with-test-connection
        (connection
         (ready-memory-connection
          :query-timeout 0.05
          :cancel-transport-factory
          (lambda (ignored-connection)
            (declare (ignore ignored-connection))
            (setf cancel-transport (make-memory-transport)))))
      (setf (connection-backend-process-id connection) 42
            (connection-backend-secret-key connection) 99)
      (handler-case
          (progn
            (cl-postgresql-kit::%call-with-query-timeout
             connection
             (lambda () (sleep 0.2)))
            (fail "Expected query timeout."))
        (timeout-error (condition)
          (is (timeout-error-cancel-sent-p condition))
          (is (timeout-error-connection-retired-p condition))))
      (is cancel-transport)
      (is (equalp (memory-transport-output cancel-transport)
                  (encode-cancel-request 42 99)))
      (is (not (connection-open connection)))
      (is (eq (connection-state connection) :failed)))))

(deftest query-timeout-retires-on-expired-resilience-deadline
  (let ((cancel-transport nil))
    (with-test-connection
        (connection
         (ready-memory-connection
          :cancel-transport-factory
          (lambda (ignored-connection)
            (declare (ignore ignored-connection))
            (setf cancel-transport (make-memory-transport)))))
      (setf (connection-backend-process-id connection) 42
            (connection-backend-secret-key connection) 99)
      (handler-case
          (cl-resilience-kit:with-deadline (:timeout 0.01)
            (sleep 0.05)
            (cl-postgresql-kit::%call-with-query-timeout
             connection
             (lambda () t)))
        (timeout-error (condition)
          (is (timeout-error-cancel-sent-p condition))
          (is (timeout-error-connection-retired-p condition)))
        (cl-resilience-kit:deadline-exceeded ()
          (fail "Expected the query timeout boundary to run first.")))
      (is cancel-transport)
      (is (not (connection-open connection))))))

(deftest query-timeout-without-backend-identity-still-retires
  (with-ready-memory-connection (:query-timeout 0.05)
    (handler-case
        (progn
          (cl-postgresql-kit::%call-with-query-timeout
           connection
           (lambda () (sleep 0.2)))
          (fail "Expected query timeout."))
      (timeout-error (condition)
        (is (null (timeout-error-cancel-sent-p condition)))
        (is (timeout-error-connection-retired-p condition))))))

(cl-weave:it-each
    ((30 nil 30)
     (nil nil nil)
     (0 nil nil)
     (30 5 5))
  "effective query timeout follows configured and remaining values"
  (configured remaining expected)
  (with-ready-memory-connection (:query-timeout configured)
    (let ((actual
            (cl-postgresql-kit::%effective-query-timeout
             connection remaining)))
      (is (eql actual expected)))))

(cl-weave:it-property "pipeline request normalization preserves sequence order"
  ((sqls (cl-weave:gen-list
          (cl-weave:gen-string :min-length 1
                               :max-length 16
                               :alphabet "ab_ ")
          :min-length 1
          :max-length 6)))
  (let* ((list-requests
           (cl-postgresql-kit::%pipeline-request-list sqls))
         (vector-requests
           (cl-postgresql-kit::%pipeline-request-list
            (coerce sqls 'vector))))
    (is (equal sqls
               (mapcar #'cl-postgresql-kit::pipeline-request-sql
                       list-requests)))
    (is (equal sqls
               (mapcar #'cl-postgresql-kit::pipeline-request-sql
                       vector-requests)))))

(cl-weave:it-property "parameter OID normalization preserves valid unsigned values"
  ((oids (cl-weave:gen-list
          (cl-weave:gen-integer :min 0 :max 4294967295)
          :min-length 0
          :max-length 6)))
  (let ((list-oids (cl-postgresql-kit::%parameter-oids-list oids))
        (vector-oids
          (cl-postgresql-kit::%parameter-oids-list
           (coerce oids 'vector))))
    (is (equal oids list-oids))
    (is (equal oids vector-oids))))

(deftest query-parameter-validation-boundaries
  (it-signals-each 'parameter-error
      ((42)
       ("parameters"))
    "rejects invalid parameter sequence input ~S"
    (value)
    (cl-postgresql-kit::%parameter-sequence-list value :parameters))
  (it-signals-each 'parameter-error
      ((:negative-parameter-oid)
       (:invalid-statement-name)
       (:invalid-portal-name)
       (:invalid-request-scalar)
       (:invalid-request-list)
       (:negative-max-rows)
       (:negative-max-result-bytes))
    "rejects invalid query parameter inputs"
    (label)
    (case label
      (:negative-parameter-oid
       (cl-postgresql-kit::%parameter-oids-list '(-1)))
      (:invalid-statement-name
       (make-pipeline-request "select 1"
                              :statement-name 1))
      (:invalid-portal-name
       (make-pipeline-request "select 1"
                              :portal-name 1))
      (:invalid-request-scalar
       (cl-postgresql-kit::%pipeline-request-list 1))
      (:invalid-request-list
       (cl-postgresql-kit::%pipeline-request-list '(1)))
      (:negative-max-rows
       (cl-postgresql-kit::%max-rows-value -1))
      (:negative-max-result-bytes
       (cl-postgresql-kit::%max-result-bytes-value -1))))
  (is-each (formats '((0 1) #(0 1) 0))
    (let ((request (make-pipeline-request
                    "select 1"
                    :parameter-formats formats)))
      (is (null
           (cl-postgresql-kit::pipeline-request-parameter-formats request)))))
  (it-signals-each 'parameter-error
      ((:invalid-statement-name)
       (:invalid-portal-name))
    "rejects invalid pipeline request name case ~A"
    (label)
    (case label
      (:invalid-statement-name
       (make-pipeline-request "select 1"
                              :statement-name 1))
      (:invalid-portal-name
       (make-pipeline-request "select 1"
                              :portal-name 1))))
  (is-each (formats '((0 1) #(0 1) 1))
    (let ((request (make-pipeline-request
                    "select 1"
                    :result-formats formats)))
      (is (equal
           (cond ((listp formats) formats)
                 ((vectorp formats) (coerce formats 'list))
                 (t (list formats)))
           (cl-postgresql-kit::pipeline-request-result-formats request)))))
  (is (string= "select 1"
               (cl-postgresql-kit::pipeline-request-sql
                (first (cl-postgresql-kit::%pipeline-request-list
                        "select 1")))))
  (let ((request (make-pipeline-request "select 1")))
    (is (cl-postgresql-kit::pipeline-request-p
         (first (cl-postgresql-kit::%pipeline-request-list request))))))

(deftest-query-error-retires-connection
    result-row-limit-retires-connection
    (ready-memory-connection
     :input (join-octets
             (make-frame #\D (octets 0 0))
             (make-frame #\D (octets 0 0))))
  (query connection "select 1" :max-result-rows 1))

(deftest-query-error-retires-connection
    result-byte-limit-retires-connection
    (ready-memory-connection
     :input (join-octets
             (make-frame #\D (octets 0 0))
             (make-frame #\D (octets 0 0))))
  (query connection "select 1" :max-result-bytes 2))

(deftest-query-error-retires-connection
    cursor-result-row-limit-retires-connection
    (ready-memory-connection
     :input (large-object-scalar-result-input
             23 (octets 0 0 0 1) "SELECT 1"))
  (open-cursor connection "select 1"
               :max-result-rows 0))

(deftest-query-error-retires-connection
    cursor-result-byte-limit-retires-connection
    (ready-memory-connection
     :input (large-object-scalar-result-input
             23 (octets 0 0 0 1) "SELECT 1"))
  (open-cursor connection "select 1"
               :max-result-bytes 1))
