(in-package #:cl-postgresql-kit/test)

(deftest query-timeout-respects-resilience-deadline
  (let ((connection (ready-memory-connection :query-timeout 30)))
    (unwind-protect
         (progn
           (is (= (cl-postgresql-kit::%effective-query-timeout connection)
                  30))
           (cl-resilience-kit:with-deadline (:timeout 0.1)
             (let ((effective
                     (cl-postgresql-kit::%effective-query-timeout connection)))
               (is (plusp effective))
               (is (<= effective 0.1))
               (is (<= effective
                       (connection-query-timeout connection))))))
      (disconnect connection))))

(deftest query-timeout-uses-resilience-deadline-when-unconfigured
  (let ((connection (ready-memory-connection)))
    (unwind-protect
         (cl-resilience-kit:with-deadline (:timeout 0.1)
           (let ((effective
                   (cl-postgresql-kit::%effective-query-timeout connection)))
             (is (plusp effective))
             (is (<= effective 0.1))))
      (disconnect connection))))

(deftest zero-query-timeout-remains-unbounded-without-deadline
  (let ((connection (ready-memory-connection :query-timeout 0)))
    (unwind-protect
         (is (null (cl-postgresql-kit::%effective-query-timeout connection)))
      (disconnect connection))))

(deftest query-timeout-retires-connection-and-sends-cancel
  (let* ((cancel-transport nil)
         (connection
           (ready-memory-connection
            :query-timeout 0.05
            :cancel-transport-factory
            (lambda (ignored-connection)
              (declare (ignore ignored-connection))
              (setf cancel-transport (make-memory-transport))))))
    (unwind-protect
         (progn
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
           (is (eq (connection-state connection) :failed)))
      (disconnect connection))))

(deftest query-timeout-retires-on-expired-resilience-deadline
  (let* ((cancel-transport nil)
         (connection
           (ready-memory-connection
            :cancel-transport-factory
            (lambda (ignored-connection)
              (declare (ignore ignored-connection))
              (setf cancel-transport (make-memory-transport))))))
    (unwind-protect
         (progn
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
           (is (not (connection-open connection))))
      (disconnect connection))))

(deftest query-timeout-without-backend-identity-still-retires
  (let ((connection (ready-memory-connection :query-timeout 0.05)))
    (unwind-protect
         (handler-case
             (progn
               (cl-postgresql-kit::%call-with-query-timeout
                connection
                (lambda () (sleep 0.2)))
               (fail "Expected query timeout."))
           (timeout-error (condition)
             (is (null (timeout-error-cancel-sent-p condition)))
             (is (timeout-error-connection-retired-p condition))))
      (disconnect connection))))

(cl-weave:it-each
    ((30 nil 30)
     (nil nil nil)
     (0 nil nil)
     (30 5 5))
  "effective query timeout follows configured and remaining values"
  (configured remaining expected)
  (let ((connection (ready-memory-connection :query-timeout configured)))
    (unwind-protect
         (let ((actual
                 (cl-postgresql-kit::%effective-query-timeout
                  connection remaining)))
           (is (eql actual expected)))
      (disconnect connection))))

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
  (dolist (value (list 42 "parameters"))
    (assert-signals 'parameter-error
                    (lambda ()
                      (cl-postgresql-kit::%parameter-sequence-list
                       value :parameters))))
  (assert-signals 'parameter-error
                  (lambda ()
                    (cl-postgresql-kit::%parameter-oids-list '(-1))))
  (dolist (formats (list '(0 1) #(0 1) 0))
    (let ((request (make-pipeline-request
                    "select 1"
                    :parameter-formats formats)))
      (is (null
           (cl-postgresql-kit::pipeline-request-parameter-formats request)))))
  (assert-signals 'parameter-error
                  (lambda ()
                    (make-pipeline-request "select 1"
                                           :statement-name 1)))
  (assert-signals 'parameter-error
                  (lambda ()
                    (make-pipeline-request "select 1"
                                           :portal-name 1)))
  (dolist (formats (list '(0 1) #(0 1) 1))
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
         (first (cl-postgresql-kit::%pipeline-request-list request)))))
  (assert-signals 'parameter-error
                  (lambda ()
                    (cl-postgresql-kit::%pipeline-request-list 1)))
  (assert-signals 'parameter-error
                  (lambda ()
                    (cl-postgresql-kit::%pipeline-request-list '(1))))
  (assert-signals 'parameter-error
                  (lambda ()
                    (cl-postgresql-kit::%max-rows-value -1)))
  (assert-signals 'parameter-error
                  (lambda ()
                    (cl-postgresql-kit::%max-result-bytes-value -1))))

(deftest result-row-limit-retires-connection
  (let* ((input (join-octets
                 (make-frame #\D (octets 0 0))
                 (make-frame #\D (octets 0 0))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (progn
           (assert-signals 'query-error
                          (lambda ()
                            (query connection "select 1" :max-result-rows 1)))
           (is (not (connection-open connection)))
           (is (eq (connection-state connection) :failed)))
      (disconnect connection))))

(deftest result-byte-limit-retires-connection
  (let* ((input (join-octets
                 (make-frame #\D (octets 0 0))
                 (make-frame #\D (octets 0 0))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (progn
           (assert-signals 'query-error
                          (lambda ()
                            (query connection "select 1" :max-result-bytes 2)))
           (is (not (connection-open connection)))
           (is (eq (connection-state connection) :failed)))
      (disconnect connection))))

(deftest cursor-result-row-limit-retires-connection
  (let* ((input (large-object-scalar-result-input
                 23 (octets 0 0 0 1) "SELECT 1"))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (progn
           (assert-signals 'query-error
                          (lambda ()
                            (open-cursor connection "select 1"
                                         :max-result-rows 0)))
           (is (not (connection-open connection)))
           (is (eq (connection-state connection) :failed)))
      (disconnect connection))))

(deftest cursor-result-byte-limit-retires-connection
  (let* ((input (large-object-scalar-result-input
                 23 (octets 0 0 0 1) "SELECT 1"))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (progn
           (assert-signals 'query-error
                          (lambda ()
                            (open-cursor connection "select 1"
                                         :max-result-bytes 1)))
           (is (not (connection-open connection)))
           (is (eq (connection-state connection) :failed)))
      (disconnect connection))))

(deftest cursor-open-fetch-close-round-trip
  (let* ((input (large-object-scalar-result-input
                 23 (octets 0 0 0 1) "SELECT 1"))
         (connection
           (ready-memory-connection
            :input input
            :on-write
            (lambda (transport octets)
              (when (and (plusp (length octets))
                         (= (aref octets 0) (char-code #\C)))
                (memory-transport-append-input
                 transport
                 (make-frame #\Z (octets (char-code #\I)))))))))
    (unwind-protect
         (let ((cursor (open-cursor connection "select 1" :fetch-size 1)))
           (multiple-value-bind (rows done-p) (cursor-fetch cursor)
             (is (= 1 (length rows)))
             (is done-p)
             (is (= 1 (aref (aref rows 0) 0)))
             (is (cursor-done-p cursor))
             (is (not (cursor-suspended-p cursor))))
           (is (string= "SELECT 1" (cursor-command-tag cursor)))
           (is (cursor-close cursor))
           (is (cursor-closed-p cursor))
           (is (null (cl-postgresql-kit::connection--active-cursors connection))))
      (disconnect connection))))

(deftest cursor-reuses-prepared-statement-without-closing-it
  (let* ((input
           (join-octets
            (make-frame #\1 #())
            (make-frame #\t (octets 0 1 0 0 0 23))
            (make-frame #\n #())
            (make-frame #\Z (octets (char-code #\I)))
            (large-object-scalar-result-input
             23 (octets 0 0 0 1) "SELECT 1")
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input))
         (transport nil))
    (setf transport (connection-transport connection))
    (unwind-protect
         (let* ((statement (prepare connection "select $1" :name "cached"))
                (cursor
                  (open-cursor connection "select $1"
                               :parameters '(1)
                               :statement-name "cached"
                               :fetch-size 1)))
           (multiple-value-bind (rows done-p) (cursor-fetch cursor)
             (is (= 1 (length rows)))
             (is done-p)
             (is (= 1 (aref (aref rows 0) 0))))
           (is (cursor-close cursor))
           (is (eq statement
                   (gethash "cached"
                            (cl-postgresql-kit::connection--prepared-statements
                             connection))))
           (labels ((frame-types (wire)
                      (loop with position = 0
                            while (< position (length wire))
                            for frame-length =
                              (+ 1 (logior (ash (aref wire (+ position 1)) 24)
                                           (ash (aref wire (+ position 2)) 16)
                                           (ash (aref wire (+ position 3)) 8)
                                           (aref wire (+ position 4))))
                            for frame =
                              (parse-frame
                               (subseq wire position (+ position frame-length)))
                            collect (code-char (backend-message-type frame))
                            do (incf position frame-length))))
             (is (equal '(#\P #\D #\S #\B #\D #\E #\S #\S)
                        (frame-types (memory-transport-output transport))))))
      (disconnect connection))))

(deftest prepared-statement-keeps-server-parameter-types
  (let* ((input
           (join-octets
            (make-frame #\t (octets 0 1 0 0 0 23))
            (make-frame #\T (octets 0 0))
            (make-frame #\n #())
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (let ((statement (prepare connection "select $1")))
           (is (equal '(23)
                      (prepared-statement-parameter-type-oids statement)))
           (is (equalp #()
                       (prepared-statement-columns statement))))
      (disconnect connection))))

(deftest prepared-statement-server-errors-preserve-cache-state
  (labels ((error-payload (message)
             (join-octets
              (octets (char-code #\S)) (cstring "ERROR")
              (octets (char-code #\C)) (cstring "42601")
              (octets (char-code #\M)) (cstring message)
              (octets 0)))
           (capture-server-error (thunk)
             (handler-case
                 (progn (funcall thunk) nil)
               (server-error (condition)
                 condition))))
    (let* ((connection
             (ready-memory-connection
              :input
              (join-octets
               (make-frame #\E (error-payload "prepare failed"))
               (make-frame #\Z (octets (char-code #\I))))))
           (condition nil))
      (unwind-protect
           (progn
             (setf condition
                   (capture-server-error
                    (lambda () (prepare connection "select 1" :name "broken"))))
             (is (typep condition 'server-error))
             (is (string= "42601" (server-error-sqlstate condition)))
             (is (string= "prepare failed" (server-error-message condition)))
             (is (null
                  (gethash "broken"
                           (cl-postgresql-kit::connection--prepared-statements
                            connection))))
             (is (connection-open connection))
             (is (eq :ready (connection-state connection))))
        (disconnect connection)))
    (let* ((connection
             (ready-memory-connection
              :input
              (join-octets
               (make-frame #\t (octets 0 1 0 0 0 23))
               (make-frame #\n #())
               (make-frame #\Z (octets (char-code #\I)))
               (make-frame #\E (error-payload "close failed"))
               (make-frame #\Z (octets (char-code #\I))))))
           (condition nil))
      (unwind-protect
           (let ((statement (prepare connection "select $1" :name "cached")))
             (setf condition
                   (capture-server-error
                    (lambda () (close-prepared statement))))
             (is (typep condition 'server-error))
             (is (string= "42601" (server-error-sqlstate condition)))
             (is (string= "close failed" (server-error-message condition)))
             (is (eq statement
                     (gethash "cached"
                              (cl-postgresql-kit::connection--prepared-statements
                               connection))))
             (is (connection-open connection))
             (is (eq :ready (connection-state connection))))
        (disconnect connection)))))

(deftest prepared-statement-execute-and-close-round-trip
  (let* ((input
           (join-octets
            (make-frame #\1 #())
            (make-frame #\t (octets 0 1 0 0 0 23))
            (make-frame #\n #())
            (make-frame #\Z (octets (char-code #\I)))
            (make-frame #\2 #())
            (make-frame #\T (octets 0 0))
            (make-frame #\C (cstring "SELECT 1"))
            (make-frame #\Z (octets (char-code #\I)))
            (make-frame #\3 #())
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input))
         (transport nil))
    (setf transport (connection-transport connection))
    (unwind-protect
         (let* ((statement (prepare connection "select $1" :name "cached"))
                (result (execute-prepared statement :parameters '(1))))
           (is (equal '(23)
                      (prepared-statement-parameter-type-oids statement)))
           (is (query-result-p result))
           (is (string= "SELECT 1" (result-command-tag result)))
           (is (zerop (result-row-count result)))
           (is (close-prepared statement))
           (is (null
                (gethash "cached"
                         (cl-postgresql-kit::connection--prepared-statements
                          connection))))
           (labels ((frame-types (wire)
                      (loop with position = 0
                            while (< position (length wire))
                            for frame-length =
                              (+ 1 (logior (ash (aref wire (+ position 1)) 24)
                                           (ash (aref wire (+ position 2)) 16)
                                           (ash (aref wire (+ position 3)) 8)
                                           (aref wire (+ position 4))))
                            for frame =
                              (parse-frame
                               (subseq wire position (+ position frame-length)))
                            collect (code-char (backend-message-type frame))
                            do (incf position frame-length))))
             (is (equal '(#\P #\D #\S #\B #\D #\E #\S #\C #\S)
                        (frame-types (memory-transport-output transport))))))
      (disconnect connection))))

(deftest prepared-statement-identity-protects-replacement
  (let* ((input
           (join-octets
            (make-frame #\n #())
            (make-frame #\Z (octets (char-code #\I)))
            (make-frame #\Z (octets (char-code #\I)))
            (make-frame #\n #())
            (make-frame #\Z (octets (char-code #\I)))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input))
         (transport nil))
    (setf transport (connection-transport connection))
    (unwind-protect
         (let* ((statement-a (prepare connection "select 1" :name "cached")))
           (is (close-prepared statement-a))
           (let ((statement-b (prepare connection "select 1" :name "cached")))
             (assert-signals 'parameter-error
                             (lambda () (execute-prepared statement-a)))
             (assert-signals 'parameter-error
                             (lambda () (close-prepared statement-a)))
             (is (eq statement-b
                     (gethash "cached"
                              (cl-postgresql-kit::connection--prepared-statements
                               connection))))
             (is (connection-open connection))
             (is (eq :ready (connection-state connection)))
             (is (close-prepared statement-b))
             (is (close-prepared statement-b))
             (is (null
                  (gethash "cached"
                           (cl-postgresql-kit::connection--prepared-statements
                            connection))))
             (labels ((frame-types (wire)
                        (loop with position = 0
                              while (< position (length wire))
                              for frame-length =
                                (+ 1 (logior (ash (aref wire (+ position 1)) 24)
                                             (ash (aref wire (+ position 2)) 16)
                                             (ash (aref wire (+ position 3)) 8)
                                             (aref wire (+ position 4))))
                              for frame =
                                (parse-frame
                                 (subseq wire position (+ position frame-length)))
                              collect (code-char (backend-message-type frame))
                              do (incf position frame-length))))
               (is (equal '(#\P #\D #\S #\C #\S #\P #\D #\S #\C #\S)
                          (frame-types (memory-transport-output transport)))))))
      (disconnect connection))))

(deftest command-operations-quote-identifiers-and-validate-input
  (labels ((command-result-input ()
             (apply #'join-octets
                    (loop repeat 9
                          append (list (make-frame #\C (cstring "OK"))
                                       (make-frame #\Z
                                                   (octets (char-code #\I))))))))
    (let* ((connection
             (ready-memory-connection :input (command-result-input)))
           (transport (connection-transport connection)))
      (unwind-protect
           (let ((results
                   (list
                    (cl-postgresql-kit:listen connection "events\"channel")
                    (cl-postgresql-kit:unlisten connection "events\"channel")
                    (cl-postgresql-kit:unlisten-all connection)
                    (begin-transaction connection :isolation :serializable
                                       :read-only t :deferrable t)
                    (commit-transaction connection)
                    (rollback-transaction connection)
                    (savepoint connection "sp\"1")
                    (release-savepoint connection "sp\"1")
                    (rollback-to-savepoint connection "sp\"1"))))
             (is (every #'query-result-p results))
             (is (equalp
                  (join-octets
                   (encode-query-message "LISTEN \"events\"\"channel\"")
                   (encode-query-message "UNLISTEN \"events\"\"channel\"")
                   (encode-query-message "UNLISTEN *")
                   (encode-query-message
                    "BEGIN ISOLATION LEVEL SERIALIZABLE READ ONLY DEFERRABLE")
                   (encode-query-message "COMMIT")
                   (encode-query-message "ROLLBACK")
                   (encode-query-message "SAVEPOINT \"sp\"\"1\"")
                   (encode-query-message "RELEASE SAVEPOINT \"sp\"\"1\"")
                   (encode-query-message
                    "ROLLBACK TO SAVEPOINT \"sp\"\"1\""))
                  (memory-transport-output transport)))
             (assert-signals 'parameter-error
                             (lambda () (cl-postgresql-kit:listen connection "")))
             (assert-signals 'parameter-error
                             (lambda ()
                               (cl-postgresql-kit:listen connection
                                                          (format nil "bad~C"
                                                                  #\Null))))
             (assert-signals 'parameter-error
                             (lambda () (savepoint connection "")))
             (assert-signals 'parameter-error
                             (lambda ()
                               (begin-transaction connection
                                                   :isolation :unsupported))))
        (disconnect connection)))))

(deftest cps-callback-validation
  (let ((connection (ready-memory-connection)))
    (unwind-protect
         (progn
           (assert-signals 'type-error
                           (lambda ()
                             (query-cps connection "select 1"
                                        (lambda (result) result)
                                        :on-error 1)))
           (assert-signals 'type-error
                           (lambda ()
                             (query-pipeline-cps connection '()
                                                  (lambda (results) results)
                                                  :on-error 1))))
      (disconnect connection))))

(deftest pipeline-prepared-statement-cache-validation
  (let ((connection (ready-memory-connection)))
    (unwind-protect
         (progn
           (setf (gethash "invalid"
                          (cl-postgresql-kit::connection--prepared-statements
                           connection))
                 :invalid)
           (assert-signals 'parameter-error
                          (lambda ()
                            (cl-postgresql-kit::%pipeline-request-frames
                             connection
                             (make-pipeline-request
                              "select 1"
                              :statement-name "invalid"))))
           (setf (gethash "cached"
                          (cl-postgresql-kit::connection--prepared-statements
                           connection))
                 (cl-postgresql-kit::%make-prepared-statement
                  :name "cached"
                  :sql "select 2"
                  :connection connection))
           (assert-signals 'parameter-error
                          (lambda ()
                            (cl-postgresql-kit::%pipeline-request-frames
                             connection
                             (make-pipeline-request
                              "select 1"
                              :statement-name "cached"))))
           (setf (gethash "cached"
                          (cl-postgresql-kit::connection--prepared-statements
                           connection))
                 (cl-postgresql-kit::%make-prepared-statement
                  :name "cached"
                  :sql "select $1"
                  :parameter-type-oids '(23)
                  :connection connection))
           (assert-signals 'parameter-error
                          (lambda ()
                            (cl-postgresql-kit::%pipeline-request-frames
                             connection
                             (make-pipeline-request
                              "select $1"
                              :parameters '(1)
                              :parameter-type-oids '(25)
                              :statement-name "cached")))))
      (disconnect connection))))

(deftest invalid-parameter-cardinality-writes-nothing
  (let* ((connection (ready-memory-connection))
         (transport (connection-transport connection)))
    (unwind-protect
         (progn
           (assert-signals 'parameter-error
                          (lambda ()
                            (query connection "select $1"
                                   :parameters (list "x")
                                   :parameter-type-oids (list 23 25))))
           (is (zerop (length (memory-transport-output transport))))
           (is (connection-open connection))
           (is (eq (connection-state connection) :ready)))
      (disconnect connection))))

(deftest async-query-uses-concurrent-kit
  (let* ((input
           (join-octets
            (make-frame #\C (cstring "SELECT 1"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input))
         (promise (query-async connection "SELECT 1")))
    (unwind-protect
         (let ((result (cl-concurrent-kit:await promise)))
           (is (typep result 'query-result))
           (is (string= "SELECT 1" (result-command-tag result))))
      (disconnect connection))))

(deftest cps-query-composes-concurrent-kit-promise
  (let* ((input
           (join-octets
            (make-frame #\C (cstring "SELECT 1"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input))
         (callback-result nil)
         (promise
           (query-cps connection "SELECT 1"
                      (lambda (result)
                        (setf callback-result result)
                        result))))
    (unwind-protect
         (let ((result (cl-concurrent-kit:await promise)))
           (is (eq result callback-result))
           (is (typep result 'query-result))
           (is (string= "SELECT 1" (result-command-tag result))))
      (disconnect connection))))

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
             (dolist (status '("error" "success"))
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
                 (is (string= "query"
                              (cdr (assoc "operation"
                                          (cl-observability-kit:metric-sample-labels
                                           sample)
                                          :test #'string=))))))))
      (disconnect success-connection)
      (disconnect failure-connection))))

(deftest connection-metrics-are-opt-in
  (let ((connection (ready-memory-connection)))
    (unwind-protect
         (is (null (connection-metric-registry connection)))
      (disconnect connection))))

(deftest cps-pipeline-delivers-validation-errors
  (let* ((connection (ready-memory-connection))
         (callback-condition nil)
         (promise
           (query-pipeline-cps
            connection '()
            (lambda (results) results)
            :on-error (lambda (condition)
                        (setf callback-condition condition)
                        :handled))))
    (unwind-protect
         (progn
           (is (eq :handled (cl-concurrent-kit:await promise)))
           (is (typep callback-condition 'parameter-error)))
      (disconnect connection))))

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
            (make-frame #\Z (octets (char-code #\I)))))
         (frame-types (wire)
           (loop with position = 0
                 while (< position (length wire))
                 for frame-length =
                   (+ 1
                      (logior (ash (aref wire (+ position 1)) 24)
                              (ash (aref wire (+ position 2)) 16)
                              (ash (aref wire (+ position 3)) 8)
                              (aref wire (+ position 4))))
                 for frame = (parse-frame
                              (subseq wire position (+ position frame-length)))
                 collect (code-char (backend-message-type frame))
                 do (incf position frame-length))))
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
                  (types (frame-types (memory-transport-output transport))))
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

(deftest query-pipeline-preserves-empty-and-suspended-results
  (flet ((notice-frame (message)
           (make-frame
            #\N
            (join-octets (octets (char-code #\S)) (cstring "NOTICE")
                         (octets (char-code #\M)) (cstring message)
                         (octets 0)))))
    (let* ((input
             (join-octets
              (make-frame #\I #())
              (notice-frame "empty")
              (make-frame #\Z (octets (char-code #\I)))
              (make-frame #\s #())
              (notice-frame "suspended")
              (make-frame #\Z (octets (char-code #\T)))))
           (connection (ready-memory-connection :input input)))
      (unwind-protect
           (let* ((results (query-pipeline connection '("select 1" "select 2")))
                  (empty-result (first results))
                  (suspended-result (second results))
                  (empty-notices (result-notices empty-result))
                  (suspended-notices (result-notices suspended-result)))
             (is (= 2 (length results)))
             (is (null (result-command-tag empty-result)))
             (is (= 0 (result-row-count empty-result)))
             (is (eq :idle (result-transaction-status empty-result)))
             (is (not (result-portal-suspended-p empty-result)))
             (is (= 1 (length empty-notices)))
             (is (string= "empty"
                          (cdr (assoc :message
                                      (notice-fields (first empty-notices))))))
             (is (result-portal-suspended-p suspended-result))
             (is (= 0 (result-row-count suspended-result)))
             (is (eq :in-transaction
                     (result-transaction-status suspended-result)))
             (is (= 1 (length suspended-notices)))
             (is (string= "suspended"
                          (cdr (assoc :message
                                      (notice-fields (first suspended-notices)))))))
        (disconnect connection)))))

(deftest query-pipeline-reports-server-error-after-ready-for-query
  (let* ((error-payload
           (join-octets
            (octets (char-code #\S)) (cstring "ERROR")
            (octets (char-code #\C)) (cstring "42601")
            (octets (char-code #\M)) (cstring "pipeline failed")
            (octets 0)))
         (input
           (join-octets
            (make-frame #\E error-payload)
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input))
         (signaled nil))
    (unwind-protect
         (progn
           (handler-case (query-pipeline connection '("select 1"))
             (server-error (condition)
               (setf signaled condition)))
           (is (typep signaled 'server-error))
           (is (string= "42601" (server-error-sqlstate signaled)))
           (is (string= "pipeline failed" (server-error-message signaled)))
           (is (connection-open connection))
           (is (eq :ready (connection-state connection))))
      (disconnect connection))))

(deftest query-pipeline-rejects-multiple-results-per-request
  (let* ((input
           (join-octets
            (make-frame #\C (cstring "SELECT 1"))
            (make-frame #\C (cstring "SELECT 2"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (progn
           (assert-signals 'multiple-results-error
                          (lambda ()
                            (query-pipeline connection '("select 1"))))
           (is (connection-open connection))
           (is (eq :ready (connection-state connection))))
      (disconnect connection))))

(deftest query-pipeline-rejects-copy-responses
  (let* ((input (make-frame #\G (copy-response-payload)))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (progn
           (assert-signals 'copy-error
                          (lambda ()
                            (query-pipeline connection '("copy source"))))
           (is (not (connection-open connection)))
           (is (eq :failed (connection-state connection))))
      (disconnect connection))))

(deftest query-rejects-copy-both-response
  (let* ((input (make-frame #\W (copy-response-payload)))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (progn
           (assert-signals 'copy-error
                          (lambda ()
                            (query connection "start_replication slot")))
           (is (not (connection-open connection)))
           (is (eq :failed (connection-state connection))))
      (disconnect connection))))

(deftest query-pipeline-result-row-limit-retires-connection
  (let* ((input (join-octets
                 (make-frame #\1 #())
                 (make-frame #\2 #())
                 (make-frame #\D (octets 0 0))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (progn
           (assert-signals 'query-error
                          (lambda ()
                            (query-pipeline connection '("select 1")
                                             :max-result-rows 0)))
           (is (not (connection-open connection)))
           (is (eq (connection-state connection) :failed)))
      (disconnect connection))))

(deftest query-pipeline-result-byte-limit-retires-connection
  (let* ((input (join-octets
                 (make-frame #\1 #())
                 (make-frame #\2 #())
                 (make-frame #\D (octets 0 0))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (progn
           (assert-signals 'query-error
                          (lambda ()
                            (query-pipeline connection '("select 1")
                                             :max-result-bytes 1)))
           (is (not (connection-open connection)))
           (is (eq (connection-state connection) :failed)))
      (disconnect connection))))

(deftest multiple-query-results-are-rejected
  (let* ((input
           (join-octets
            (make-frame #\C (cstring "SELECT 1"))
            (make-frame #\C (cstring "SELECT 2"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (progn
           (assert-signals 'multiple-results-error
                          (lambda () (query connection "select 1; select 2")))
           (is (connection-open connection))
           (is (eq (connection-state connection) :ready)))
      (disconnect connection))))

(deftest multiple-query-results-can-be-collected
  (let* ((input
           (join-octets
            (make-frame #\C (cstring "SELECT 1"))
            (make-frame #\C (cstring "SELECT 2"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input))
         (transport (connection-transport connection)))
    (unwind-protect
         (let ((results (query-all connection "select 1; select 2")))
           (is (= 2 (length results)))
           (is (string= "SELECT 1" (result-command-tag (first results))))
           (is (string= "SELECT 2" (result-command-tag (second results))))
           (is (eq (second results) (connection-last-result connection)))
           (memory-transport-append-input
            transport
            (join-octets
             (make-frame #\C (cstring "SELECT 3"))
             (make-frame #\C (cstring "SELECT 4"))
             (make-frame #\Z (octets (char-code #\I)))))
           (let ((query-results (query-all connection "select 3; select 4")))
             (is (= 2 (length query-results)))
             (is (string= "SELECT 3"
                          (result-command-tag (first query-results))))
             (is (string= "SELECT 4"
                          (result-command-tag (second query-results))))))
      (disconnect connection))))

(deftest multiple-query-results-keep-notices-with-result
  (flet ((notice-frame (message)
           (make-frame
            #\N
            (join-octets (octets (char-code #\S)) (cstring "NOTICE")
                         (octets (char-code #\M)) (cstring message)
                         (octets 0)))))
    (let* ((input
             (join-octets
              (notice-frame "first")
              (make-frame #\C (cstring "SELECT 1"))
              (notice-frame "middle")
              (make-frame #\C (cstring "SELECT 2"))
              (notice-frame "last")
              (make-frame #\Z (octets (char-code #\I)))))
           (connection (ready-memory-connection :input input)))
      (unwind-protect
           (let* ((results (query-all connection "select 1; select 2"))
                  (first-notices (result-notices (first results)))
                  (second-notices (result-notices (second results))))
             (is (= 2 (length results)))
             (is (= 1 (length first-notices)))
             (is (string= "first"
                          (cdr (assoc :message
                                      (notice-fields (first first-notices))))))
             (is (= 2 (length second-notices)))
             (is (string= "middle"
                          (cdr (assoc :message
                                      (notice-fields (first second-notices))))))
             (is (string= "last"
                          (cdr (assoc :message
                                      (notice-fields (second second-notices))))))
             (is (= 3 (length (connection-notifications connection)))))
        (disconnect connection)))))

(deftest copy-out-preserves-server-error
  (let* ((error-payload
           (join-octets
            (octets (char-code #\S)) (cstring "ERROR")
            (octets (char-code #\C)) (cstring "42601")
            (octets (char-code #\M)) (cstring "copy failed")
            (octets 0)))
         (input
           (join-octets
            (make-frame #\H (copy-response-payload))
            (make-frame #\E error-payload)
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (let ((operation (copy-out connection "copy source to stdout"))
               (signaled nil))
           (handler-case (copy-out-read operation)
             (server-error (condition)
               (setf signaled condition)))
           (is (typep signaled 'server-error))
           (is (string= "42601" (server-error-sqlstate signaled)))
           (is (string= "copy failed" (server-error-message signaled)))
           (is (not (connection-open connection)))
           (is (eq (connection-state connection) :failed)))
      (disconnect connection))))

(deftest portal-suspension-is-explicit
  (let* ((input
           (join-octets
            (make-frame #\s #())
            (make-frame #\Z (octets (char-code #\T)))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (let ((result (query connection "select $1"
                              :parameters (list "value")
                              :max-rows 1)))
           (is (result-portal-suspended-p result))
           (is (= 0 (result-row-count result)))
           (is (eq :in-transaction
                   (result-transaction-status result))))
      (disconnect connection))))
