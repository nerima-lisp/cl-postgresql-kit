(in-package #:cl-postgresql-kit/test)

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
             (is-alist-values (notice-fields (first empty-notices))
               (:message "empty" string=))
             (is (result-portal-suspended-p suspended-result))
             (is (= 0 (result-row-count suspended-result)))
             (is (eq :in-transaction
                     (result-transaction-status suspended-result)))
             (is (= 1 (length suspended-notices)))
             (is-alist-values (notice-fields (first suspended-notices))
               (:message "suspended" string=)))
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
           (is-connection-values signaled
             (server-error-sqlstate "42601" string=)
             (server-error-message "pipeline failed" string=))
           (is (connection-open connection))
           (is (eq :ready (connection-state connection))))
      (disconnect connection))))

(deftest-condition-preserves-connection
    query-pipeline-rejects-multiple-results-per-request
    multiple-results-error
    (ready-memory-connection
     :input
     (join-octets
      (make-frame #\C (cstring "SELECT 1"))
      (make-frame #\C (cstring "SELECT 2"))
      (make-frame #\Z (octets (char-code #\I)))))
  (query-pipeline connection '("select 1")))

(deftest-condition-retires-connection
    query-pipeline-rejects-copy-responses
    copy-error
    (ready-memory-connection :input (make-frame #\G (copy-response-payload)))
  (query-pipeline connection '("copy source")))

(deftest-condition-retires-connection
    query-rejects-copy-both-response
    copy-error
    (ready-memory-connection :input (make-frame #\W (copy-response-payload)))
  (query connection "start_replication slot"))

(deftest-condition-retires-connection
    query-pipeline-result-row-limit-retires-connection
    query-error
    (ready-memory-connection
     :input (join-octets
             (make-frame #\1 #())
             (make-frame #\2 #())
             (make-frame #\D (octets 0 0))))
  (query-pipeline connection '("select 1")
                  :max-result-rows 0))

(deftest-condition-retires-connection
    query-pipeline-result-byte-limit-retires-connection
    query-error
    (ready-memory-connection
     :input (join-octets
             (make-frame #\1 #())
             (make-frame #\2 #())
             (make-frame #\D (octets 0 0))))
  (query-pipeline connection '("select 1")
                  :max-result-bytes 1))

(deftest-condition-preserves-connection
    multiple-query-results-are-rejected
    multiple-results-error
    (ready-memory-connection
     :input
     (join-octets
      (make-frame #\C (cstring "SELECT 1"))
      (make-frame #\C (cstring "SELECT 2"))
      (make-frame #\Z (octets (char-code #\I)))))
  (query connection "select 1; select 2"))

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
