(in-package #:cl-postgresql-kit/test)

(deftest cursor-open-fetch-close-round-trip
  (let* ((input (large-object-scalar-result-input
                 23 (octets 0 0 0 1) "SELECT 1")))
    (with-test-connection
        (connection
         (ready-memory-connection
          :input input
          :on-write
          (lambda (transport octets)
            (when (and (plusp (length octets))
                       (= (aref octets 0) (char-code #\C)))
              (memory-transport-append-input
               transport
               (make-frame #\Z (octets (char-code #\I))))))))
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
        (is (null (cl-postgresql-kit::connection--active-cursors connection)))))))

(deftest cursor-reuses-prepared-statement-without-closing-it
  (let* ((input
           (join-octets
            (make-frame #\1 #())
            (make-frame #\t (octets 0 1 0 0 0 23))
            (make-frame #\n #())
            (make-frame #\Z (octets (char-code #\I)))
            (large-object-scalar-result-input
             23 (octets 0 0 0 1) "SELECT 1")
            (make-frame #\Z (octets (char-code #\I))))))
    (with-test-transport-connection
        (connection transport (ready-memory-connection :input input))
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
        (is (equal '(#\P #\D #\S #\B #\D #\E #\S #\S)
                   (memory-transport-frame-types transport)))))))

(deftest prepared-statement-keeps-server-parameter-types
  (let* ((input
           (join-octets
            (make-frame #\t (octets 0 1 0 0 0 23))
            (make-frame #\T (octets 0 0))
            (make-frame #\n #())
            (make-frame #\Z (octets (char-code #\I))))))
    (with-test-connection
        (connection (ready-memory-connection :input input))
      (let ((statement (prepare connection "select $1")))
        (is (equal '(23)
                   (prepared-statement-parameter-type-oids statement)))
        (is (equalp #()
                    (prepared-statement-columns statement)))))))

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
      (with-test-connection (connection connection)
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
        (is (eq :ready (connection-state connection)))))
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
      (with-test-connection (connection connection)
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
          (is (eq :ready (connection-state connection))))))))

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
            (make-frame #\Z (octets (char-code #\I))))))
    (with-test-transport-connection
        (connection transport (ready-memory-connection :input input))
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
        (is (equal '(#\P #\D #\S #\B #\D #\E #\S #\C #\S)
                   (memory-transport-frame-types transport)))))))

(deftest prepared-statement-identity-protects-replacement
  (let* ((input
           (join-octets
            (make-frame #\n #())
            (make-frame #\Z (octets (char-code #\I)))
            (make-frame #\Z (octets (char-code #\I)))
            (make-frame #\n #())
            (make-frame #\Z (octets (char-code #\I)))
            (make-frame #\Z (octets (char-code #\I))))))
    (with-test-transport-connection
        (connection transport (ready-memory-connection :input input))
      (let* ((statement-a (prepare connection "select 1" :name "cached")))
        (is (close-prepared statement-a))
        (let ((statement-b (prepare connection "select 1" :name "cached")))
          (it-signals-each 'parameter-error
              ((:execute-closed-statement)
               (:close-closed-statement))
            "rejects prepared statement reuse case ~A"
            (label)
            (case label
              (:execute-closed-statement
               (execute-prepared statement-a))
              (:close-closed-statement
               (close-prepared statement-a))))
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
          (is (equal '(#\P #\D #\S #\C #\S #\P #\D #\S #\C #\S)
                     (memory-transport-frame-types transport))))))))

(cl-weave:it-each
    ((:listen "LISTEN \"events\"\"channel\"")
     (:unlisten "UNLISTEN \"events\"\"channel\"")
     (:unlisten-all "UNLISTEN *")
     (:begin "BEGIN ISOLATION LEVEL SERIALIZABLE READ ONLY DEFERRABLE")
     (:commit "COMMIT")
     (:rollback "ROLLBACK")
     (:savepoint "SAVEPOINT \"sp\"\"1\"")
     (:release-savepoint "RELEASE SAVEPOINT \"sp\"\"1\"")
     (:rollback-to-savepoint "ROLLBACK TO SAVEPOINT \"sp\"\"1\""))
  "command operations render expected SQL"
  (operation-name expected-sql)
  (let* ((input
           (join-octets
            (make-frame #\C (cstring "OK"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input))
         (transport (connection-transport connection)))
    (with-test-connection (connection connection)
      (is (query-result-p
           (case operation-name
             (:listen
              (cl-postgresql-kit:listen connection "events\"channel"))
             (:unlisten
              (cl-postgresql-kit:unlisten connection "events\"channel"))
             (:unlisten-all
              (cl-postgresql-kit:unlisten-all connection))
             (:begin
              (begin-transaction connection :isolation :serializable
                                 :read-only t :deferrable t))
             (:commit
              (commit-transaction connection))
             (:rollback
              (rollback-transaction connection))
             (:savepoint
              (savepoint connection "sp\"1"))
             (:release-savepoint
              (release-savepoint connection "sp\"1"))
             (:rollback-to-savepoint
              (rollback-to-savepoint connection "sp\"1")))))
      (is (equalp (encode-query-message expected-sql)
                  (memory-transport-output transport))))))

(cl-weave:it-each
    ((:empty-listen parameter-error)
     (:nul-listen parameter-error)
     (:empty-savepoint parameter-error)
     (:unsupported-isolation parameter-error))
  "command operations validate invalid input"
  (operation-name condition-type)
  (with-test-connection
      (connection (ready-memory-connection))
    (assert-signals condition-type
                    (lambda ()
                      (case operation-name
                        (:empty-listen
                         (cl-postgresql-kit:listen connection ""))
                        (:nul-listen
                         (cl-postgresql-kit:listen connection
                                                   (format nil "bad~C" #\Null)))
                        (:empty-savepoint
                         (savepoint connection ""))
                        (:unsupported-isolation
                         (begin-transaction connection
                                            :isolation :unsupported)))))))
