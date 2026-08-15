(in-package #:cl-postgresql-kit/test)

(deftest large-object-round-trip
  (let* ((input
           (join-octets
            (make-frame #\C (cstring "BEGIN"))
            (make-frame #\Z (octets (char-code #\T)))
            (large-object-scalar-result-input
             26 (octets 0 0 0 42) "SELECT 42")
            (large-object-scalar-result-input
             23 (octets 0 0 0 7) "SELECT 7")
            (large-object-scalar-result-input
             17 (octets 1 2 3) "SELECT 3")
            (large-object-scalar-result-input
             23 (octets 0 0 0 3) "SELECT 3")
            (large-object-scalar-result-input
             20 (octets 0 0 0 0 0 0 0 3) "SELECT 3")
            (large-object-scalar-result-input
             20 (octets 0 0 0 0 0 0 0 3) "SELECT 3")
            (large-object-scalar-result-input
             23 (octets 0 0 0 0) "SELECT 0")
            (large-object-scalar-result-input
             23 (octets 0 0 0 0) "SELECT 0")
            (large-object-scalar-result-input
             23 (octets 0 0 0 1) "SELECT 1")))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (progn
           (assert-signals 'transaction-error
                           (lambda () (large-object-create connection)))
           (is (typep (query connection "BEGIN") 'query-result))
           (let* ((oid (large-object-create connection))
                  (object (large-object-open connection oid
                                             :mode :read-write)))
             (is (= 42 oid))
             (is (= 7 (large-object-descriptor object)))
             (is-case-each
                 ((case-name thunk)
                  (list (list 'read-empty
                              (lambda ()
                                (equalp #() (large-object-read object 0))))
                        (list 'read-filled
                              (lambda ()
                                (equalp (octets 1 2 3)
                                        (large-object-read object 3))))
                        (list 'write
                              (lambda ()
                                (= 3 (large-object-write object
                                                         (octets 4 5 6)))))
                        (list 'seek
                              (lambda ()
                                (= 3 (large-object-seek object 3))))
                        (list 'tell
                              (lambda () (= 3 (large-object-tell object))))
                        (list 'truncate
                              (lambda ()
                                (large-object-truncate object 0)))
                        (list 'close
                              (lambda () (large-object-close object)))
                        (list 'closed-p
                              (lambda () (large-object-closed-p object)))
                        (list 'close-idempotent
                              (lambda () (large-object-close object)))))
               (declare (ignore case-name))
               (is (funcall thunk)))
             (assert-signals 'transaction-error
                             (lambda () (large-object-tell object))))
           (is (large-object-unlink connection 42)))
      (disconnect connection))))

(deftest large-object-server-operations
  (let* ((input
           (join-octets
            (make-frame #\C (cstring "BEGIN"))
            (make-frame #\Z (octets (char-code #\T)))
            (large-object-scalar-result-input
             17 (octets 9 8 7) "SELECT 1")
            (large-object-scalar-result-input
             17 (octets 8 7) "SELECT 2")
            (large-object-scalar-result-input
             26 (octets 0 0 0 55) "SELECT 55")
            (large-object-scalar-result-input
             2278 #() "SELECT 1")
            (large-object-scalar-result-input
             26 (octets 0 0 0 56) "SELECT 56")
            (large-object-scalar-result-input
             26 (octets 0 0 0 57) "SELECT 57")
            (large-object-scalar-result-input
             23 (octets 0 0 0 1) "SELECT 1")))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (progn
           (assert-signals 'transaction-error
                           (lambda () (large-object-read-all connection 42)))
           (is (typep (query connection "BEGIN") 'query-result))
           (is-case-each
               ((case-name thunk)
                (list (list 'read-all
                            (lambda ()
                              (equalp (octets 9 8 7)
                                      (large-object-read-all connection 42))))
                      (list 'read-range
                            (lambda ()
                              (equalp (octets 8 7)
                                      (large-object-read-range connection
                                                               42 1 2))))
                      (list 'from-bytea
                            (lambda ()
                              (= 55
                                 (large-object-from-bytea connection
                                                          (octets 1 2 3)))))
                      (list 'write-at
                            (lambda ()
                              (large-object-write-at connection
                                                     55 2 (octets 4 5 6))))
                      (list 'import-default
                            (lambda ()
                              (= 56
                                 (large-object-import-server-file
                                  connection
                                  "/server/input.bin"))))
                      (list 'import-explicit-oid
                            (lambda ()
                              (= 57
                                 (large-object-import-server-file
                                  connection
                                  "/server/input.bin"
                                  :oid 57))))
                      (list 'export
                            (lambda ()
                              (large-object-export-server-file connection
                                                               57
                                                               "/server/output.bin")))))
             (declare (ignore case-name))
             (is (funcall thunk)))
           (it-signals-each 'parameter-error
               ((:negative-read-start)
                (:server-path-contains-nul))
             "large object helper rejects invalid input case ~A"
             (label)
             (ecase label
               (:negative-read-start
                (large-object-read-range connection 42 -1 1))
               (:server-path-contains-nul
                (large-object-import-server-file
                 connection
                 (format nil "server~Cpath" (code-char 0)))))))
      (disconnect connection))))
