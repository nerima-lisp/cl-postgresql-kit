(in-package #:cl-postgresql-kit/test)

(cl-weave:it-each
    ((explicit-formats)
     (default-formats))
  "function call renders expected frontend messages"
  (case-name)
  (let ((input
          (join-octets
           (make-frame #\V (octets 0 0 0 2 111 107))
           (make-frame #\Z (octets (char-code #\I))))))
    (with-ready-memory-connection (:input input)
      (is (equalp
           (octets 111 107)
           (case case-name
             (explicit-formats
              (function-call connection 123 (list (octets 1) +sql-null+)
                             :argument-formats '(0 0)
                             :result-format 0))
             (default-formats
              (function-call connection 123 nil)))))
      (is (equalp
           (memory-transport-output (connection-transport connection))
           (join-octets
            (case case-name
              (explicit-formats
               (encode-function-call-message
                123 (list (octets 1) +sql-null+)
                :argument-formats '(0 0) :result-format 0))
              (default-formats
               (encode-function-call-message 123 nil)))
            (encode-sync-message)))))))

(deftest function-call-protocol-boundaries
  (labels ((ready-input (payload)
             (join-octets
              (make-frame #\V payload)
              (make-frame #\Z (octets (char-code #\I))))))
    (with-ready-memory-connection ()
      (it-signals-each 'parameter-error
          ((-1)
           (#x100000000)
           ("123"))
        "rejects invalid function OID ~S"
        (value)
        (function-call connection value nil)))
    (with-ready-memory-connection (:input (ready-input (octets 0 0 0 2 52 50)))
      (is (= 42 (function-call connection 123 nil :result-type-oid 23))))
    (with-ready-memory-connection (:input (ready-input (octets #xff #xff #xff #xff)))
      (is (sql-null-p
           (function-call connection 123 nil :result-type-oid 23))))
    (let* ((notice-payload
             (join-octets
              (octets (char-code #\S)) (cstring "NOTICE")
              (octets (char-code #\M)) (cstring "function notice")
              (octets 0)))
           (notification-payload
             (join-octets (octets 0 0 0 42)
                          (cstring "events")
                          (cstring "created")))
           (input
             (join-octets
              (make-frame #\N notice-payload)
              (make-frame #\A notification-payload)
              (make-frame #\V (octets 0 0 0 2 111 107))
              (make-frame #\Z (octets (char-code #\I))))))
      (with-ready-memory-connection (:input input)
        (is (equalp (octets 111 107)
                    (function-call connection 123 nil)))
        (is (= 2 (length (connection-notifications connection))))))))

(cl-weave:it-each
    ((server-error-response server-error)
     (ready-only-response protocol-error)
     (duplicate-value-response protocol-error)
     (command-complete-response protocol-error))
  "function call rejects invalid protocol flows"
  (case-name condition-type)
  (with-ready-memory-connection
      (:input
       (case case-name
         (server-error-response
          (join-octets
           (make-frame #\E
                       (join-octets
                        (octets (char-code #\S)) (cstring "ERROR")
                        (octets (char-code #\C)) (cstring "XX000")
                        (octets (char-code #\M)) (cstring "function failed")
                        (octets 0)))
           (make-frame #\Z (octets (char-code #\I)))))
         (ready-only-response
          (make-frame #\Z (octets (char-code #\I))))
         (duplicate-value-response
          (join-octets
           (make-frame #\V (octets 0 0 0 2 111 107))
           (make-frame #\V (octets 0 0 0 2 111 107))
           (make-frame #\Z (octets (char-code #\I)))))
         (command-complete-response
          (join-octets
           (make-frame #\C (cstring "CALL"))
           (make-frame #\Z (octets (char-code #\I)))))))
    (assert-signals condition-type
                    (lambda ()
                      (function-call connection 123 nil)))))
