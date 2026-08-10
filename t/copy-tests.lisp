(in-package #:cl-postgresql-kit/test)

(deftest copy-in-and-out
  (let* ((copy-in-input
           (join-octets
            (make-frame #\G (copy-response-payload))
            (make-frame #\C (cstring "COPY 1"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input copy-in-input)))
    (unwind-protect
         (let ((operation (copy-in connection "copy target from stdin")))
           (assert-signals 'copy-error
                          (lambda () (query connection "select 1")))
           (is (copy-in-write operation "one-row"))
           (is (string= (copy-in-finish operation) "COPY 1"))
           (is (equalp
                (memory-transport-output (connection-transport connection))
                (join-octets
                 (encode-query-message "copy target from stdin")
                 (encode-copy-data-message "one-row")
                 (encode-copy-done-message)))))
      (disconnect connection)))
  (let* ((copy-out-input
           (join-octets
            (make-frame #\H (copy-response-payload))
            (make-frame #\d (octets 102 111 111))
            (make-frame #\c #())
            (make-frame #\C (cstring "COPY 1"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input copy-out-input)))
    (unwind-protect
         (let ((operation (copy-out connection "copy source to stdout")))
           (is (equalp (copy-out-read operation) (octets 102 111 111)))
           (is (null (copy-out-read operation)))
           (is (equalp
                (memory-transport-output (connection-transport connection))
                (encode-query-message "copy source to stdout"))))
      (disconnect connection))))

(deftest copy-in-abort-keeps-connection-ready
  (let* ((error-payload
           (join-octets
            (octets (char-code #\S)) (cstring "ERROR")
            (octets (char-code #\C)) (cstring "57014")
            (octets (char-code #\M)) (cstring "COPY cancelled")
            (octets 0)))
         (input
           (join-octets
            (make-frame #\G (copy-response-payload))
            (make-frame #\E error-payload)
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (let ((operation (copy-in connection "copy target from stdin")))
           (is (copy-in-abort operation "stop now"))
           (is (connection-open connection))
           (is (eq (connection-state connection) :ready))
           (is (equalp
                (memory-transport-output (connection-transport connection))
                (join-octets
                 (encode-query-message "copy target from stdin")
                 (encode-copy-fail-message "stop now"))))
           (assert-signals 'copy-error
                          (lambda () (copy-in-finish operation))))
      (disconnect connection))))

(deftest copy-both-round-trip
  (let* ((copy-both-input
           (join-octets
            (make-frame #\W (copy-response-payload))
            (make-frame #\d (octets 115 101 114 118 101 114 45 114 111 119))
            (make-frame #\c #())
            (make-frame #\C (cstring "COPY 1"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input copy-both-input)))
    (unwind-protect
         (let ((operation (copy-both-start connection "copy both target")))
           (is (copy-both-write operation "client-row"))
           (is (equalp (copy-both-read operation)
                       (octets 115 101 114 118 101 114 45 114 111 119)))
           (is (null (copy-both-read operation)))
           (is (string= (copy-both-finish operation) "COPY 1"))
           (is (equalp
                (memory-transport-output (connection-transport connection))
                (join-octets
                 (encode-query-message "copy both target")
                 (encode-copy-data-message "client-row")
                 (encode-copy-done-message)))))
      (disconnect connection))))

(deftest copy-codec-session-helpers
  (let* ((input
           (join-octets
            (make-frame #\G (copy-response-payload))
            (make-frame #\C (cstring "COPY 1"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (let ((operation (copy-in-start connection "copy target from stdin")))
           (is (copy-in-write-row operation '(7 "hello") '(23 25)))
           (is (copy-in-write-binary-stream operation '((8) (9)) '(23)))
           (is (string= (copy-in-finish operation) "COPY 1"))
           (is (equalp
                (memory-transport-output (connection-transport connection))
                (join-octets
                 (encode-query-message "copy target from stdin")
                 (encode-copy-data-message
                  (encode-copy-row '(7 "hello") '(23 25)))
                 (encode-copy-data-message
                  (encode-copy-binary-stream '((8) (9)) '(23)))
                 (encode-copy-done-message)))))
      (disconnect connection)))
  (let* ((stream (encode-copy-binary-stream '((1) (2)) '(23)))
         (split (floor (length stream) 2))
         (input
           (join-octets
            (make-frame #\H (copy-response-payload))
            (make-frame #\d (subseq stream 0 split))
            (make-frame #\d (subseq stream split))
            (make-frame #\c #())
            (make-frame #\C (cstring "COPY 2"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (let ((operation (copy-out-start connection "copy source to stdout")))
           (multiple-value-bind (rows flags extension)
               (decode-copy-binary-stream
                (copy-out-read-all operation) '(23))
             (is (equal rows '((1) (2))))
             (is (= flags 0))
             (is (zerop (length extension)))))
      (disconnect connection)))
  (let* ((stream (encode-copy-binary-stream '((11) (12)) '(23)))
         (input
           (join-octets
            (make-frame #\W (copy-response-payload))
            (make-frame #\d stream)
            (make-frame #\c #())
            (make-frame #\C (cstring "COPY 2"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (let ((operation (copy-both-start connection "copy both target")))
           (is (copy-both-write-row operation '(10) '(23)))
           (is (equalp (copy-both-read-all operation) stream))
           (is (string= (copy-both-finish operation) "COPY 2"))
           (is (equalp
                (memory-transport-output (connection-transport connection))
                (join-octets
                 (encode-query-message "copy both target")
                 (encode-copy-data-message
                  (encode-copy-row '(10) '(23)))
                 (encode-copy-done-message)))))
      (disconnect connection))))

(deftest copy-text-stream-codec-session-helpers
  (let* ((rows
           (list (list 7 (format nil "hello~Cworld" #\Newline))
                 (list 8 (format nil "back~Cslash~Cvalue" #\\ #\Tab))))
         (stream (encode-copy-text-stream rows '(23 25))))
    (is (equal rows (decode-copy-text-stream stream '(23 25))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (decode-copy-text-stream
                       (subseq stream 0 (1- (length stream)))
                       '(23 25)))))
  (let* ((rows
           (list (list 7 (format nil "hello~Cworld" #\Newline))
                 (list 8 (format nil "back~Cslash~Cvalue" #\\ #\Tab))))
         (stream (encode-copy-text-stream rows '(23 25)))
         (input
           (join-octets
            (make-frame #\G (copy-response-payload))
            (make-frame #\C (cstring "COPY 2"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (let ((operation (copy-in-start connection "copy target from stdin")))
           (is (copy-in-write-text-stream operation rows '(23 25)))
           (is (string= (copy-in-finish operation) "COPY 2"))
           (is (equalp
                (memory-transport-output (connection-transport connection))
                (join-octets
                 (encode-query-message "copy target from stdin")
                 (encode-copy-data-message stream)
                 (encode-copy-done-message)))))
      (disconnect connection)))
  (let* ((rows
           (list (list 1 (format nil "one~Cline" #\Newline))
                 (list 2 (format nil "two~Cslash" #\\))))
         (stream (encode-copy-text-stream rows '(23 25)))
         (split (floor (length stream) 2))
         (input
           (join-octets
            (make-frame #\H (copy-response-payload))
            (make-frame #\d (subseq stream 0 split))
            (make-frame #\d (subseq stream split))
            (make-frame #\c #())
            (make-frame #\C (cstring "COPY 2"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (let ((operation (copy-out-start connection "copy source to stdout")))
           (is (equal rows (copy-out-read-rows operation '(23 25)))))
      (disconnect connection)))
  (let* ((server-rows
           (list (list 11 (format nil "server~Cline" #\Newline))
                 (list 12 (format nil "server~Cslash" #\\))))
         (client-rows
           (list (list 10 (format nil "client~Cline" #\Newline))))
         (server-stream (encode-copy-text-stream server-rows '(23 25)))
         (client-stream (encode-copy-text-stream client-rows '(23 25)))
         (input
           (join-octets
            (make-frame #\W (copy-response-payload))
            (make-frame #\d server-stream)
            (make-frame #\c #())
            (make-frame #\C (cstring "COPY 2"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (let ((operation (copy-both-start connection "copy both target")))
           (is (copy-both-write-text-stream operation client-rows '(23 25)))
           (is (equal server-rows
                      (copy-both-read-rows operation '(23 25))))
           (is (string= (copy-both-finish operation) "COPY 2"))
           (is (equalp
                (memory-transport-output (connection-transport connection))
                (join-octets
                 (encode-query-message "copy both target")
                 (encode-copy-data-message client-stream)
                 (encode-copy-done-message)))))
      (disconnect connection))))

(deftest replication-message-codecs
  (let ((xlog-payload
          (let ((builder (make-octet-builder)))
            (append-u8 builder (char-code #\w))
            (append-u64 builder #x0000000100000002)
            (append-u64 builder #x0000000300000004)
            (append-i64 builder 123456789)
            (append-octets builder (octets 1 2 3))
            (builder-octets builder)))
        (keepalive-payload
          (let ((builder (make-octet-builder)))
            (append-u8 builder (char-code #\k))
            (append-u64 builder 5)
            (append-i64 builder 6)
            (append-u8 builder 1)
            (builder-octets builder)))
        (status-payload
          (encode-standby-status-update 100 90 80
                                         :client-time 123
                                         :reply-requested t))
        (feedback-payload
          (encode-hot-standby-feedback 1 2 3 4 :client-time 321)))
    (let ((message (parse-replication-message xlog-payload)))
      (is (eq :xlog-data (replication-message-kind message)))
      (is (= #x0000000100000002 (replication-message-wal-start message)))
      (is (= #x0000000300000004 (replication-message-wal-end message)))
      (is (= 123456789 (replication-message-send-time message)))
      (is (equalp (octets 1 2 3) (replication-message-data message))))
    (let ((message (parse-replication-message keepalive-payload)))
      (is (eq :primary-keepalive (replication-message-kind message)))
      (is (= 5 (replication-message-wal-end message)))
      (is (= 6 (replication-message-send-time message)))
      (is (replication-message-reply-requested message)))
    (let ((message (parse-replication-message status-payload)))
      (is (eq :standby-status-update (replication-message-kind message)))
      (is (= 100 (replication-message-write-lsn message)))
      (is (= 90 (replication-message-flush-lsn message)))
      (is (= 80 (replication-message-apply-lsn message)))
      (is (= 123 (replication-message-client-time message)))
      (is (replication-message-reply-requested message)))
    (let ((message (parse-replication-message feedback-payload)))
      (is (eq :hot-standby-feedback (replication-message-kind message)))
      (is (= 321 (replication-message-client-time message)))
      (is (= 1 (replication-message-xmin message)))
      (is (= 2 (replication-message-xmin-epoch message)))
      (is (= 3 (replication-message-catalog-xmin message)))
      (is (= 4 (replication-message-catalog-xmin-epoch message))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (parse-replication-message
                       (octets (char-code #\k)))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (parse-replication-message
                       (join-octets keepalive-payload (octets 0)))))))

(deftest logical-replication-message-codecs
  (labels ((payload (marker writer)
             (let ((builder (make-octet-builder)))
               (append-u8 builder (char-code marker))
               (funcall writer builder)
               (builder-octets builder)))
           (column (flags name type-oid type-modifier)
             (let ((builder (make-octet-builder)))
               (append-u8 builder flags)
               (append-octets builder (cstring name))
               (append-u32 builder type-oid)
               (append-i32 builder type-modifier)
               (builder-octets builder)))
           (tuple (fields)
             (let ((builder (make-octet-builder)))
               (append-i16 builder (length fields))
               (dolist (field fields)
                 (cond
                   ((eq field :null)
                    (append-u8 builder (char-code #\n)))
                   ((eq field :unchanged-toast)
                    (append-u8 builder (char-code #\u)))
                   ((and (consp field) (eq (first field) :text))
                    (append-u8 builder (char-code #\t))
                    (append-i32 builder (length (second field)))
                    (append-octets builder (second field)))
                   ((and (consp field) (eq (first field) :binary))
                    (append-u8 builder (char-code #\b))
                    (append-i32 builder (length (second field)))
                    (append-octets builder (second field)))
                   (t
                    (error "Unknown test TupleData field: ~S" field))))
               (builder-octets builder))))
    (let ((message
           (parse-logical-replication-message
            (payload #\B
                      (lambda (builder)
                        (append-u64 builder 10)
                        (append-i64 builder -11)
                        (append-u32 builder 12))))))
      (is (eq :begin (logical-replication-message-kind message)))
      (is (= 1 (logical-replication-message-protocol-version message)))
      (is (= 10 (logical-replication-message-final-lsn message)))
      (is (= -11 (logical-replication-message-commit-time message)))
      (is (= 12 (logical-replication-message-xid message))))
    (let ((message
            (parse-logical-replication-message
             (payload #\M
                      (lambda (builder)
                        (append-u8 builder 1)
                        (append-u64 builder 20)
                        (append-octets builder (cstring "prefix"))
                        (append-i32 builder 3)
                        (append-octets builder (octets 1 2 3)))))))
      (is (eq :message (logical-replication-message-kind message)))
      (is (= 1 (logical-replication-message-flags message)))
      (is (= 20 (logical-replication-message-message-lsn message)))
      (is (string= "prefix" (logical-replication-message-prefix message)))
      (is (equalp (octets 1 2 3)
                  (logical-replication-message-content message))))
    (let ((message
            (parse-logical-replication-message
             (payload #\C
                      (lambda (builder)
                        (append-u8 builder 2)
                        (append-u64 builder 30)
                        (append-u64 builder 31)
                        (append-i64 builder -32))))))
      (is (eq :commit (logical-replication-message-kind message)))
      (is (= 2 (logical-replication-message-flags message)))
      (is (= 30 (logical-replication-message-commit-lsn message)))
      (is (= 31 (logical-replication-message-end-lsn message)))
      (is (= -32 (logical-replication-message-commit-time message))))
    (let ((message
            (parse-logical-replication-message
             (payload #\O
                      (lambda (builder)
                        (append-u64 builder 40)
                        (append-octets builder (cstring "origin")))))))
      (is (eq :origin (logical-replication-message-kind message)))
      (is (= 40 (logical-replication-message-origin-lsn message)))
      (is (string= "origin" (logical-replication-message-origin-name message))))
    (let* ((message
            (parse-logical-replication-message
             (payload #\R
                      (lambda (builder)
                        (append-u32 builder 50)
                        (append-octets builder (cstring "public"))
                        (append-octets builder (cstring "items"))
                        (append-u8 builder 2)
                        (append-i16 builder 2)
                        (append-octets builder (column 1 "id" 23 -1))
                        (append-octets builder (column 0 "body" 25 -1))))))
          (columns (logical-replication-message-columns message)))
      (is (eq :relation (logical-replication-message-kind message)))
      (is (= 50 (logical-replication-message-relation-id message)))
      (is (string= "public" (logical-replication-message-namespace message)))
      (is (string= "items" (logical-replication-message-relation-name message)))
      (is (= 2 (logical-replication-message-replica-identity message)))
      (is (= 2 (length columns)))
      (is (= 1 (logical-replication-column-flags (aref columns 0))))
      (is (string= "id" (logical-replication-column-name (aref columns 0))))
      (is (= 23 (logical-replication-column-type-oid (aref columns 0))))
      (is (= -1 (logical-replication-column-type-modifier (aref columns 0))))
      (is (string= "body" (logical-replication-column-name (aref columns 1)))))
    (let ((message
            (parse-logical-replication-message
             (payload #\Y
                      (lambda (builder)
                        (append-u32 builder 60)
                        (append-octets builder (cstring "pg_catalog"))
                        (append-octets builder (cstring "my_type")))))))
      (is (eq :type (logical-replication-message-kind message)))
      (is (= 60 (logical-replication-message-type-oid message)))
      (is (string= "pg_catalog"
                   (logical-replication-message-type-namespace message)))
      (is (string= "my_type"
                   (logical-replication-message-type-name message))))
    (let ((message
            (parse-logical-replication-message
             (payload #\I
                      (lambda (builder)
                        (append-u32 builder 70)
                        (append-u8 builder (char-code #\N))
                        (append-octets
                         builder
                         (tuple (list :null
                                      :unchanged-toast
                                      (list :text (octets 1 2))
                                      (list :binary (octets 3 4 5))))))))))
      (let ((tuple (logical-replication-message-new-tuple message)))
        (is (eq :insert (logical-replication-message-kind message)))
        (is (= 70 (logical-replication-message-relation-id message)))
        (is (= 4 (length (logical-replication-tuple-fields tuple))))
        (is (eq :null
                (logical-replication-field-kind
                 (aref (logical-replication-tuple-fields tuple) 0))))
        (is (eq +sql-null+
                (logical-replication-field-data
                 (aref (logical-replication-tuple-fields tuple) 0))))
        (is (eq :unchanged-toast
                (logical-replication-field-kind
                 (aref (logical-replication-tuple-fields tuple) 1))))
        (is (null
             (logical-replication-field-data
              (aref (logical-replication-tuple-fields tuple) 1))))
        (is (equalp (octets 1 2)
                    (logical-replication-field-data
                     (aref (logical-replication-tuple-fields tuple) 2))))
        (is (eq :binary
                (logical-replication-field-kind
                 (aref (logical-replication-tuple-fields tuple) 3))))))
    (let ((message
            (parse-logical-replication-message
             (payload #\U
                      (lambda (builder)
                        (append-u32 builder 71)
                        (append-u8 builder (char-code #\K))
                        (append-octets builder (tuple (list (list :text (octets 9)))))
                        (append-u8 builder (char-code #\N))
                        (append-octets builder (tuple (list (list :text (octets 8))))))))))
      (is (eq :update (logical-replication-message-kind message)))
      (is (= 71 (logical-replication-message-relation-id message)))
      (is (eq :key (logical-replication-message-old-tuple-kind message)))
      (is (equalp (octets 9)
                  (logical-replication-field-data
                   (aref (logical-replication-tuple-fields
                          (logical-replication-message-old-tuple message))
                         0))))
      (is (equalp (octets 8)
                  (logical-replication-field-data
                   (aref (logical-replication-tuple-fields
                          (logical-replication-message-new-tuple message))
                         0)))))
    (let ((message
            (parse-logical-replication-message
             (payload #\U
                      (lambda (builder)
                        (append-u32 builder 72)
                        (append-u8 builder (char-code #\N))
                        (append-octets builder (tuple (list (list :text (octets 7))))))))))
      (is (eq :update (logical-replication-message-kind message)))
      (is (null (logical-replication-message-old-tuple message)))
      (is (null (logical-replication-message-old-tuple-kind message))))
    (let ((message
            (parse-logical-replication-message
             (payload #\D
                      (lambda (builder)
                        (append-u32 builder 73)
                        (append-u8 builder (char-code #\O))
                        (append-octets builder (tuple (list (list :binary (octets 6))))))))))
      (is (eq :delete (logical-replication-message-kind message)))
      (is (eq :old (logical-replication-message-old-tuple-kind message)))
      (is (= 73 (logical-replication-message-relation-id message))))
    (let ((message
            (parse-logical-replication-message
             (payload #\T
                      (lambda (builder)
                        (append-i32 builder 2)
                        (append-u8 builder 3)
                        (append-u32 builder 80)
                        (append-u32 builder 81))))))
      (is (eq :truncate (logical-replication-message-kind message)))
      (is (= 3 (logical-replication-message-options message)))
      (is (equalp #(80 81)
                  (logical-replication-message-relation-ids message))))))

(deftest logical-replication-streaming-and-two-phase-codecs
  (labels ((payload (marker writer)
             (let ((builder (make-octet-builder)))
               (append-u8 builder (char-code marker))
               (funcall writer builder)
               (builder-octets builder)))
           (tuple (value)
             (let ((builder (make-octet-builder)))
               (append-i16 builder 1)
               (append-u8 builder (char-code #\t))
               (append-i32 builder 1)
               (append-u8 builder value)
               (builder-octets builder))))
    (let ((message
            (parse-logical-replication-message
             (payload #\S
                      (lambda (builder)
                        (append-u32 builder 100)
                        (append-u8 builder 1)))
             :protocol-version 2
             :streamed-p t)))
      (is (eq :stream-start (logical-replication-message-kind message)))
      (is (= 100 (logical-replication-message-xid message)))
      (is (logical-replication-message-first-segment message)))
    (is (eq :stream-stop
            (logical-replication-message-kind
             (parse-logical-replication-message
              (octets (char-code #\E))
              :protocol-version 2
              :streamed-p t))))
    (let ((message
            (parse-logical-replication-message
             (payload #\c
                      (lambda (builder)
                        (append-u32 builder 101)
                        (append-u8 builder 2)
                        (append-u64 builder 110)
                        (append-u64 builder 111)
                        (append-i64 builder -112)))
             :protocol-version 2
             :streamed-p t)))
      (is (eq :stream-commit (logical-replication-message-kind message)))
      (is (= 101 (logical-replication-message-xid message)))
      (is (= 2 (logical-replication-message-flags message)))
      (is (= 110 (logical-replication-message-commit-lsn message)))
      (is (= 111 (logical-replication-message-end-lsn message)))
      (is (= -112 (logical-replication-message-commit-time message))))
    (let ((message
            (parse-logical-replication-message
             (payload #\A
                      (lambda (builder)
                        (append-u32 builder 102)
                        (append-u32 builder 103)))
             :protocol-version 2
             :streamed-p t)))
      (is (eq :stream-abort (logical-replication-message-kind message)))
      (is (= 102 (logical-replication-message-xid message)))
      (is (= 103 (logical-replication-message-subtransaction-id message)))
      (is (null (logical-replication-message-abort-lsn message))))
    (let ((message
            (parse-logical-replication-message
             (payload #\M
                      (lambda (builder)
                        (append-u32 builder 104)
                        (append-u8 builder 0)
                        (append-u64 builder 120)
                        (append-octets builder (cstring "stream"))
                        (append-i32 builder 1)
                        (append-u8 builder 9)))
             :protocol-version 2
             :streamed-p t)))
      (is (= 104 (logical-replication-message-xid message)))
      (is (equalp (octets 9) (logical-replication-message-content message))))
    (let ((message
            (parse-logical-replication-message
             (payload #\I
                      (lambda (builder)
                        (append-u32 builder 105)
                        (append-u32 builder 106)
                        (append-u8 builder (char-code #\N))
                        (append-octets builder (tuple 4))))
             :protocol-version 2
             :streamed-p t)))
      (is (= 105 (logical-replication-message-xid message)))
      (is (= 106 (logical-replication-message-relation-id message))))
    (let ((message
            (parse-logical-replication-message
             (payload #\A
                      (lambda (builder)
                        (append-u32 builder 107)
                        (append-u32 builder 108)
                        (append-u64 builder 130)
                        (append-i64 builder -131)))
             :protocol-version 4
             :streamed-p t
             :parallel-streaming-p t)))
      (is (= 130 (logical-replication-message-abort-lsn message)))
      (is (= -131 (logical-replication-message-abort-time message))))
    (let ((message
            (parse-logical-replication-message
             (payload #\b
                      (lambda (builder)
                        (append-u64 builder 140)
                        (append-u64 builder 141)
                        (append-i64 builder -142)
                        (append-u32 builder 109)
                        (append-octets builder (cstring "gid"))))
             :protocol-version 3)))
      (is (eq :begin-prepare (logical-replication-message-kind message)))
      (is (= 109 (logical-replication-message-xid message)))
      (is (= 140 (logical-replication-message-prepare-lsn message)))
      (is (= 141 (logical-replication-message-prepare-end-lsn message)))
      (is (= -142 (logical-replication-message-prepare-time message)))
      (is (string= "gid" (logical-replication-message-gid message))))
    (dolist (spec
             '((#\P :prepare)
               (#\K :commit-prepared)
               (#\r :rollback-prepared)
               (#\p :stream-prepare)))
      (destructuring-bind (marker expected-kind) spec
        (let ((message
                (parse-logical-replication-message
                 (payload marker
                          (lambda (builder)
                            (append-u8 builder 4)
                            (append-u64 builder 150)
                            (append-u64 builder 151)
                            (append-i64 builder -152)
                            (when (member marker '(#\r))
                              (append-i64 builder -153))
                            (append-u32 builder 110)
                            (append-octets builder (cstring "gid"))))
                 :protocol-version 3)))
          (is (eq expected-kind
                  (logical-replication-message-kind message)))
          (is (= 110 (logical-replication-message-xid message)))
          (is (= 4 (logical-replication-message-flags message)))
          (is (string= "gid" (logical-replication-message-gid message))))))))

(deftest logical-replication-message-validation
  (labels ((payload (marker writer)
             (let ((builder (make-octet-builder)))
               (append-u8 builder (char-code marker))
               (funcall writer builder)
               (builder-octets builder))))
    (assert-signals 'parameter-error
                    (lambda ()
                      (parse-logical-replication-message #() :protocol-version 0)))
    (assert-signals 'parameter-error
                    (lambda ()
                      (parse-logical-replication-message #() :protocol-version 5)))
    (assert-signals 'parameter-error
                    (lambda ()
                      (parse-logical-replication-message
                       #() :streamed-p :yes)))
    (assert-signals 'parameter-error
                    (lambda ()
                      (parse-logical-replication-message
                       #() :parallel-streaming-p t)))
    (assert-signals 'unsupported-feature
                    (lambda ()
                      (parse-logical-replication-message
                       (octets (char-code #\S) 0 0 0 1 0)
                       :protocol-version 1)))
    (assert-signals 'unsupported-feature
                    (lambda ()
                      (parse-logical-replication-message
                       (octets (char-code #\E))
                       :protocol-version 1)))
    (assert-signals 'unsupported-feature
                    (lambda ()
                      (parse-logical-replication-message
                       (octets (char-code #\b) 0)
                       :protocol-version 2)))
    (assert-signals 'unsupported-feature
                    (lambda ()
                      (parse-logical-replication-message
                       (octets (char-code #\A) 0 0 0 1 0 0 0 2 0)
                       :protocol-version 3
                       :streamed-p t
                       :parallel-streaming-p t)))
    (assert-signals 'protocol-error
                    (lambda ()
                      (parse-logical-replication-message
                       (octets (char-code #\x)))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (parse-logical-replication-message
                       (octets (char-code #\E) 0)
                       :protocol-version 2)))
    (assert-signals 'protocol-error
                    (lambda ()
                      (parse-logical-replication-message
                       (payload #\M
                                (lambda (builder)
                                  (append-u8 builder 0)
                                  (append-u64 builder 1)
                                  (append-octets builder (cstring "x"))
                                  (append-i32 builder -1))))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (parse-logical-replication-message
                       (payload #\I
                                (lambda (builder)
                                  (append-u32 builder 1)
                                  (append-u8 builder (char-code #\X)))))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (parse-logical-replication-message
                       (payload #\U
                                (lambda (builder)
                                  (append-u32 builder 1)
                                  (append-u8 builder (char-code #\X)))))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (parse-logical-replication-message
                       (payload #\D
                                (lambda (builder)
                                  (append-u32 builder 1)
                                  (append-u8 builder (char-code #\N)))))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (parse-logical-replication-message
                       (payload #\T
                                (lambda (builder)
                                  (append-i32 builder -1)
                                  (append-u8 builder 0))))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (parse-logical-replication-message
                       (payload #\R
                                (lambda (builder)
                                  (append-u32 builder 1)
                                  (append-octets builder (cstring "public"))
                                  (append-octets builder (cstring "items"))
                                  (append-u8 builder 0)
                                  (append-i16 builder -1))))))))

(deftest logical-replication-value-decoder
  (labels ((payload (marker writer)
             (let ((builder (make-octet-builder)))
               (append-u8 builder (char-code marker))
               (funcall writer builder)
               (builder-octets builder)))
           (column (flags name type-oid type-modifier)
             (let ((builder (make-octet-builder)))
               (append-u8 builder flags)
               (append-octets builder (cstring name))
               (append-u32 builder type-oid)
               (append-i32 builder type-modifier)
               (builder-octets builder)))
           (tuple (fields)
             (let ((builder (make-octet-builder)))
               (append-i16 builder (length fields))
               (dolist (field fields)
                 (cond
                   ((eq field :null)
                    (append-u8 builder (char-code #\n)))
                   ((eq field :unchanged-toast)
                    (append-u8 builder (char-code #\u)))
                   ((and (consp field) (eq (first field) :text))
                    (append-u8 builder (char-code #\t))
                    (append-i32 builder (length (second field)))
                    (append-octets builder (second field)))
                   ((and (consp field) (eq (first field) :binary))
                    (append-u8 builder (char-code #\b))
                    (append-i32 builder (length (second field)))
                    (append-octets builder (second field)))
                   (t
                    (error "Unknown test TupleData field: ~S" field))))
               (builder-octets builder))))
    (let* ((relation-message
             (parse-logical-replication-message
              (payload #\R
                       (lambda (builder)
                         (append-u32 builder 200)
                         (append-octets builder (cstring "public"))
                         (append-octets builder (cstring "items"))
                         (append-u8 builder 3)
                         (append-i16 builder 3)
                         (append-octets builder (column #x03 "id" 23 -1))
                         (append-octets builder (column #x02 "body" 25 -1))
                         (append-octets builder (column #x00 "note" 25 -1))))))
           (decoder (make-logical-replication-decoder))
           (relation-event
             (decode-logical-replication-message decoder relation-message)))
      (is (eq :relation (logical-replication-event-kind relation-event)))
      (is (eq relation-message
              (logical-replication-event-relation relation-event)))
      (is (eq relation-message
              (find-logical-replication-relation decoder 200)))
      (let* ((message
               (parse-logical-replication-message
                (payload #\I
                         (lambda (builder)
                           (append-u32 builder 200)
                           (append-u8 builder (char-code #\N))
                           (append-octets
                            builder
                            (tuple (list (list :text (octets 52 50))
                                         (list :text
                                               (octets 104 101 108 108 111))
                                         :null)))))))
             (event (decode-logical-replication-message decoder message))
             (values (logical-replication-event-new-values event)))
        (is (eq :insert (logical-replication-event-kind event)))
        (is (= 42 (aref values 0)))
        (is (string= "hello" (aref values 1)))
        (is (eq +sql-null+ (aref values 2))))
      (let* ((message
               (parse-logical-replication-message
                (payload #\I
                         (lambda (builder)
                           (append-u32 builder 200)
                           (append-u8 builder (char-code #\N))
                           (append-octets
                            builder
                            (tuple (list (list :binary (octets 0 0 0 42))
                                         (list :binary
                                               (octets 119 111 114 108 100))
                                         :null)))))))
             (event (decode-logical-replication-message decoder message)))
        (is (= 42
               (aref (logical-replication-event-new-values event) 0)))
        (is (string= "world"
                     (aref (logical-replication-event-new-values event) 1))))
      (let* ((message
               (parse-logical-replication-message
                (payload #\U
                         (lambda (builder)
                           (append-u32 builder 200)
                           (append-u8 builder (char-code #\K))
                           (append-octets
                            builder
                            (tuple (list (list :text (octets 52 50)))))
                           (append-u8 builder (char-code #\N))
                           (append-octets
                            builder
                            (tuple (list (list :text (octets 52 51))
                                         :unchanged-toast
                                         (list :text
                                               (octets 119 111 114 108 100)))))))))
             (event (decode-logical-replication-message decoder message))
             (new-values (logical-replication-event-new-values event))
             (old-values (logical-replication-event-old-values event)))
        (is (eq :update (logical-replication-event-kind event)))
        (is (= 43 (aref new-values 0)))
        (is (eq +logical-replication-unchanged-toast+
                (aref new-values 1)))
        (is (string= "world" (aref new-values 2)))
        (is (= 1 (length old-values)))
        (is (= 42 (aref old-values 0)))
        (is (eq :key
                (logical-replication-event-old-values-kind event))))
      (let* ((message
               (parse-logical-replication-message
                (payload #\D
                         (lambda (builder)
                           (append-u32 builder 200)
                           (append-u8 builder (char-code #\O))
                           (append-octets
                            builder
                            (tuple (list (list :text (octets 52 51))
                                         (list :text
                                               (octets 119 111 114 108 100)))))))))
             (event (decode-logical-replication-message decoder message))
             (old-values (logical-replication-event-old-values event)))
        (is (eq :delete (logical-replication-event-kind event)))
        (is (= 2 (length old-values)))
        (is (= 43 (aref old-values 0)))
        (is (string= "world" (aref old-values 1)))
        (is (eq :old
                (logical-replication-event-old-values-kind event))))
      (let ((unknown-message
              (parse-logical-replication-message
               (payload #\I
                        (lambda (builder)
                          (append-u32 builder 999)
                          (append-u8 builder (char-code #\N))
                          (append-octets
                           builder
                           (tuple (list (list :text (octets 52 50))))))))))
        (assert-signals
         'protocol-error
         (lambda ()
           (decode-logical-replication-message decoder unknown-message))))
      (let ((mismatch-message
              (parse-logical-replication-message
               (payload #\I
                        (lambda (builder)
                          (append-u32 builder 200)
                          (append-u8 builder (char-code #\N))
                          (append-octets
                           builder
                           (tuple (list (list :text (octets 52 50))
                                        (list :text (octets 120))))))))))
        (assert-signals
         'protocol-error
         (lambda ()
           (decode-logical-replication-message decoder mismatch-message))))
      (is (forget-logical-replication-relation decoder 200))
      (is (null (find-logical-replication-relation decoder 200)))
      (is (eq decoder (clear-logical-replication-relations decoder))))))

(deftest logical-replication-stream-decoder
  (labels ((payload (marker writer)
             (let ((builder (make-octet-builder)))
               (append-u8 builder (char-code marker))
               (funcall writer builder)
               (builder-octets builder)))
           (tuple (fields)
             (let ((builder (make-octet-builder)))
               (append-i16 builder (length fields))
               (dolist (field fields)
                 (append-u8 builder (char-code #\t))
                 (append-i32 builder (length field))
                 (append-octets builder field))
               (builder-octets builder)))
           (relation-payload ()
             (payload
              #\R
              (lambda (builder)
                (append-u32 builder 200)
                (append-octets builder (cstring "public"))
                (append-octets builder (cstring "items"))
                (append-u8 builder 3)
                (append-i16 builder 3)
                (dolist (column '((#x03 "id" 23 -1)
                                  (#x02 "body" 25 -1)
                                  (#x00 "note" 25 -1)))
                  (append-u8 builder (first column))
                  (append-octets builder (cstring (second column)))
                  (append-u32 builder (third column))
                  (append-i32 builder (fourth column))))))
           (insert-payload ()
             (payload
              #\I
              (lambda (builder)
                (append-u32 builder 200)
                (append-u8 builder (char-code #\N))
                (append-octets
                 builder
                 (tuple (list (octets 52 50)
                              (octets 104 101 108 108 111)
                              (octets 110 111 116 101)))))))
           (xlog-payload (logical-payload)
             (let ((builder (make-octet-builder)))
               (append-u8 builder (char-code #\w))
               (append-u64 builder 10)
               (append-u64 builder 12)
               (append-i64 builder 123)
               (append-octets builder logical-payload)
               (builder-octets builder))))
    (let* ((replication-input
             (join-octets
              (make-frame #\W (copy-response-payload))
              (make-frame #\d (xlog-payload (relation-payload)))
              (make-frame #\d (xlog-payload (insert-payload)))
              (make-frame #\d
                          (let ((builder (make-octet-builder)))
                            (append-u8 builder (char-code #\k))
                            (append-u64 builder 12)
                            (append-i64 builder 124)
                            (append-u8 builder 1)
                            (builder-octets builder)))
              (make-frame #\c #())
              (make-frame #\C (cstring "START_REPLICATION"))
              (make-frame #\Z (octets (char-code #\I)))))
           (connection (ready-memory-connection :input replication-input))
           (decoder (make-logical-replication-decoder)))
      (unwind-protect
           (let ((operation
                   (replication-start
                    connection
                    "START_REPLICATION SLOT slot LOGICAL 0/0")))
             (let* ((relation-stream
                      (logical-replication-read operation decoder))
                    (insert-stream
                      (logical-replication-read operation decoder))
                    (keepalive-stream
                      (logical-replication-read operation decoder)))
               (is (eq :xlog-data
                       (logical-replication-stream-event-kind relation-stream)))
               (is (= 10
                      (replication-message-wal-start
                       (logical-replication-stream-event-replication-message
                        relation-stream))))
               (is (eq :relation
                       (logical-replication-message-kind
                        (logical-replication-stream-event-logical-message
                         relation-stream))))
               (is (eq :relation
                       (logical-replication-event-kind
                        (logical-replication-stream-event-logical-event
                         relation-stream))))
               (is (eq :insert
                       (logical-replication-message-kind
                        (logical-replication-stream-event-logical-message
                         insert-stream))))
               (is (= 42
                      (aref
                       (logical-replication-event-new-values
                        (logical-replication-stream-event-logical-event
                         insert-stream))
                       0)))
               (is (string= "hello"
                            (aref
                             (logical-replication-event-new-values
                              (logical-replication-stream-event-logical-event
                               insert-stream))
                             1)))
               (is (eq :primary-keepalive
                       (logical-replication-stream-event-kind keepalive-stream)))
               (is (null
                    (logical-replication-stream-event-logical-message
                     keepalive-stream)))
               (is (null
                    (logical-replication-stream-event-logical-event
                     keepalive-stream))))
             (is (null (logical-replication-read operation decoder)))
             (is (string= "START_REPLICATION"
                          (replication-finish operation))))
        (disconnect connection)))))

(deftest base-backup-message-codecs
  (let* ((new-archive
           (parse-base-backup-message
            (join-octets (octets (char-code #\n))
                         (cstring "base.tar")
                         (cstring "/var/lib/postgresql"))))
         (manifest
           (parse-base-backup-message
            (octets (char-code #\m))))
         (data
           (parse-base-backup-message
            (join-octets (octets (char-code #\d))
                         (octets 1 2 3))))
         (progress-builder (make-octet-builder)))
    (append-u8 progress-builder (char-code #\p))
    (append-u64 progress-builder 42)
    (let ((progress (parse-base-backup-message
                     (builder-octets progress-builder))))
      (is (eq :new-archive (base-backup-message-kind new-archive)))
      (is (string= "base.tar"
                   (base-backup-message-archive-name new-archive)))
      (is (string= "/var/lib/postgresql"
                   (base-backup-message-tablespace-path new-archive)))
      (is (eq :manifest (base-backup-message-kind manifest)))
      (is (eq :data (base-backup-message-kind data)))
      (is (equalp #(1 2 3) (base-backup-message-data data)))
      (is (eq :progress (base-backup-message-kind progress)))
      (is (= 42 (base-backup-message-bytes-completed progress))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (parse-base-backup-message
                       (octets (char-code #\x)))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (parse-base-backup-message
                       (join-octets (octets (char-code #\m))
                                    (octets 0)))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (parse-base-backup-message
                       (octets (char-code #\n) 1))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (parse-base-backup-message
                       (octets (char-code #\p) 0 0 0 0 0 0 0 0 1 0))))))

(deftest replication-copy-both-round-trip
  (let* ((xlog-payload
           (let ((builder (make-octet-builder)))
             (append-u8 builder (char-code #\w))
             (append-u64 builder 10)
             (append-u64 builder 12)
             (append-i64 builder 123)
             (append-octets builder (octets 9 8 7))
             (builder-octets builder)))
         (keepalive-payload
           (let ((builder (make-octet-builder)))
             (append-u8 builder (char-code #\k))
             (append-u64 builder 12)
             (append-i64 builder 124)
             (append-u8 builder 1)
             (builder-octets builder)))
         (replication-input
           (join-octets
            (make-frame #\W (copy-response-payload))
            (make-frame #\d xlog-payload)
            (make-frame #\d keepalive-payload)
            (make-frame #\c #())
            (make-frame #\C (cstring "START_REPLICATION"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input replication-input)))
    (unwind-protect
         (let ((operation
                 (replication-start
                  connection
                  "START_REPLICATION SLOT slot LOGICAL 0/0")))
           (is (replication-send-status operation 100 90 80
                                         :client-time 123
                                         :reply-requested t))
           (is (replication-send-hot-standby-feedback
                operation 1 2 3 4 :client-time 321))
           (let ((xlog (replication-read operation))
                 (keepalive (replication-read operation)))
             (is (eq :xlog-data (replication-message-kind xlog)))
             (is (= 10 (replication-message-wal-start xlog)))
             (is (= 12 (replication-message-wal-end xlog)))
             (is (equalp (octets 9 8 7) (replication-message-data xlog)))
             (is (eq :primary-keepalive
                     (replication-message-kind keepalive)))
             (is (replication-message-reply-requested keepalive)))
           (is (null (replication-read operation)))
           (is (string= "START_REPLICATION"
                        (replication-finish operation)))
           (is (equalp
                (memory-transport-output (connection-transport connection))
                (join-octets
                 (encode-query-message
                  "START_REPLICATION SLOT slot LOGICAL 0/0")
                 (encode-copy-data-message
                  (encode-standby-status-update
                   100 90 80 :client-time 123 :reply-requested t))
                 (encode-copy-data-message
                  (encode-hot-standby-feedback
                   1 2 3 4 :client-time 321))
                 (encode-copy-done-message)))))
      (disconnect connection))))

(deftest replication-malformed-payload-retires-connection
  (let* ((replication-input
           (join-octets
            (make-frame #\W (copy-response-payload))
            (make-frame #\d (octets (char-code #\k)))))
         (connection (ready-memory-connection :input replication-input)))
    (unwind-protect
         (let ((operation
                 (replication-start
                  connection
                  "START_REPLICATION SLOT slot LOGICAL 0/0")))
           (assert-signals 'protocol-error
                           (lambda () (replication-read operation)))
           (is (not (connection-open connection)))
           (is (eq :failed (connection-state connection))))
      (disconnect connection))))

(deftest replication-lsn-codec
  (let ((registry (make-type-registry)))
    (is (= #x0000000100000002 (parse-replication-lsn "1/2")))
    (is (string= "1/2" (format-replication-lsn #x0000000100000002)))
    (is (string= "1/2"
                 (decode-value registry 3220
                               (cl-codec-kit:string-to-octets
                                "1/2" :encoding :utf-8))))
    (is (equalp (octets 0 0 0 1 0 0 0 2)
                (encode-value registry 3220 "1/2" :format 1)))
    (is (string= "1/2"
                 (decode-value registry 3220
                               (octets 0 0 0 1 0 0 0 2)
                               :format 1)))
    (is (string= "1/2"
                 (format-replication-lsn "00000001/00000002")))
    (assert-signals 'parameter-error
                    (lambda () (parse-replication-lsn -1)))
    (assert-signals 'parameter-error
                    (lambda () (parse-replication-lsn #x10000000000000000)))
    (dolist (value '("" "/1" "1/" "1/2/3" "1/Z" "G/1"))
      (assert-signals 'parameter-error
                      (lambda () (parse-replication-lsn value))))
    (assert-signals 'parameter-error
                    (lambda () (parse-replication-lsn #())))
    (assert-signals 'parameter-error
                    (lambda () (parse-replication-lsn "1/100000000")))
    (assert-signals 'parameter-error
                    (lambda () (parse-replication-lsn "invalid")))))

(deftest replication-sql-validation
  (is (string= "\"slot\"\"name\""
               (cl-postgresql-kit::%replication-quote-identifier "slot\"name")))
  (assert-signals 'parameter-error
                 (lambda ()
                   (cl-postgresql-kit::%replication-quote-identifier "")))
  (assert-signals 'parameter-error
                 (lambda ()
                   (cl-postgresql-kit::%replication-quote-identifier
                    (format nil "bad~Cname" (code-char 0)))))
  (is (string= "E'plain'"
               (cl-postgresql-kit::%replication-quote-literal "plain")))
  (let ((escaped
          (cl-postgresql-kit::%replication-quote-literal
           (format nil "line~Cquote~Cslash~Creturn~Ctab~C"
                   #\Newline #\' #\\ #\Return #\Tab))))
    (is (search (format nil "~C~C" #\\ #\n) escaped))
    (is (search (format nil "~C~C" #\\ #\') escaped))
    (is (search (format nil "~C~C" #\\ #\r) escaped))
    (is (search (format nil "~C~C" #\\ #\t) escaped))
    (is (search (format nil "~C~C" #\\ #\\) escaped)))
  (assert-signals 'parameter-error
                 (lambda ()
                   (cl-postgresql-kit::%replication-quote-literal
                    (format nil "bad~Cvalue" (code-char 0)))))
  (is (string= "PROTO_VERSION"
               (cl-postgresql-kit::%replication-option-name 'proto-version)))
  (is (string= "PUBLICATION_NAMES"
               (cl-postgresql-kit::%replication-option-name "publication-names")))
  (dolist (name (list "" "1bad" "bad.name" :bad?))
    (assert-signals 'parameter-error
                   (lambda ()
                     (cl-postgresql-kit::%replication-option-name name))))
  (is (string= "FLAG"
               (cl-postgresql-kit::%replication-option-sql '("flag"))))
  (is (string= "FLAG 7"
               (cl-postgresql-kit::%replication-option-sql '("flag" 7))))
  (is (string= "FLAG 7"
               (cl-postgresql-kit::%replication-option-sql
                (cons "flag" 7))))
  (is (string= "FLAG TRUE"
               (cl-postgresql-kit::%replication-option-sql '("flag" t))))
  (is (string= "FLAG"
               (cl-postgresql-kit::%replication-option-sql '("flag" nil))))
  (is (string= "(PROTO_VERSION 1, PUBLICATION_NAMES E'pub1,pub2')"
               (cl-postgresql-kit::%replication-options-sql
                '(("proto-version" 1) ("publication-names" "pub1,pub2")))))
  (is (null (cl-postgresql-kit::%replication-options-sql nil)))
  (assert-signals 'parameter-error
                 (lambda ()
                   (cl-postgresql-kit::%replication-option-sql nil)))
  (assert-signals 'parameter-error
                 (lambda ()
                   (cl-postgresql-kit::%replication-option-sql
                    '("flag" 1 2))))
  (assert-signals 'parameter-error
                 (lambda ()
                   (cl-postgresql-kit::%replication-options-sql
                    (cons '("flag" 1) 2))))
  (is (null (cl-postgresql-kit::%replication-timeline-sql nil)))
  (is (string= " TIMELINE 1"
               (cl-postgresql-kit::%replication-timeline-sql 1)))
  (is (string= " TIMELINE 4294967295"
               (cl-postgresql-kit::%replication-timeline-sql #xffffffff)))
  (dolist (timeline (list 0 -1 #x100000000 "1"))
    (assert-signals 'parameter-error
                   (lambda ()
                     (cl-postgresql-kit::%replication-timeline-sql timeline)))))

(deftest replication-control-commands
  (let* ((input
           (join-octets
            (catalog-query-input
             '(("systemid" 25 -1)
               ("timeline" 20 8)
               ("xlogpos" 3220 -1)
               ("dbname" 19 -1))
             '(("system-id" 7 "1/2" nil))
             "IDENTIFY_SYSTEM")
            (catalog-query-input
             '(("slot_name" 19 -1))
             '(("slot"))
             "CREATE_REPLICATION_SLOT")
            (catalog-query-input nil nil "ALTER_REPLICATION_SLOT")
            (catalog-query-input
             '(("slot_name" 19 -1))
             '(("slot"))
             "READ_REPLICATION_SLOT")
            (catalog-query-input nil nil "DROP_REPLICATION_SLOT")))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (let ((identity (replication-identify-system connection))
               (create (replication-create-slot connection "slot"
                                                 :reserve-wal-p t))
               (alter (replication-alter-slot connection "slot"
                                               :two-phase-p nil
                                               :failover-p t))
               (read (replication-read-slot connection "slot"))
               (drop (replication-drop-slot connection "slot" :wait-p t)))
           (is (string= "system-id" (getf identity :system-id)))
           (is (= 7 (getf identity :timeline)))
           (is (string= "1/2" (getf identity :xlog-position)))
           (is (null (getf identity :database)))
           (is (query-result-p create))
           (is (query-result-p alter))
           (is (query-result-p read))
           (is (query-result-p drop))
           (is (equalp
                (memory-transport-output (connection-transport connection))
                (join-octets
                 (encode-query-message "IDENTIFY_SYSTEM")
                 (encode-query-message
                  "CREATE_REPLICATION_SLOT \"slot\" PHYSICAL (RESERVE_WAL)")
                 (encode-query-message
                  "ALTER_REPLICATION_SLOT \"slot\" (TWO_PHASE FALSE, FAILOVER TRUE)")
                 (encode-query-message "READ_REPLICATION_SLOT \"slot\"")
                 (encode-query-message
                  "DROP_REPLICATION_SLOT \"slot\" WAIT")))))
      (disconnect connection))))

(deftest replication-show-and-timeline-history
  (let* ((input (join-octets
                 (catalog-query-input
                  '(("max_wal_senders" 25 -1))
                  '(("10"))
                  "SHOW")
                 (catalog-query-input
                  '(("filename" 25 -1) ("content" 25 -1))
                  '(("00000002.history" "history contents"))
                  "TIMELINE_HISTORY")))
         (connection (ready-memory-connection :input input))
         (show-result (replication-show connection "max_wal_senders"))
         (history-result (replication-timeline-history connection 2)))
    (is (query-result-p show-result))
    (is (= 1 (result-row-count show-result)))
    (is (query-result-p history-result))
    (is (= 1 (result-row-count history-result)))
    (is (equalp (join-octets
                 (encode-query-message "SHOW \"max_wal_senders\"")
                 (encode-query-message "TIMELINE_HISTORY 2"))
                (memory-transport-output (connection-transport connection))))
    (disconnect connection)))

(deftest replication-upload-manifest
  (let* ((input (join-octets
                 (make-frame #\G (copy-response-payload))
                 (make-frame #\C (cstring "COPY 1"))
                 (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input))
         (manifest "{\"PostgreSQL-Database\":\"example\"}")
         (result (replication-upload-manifest connection manifest)))
    (is (string= "COPY 1" result))
    (is (equalp (join-octets
                 (encode-query-message "UPLOAD_MANIFEST")
                 (encode-copy-data-message manifest)
                 (encode-copy-done-message))
                (memory-transport-output (connection-transport connection))))
    (disconnect connection)))

(deftest replication-base-backup
  (labels ((without-ready (bytes)
             (let ((ready-length
                     (length (make-frame #\Z (octets (char-code #\I))))))
               (subseq bytes 0 (- (length bytes) ready-length)))))
    (let* ((input (join-octets
                   (without-ready
                    (catalog-query-input
                     '(("start_lsn" 25 -1) ("start_tli" 25 -1))
                     '(("0/16" "2"))
                     "BASE_BACKUP"))
                   (without-ready
                    (catalog-query-input
                     '(("tablespace_oid" 25 -1)
                       ("tablespace_name" 25 -1)
                       ("tablespace_path" 25 -1))
                     '(("1663" "pg_default" "/var/lib/postgresql/data"))
                     "BASE_BACKUP"))
                   (make-frame #\H (copy-response-payload))
                   (make-frame #\d
                               (join-octets (octets (char-code #\n))
                                            (cstring "base.tar")
                                            (cstring "/var/lib/postgresql/data")))
                   (make-frame #\d
                               (join-octets (octets (char-code #\d))
                                            (octets 1 2 3)))
                   (make-frame #\c #())
                   (make-frame #\H (copy-response-payload))
                   (make-frame #\d (octets (char-code #\m)))
                   (make-frame #\d
                               (let ((builder (make-octet-builder)))
                                 (append-u8 builder (char-code #\p))
                                 (append-u64 builder 42)
                                 (builder-octets builder)))
                   (make-frame #\c #())
                   (catalog-query-input
                    '(("wal_start" 25 -1)
                      ("wal_end" 25 -1)
                      ("timeline" 25 -1))
                    '(("0/16" "0/20" "2"))
                    "BASE_BACKUP")))
           (connection (ready-memory-connection :input input))
           (operation
             (replication-base-backup-start
              connection
              :label "nightly"
              :target :client
              :target-detail "stream"
              :progress-p t
              :checkpoint :spread
              :wal-p nil
              :wait-p t
              :compression :gzip
              :compression-detail 6
              :max-rate 32
              :tablespace-map-p nil
              :verify-checksums-p t
              :manifest :force-encode
              :manifest-checksums :sha256
              :incremental-p t
              :options '(("custom" . "value"))))
           (archive (base-backup-read operation))
           (data (base-backup-read operation))
           (manifest (base-backup-read operation))
           (progress (base-backup-read operation))
           (finished (base-backup-read operation)))
      (is (= 2 (length (base-backup-start-results operation))))
      (is (eq :new-archive (base-backup-message-kind archive)))
      (is (string= "base.tar"
                   (base-backup-message-archive-name archive)))
      (is (eq :data (base-backup-message-kind data)))
      (is (equalp #(1 2 3) (base-backup-message-data data)))
      (is (eq :manifest (base-backup-message-kind manifest)))
      (is (eq :progress (base-backup-message-kind progress)))
      (is (= 42 (base-backup-message-bytes-completed progress)))
      (is (null finished))
      (is (= 1 (length (base-backup-final-results operation))))
      (is (equalp
           (join-octets
            (encode-query-message
             "BASE_BACKUP (LABEL E'nightly', TARGET E'client', TARGET_DETAIL E'stream', PROGRESS TRUE, CHECKPOINT E'spread', WAL FALSE, WAIT TRUE, COMPRESSION E'gzip', COMPRESSION_DETAIL 6, MAX_RATE 32, TABLESPACE_MAP FALSE, VERIFY_CHECKSUMS TRUE, MANIFEST E'force-encode', MANIFEST_CHECKSUMS E'sha256', INCREMENTAL, CUSTOM E'value')"))
           (memory-transport-output (connection-transport connection))))
      (disconnect connection))))

(deftest replication-base-backup-validates-options
  (assert-signals 'parameter-error
                  (lambda ()
                    (cl-postgresql-kit::%replication-base-backup-sql
                     :target :invalid)))
  (assert-signals 'parameter-error
                  (lambda ()
                    (cl-postgresql-kit::%replication-base-backup-sql
                     :max-rate 31)))
  (assert-signals 'parameter-error
                  (lambda ()
                    (cl-postgresql-kit::%replication-base-backup-sql
                     :progress-p :yes))))

(deftest replication-alter-slot-validates-options
  (assert-signals 'parameter-error
                 (lambda ()
                   (replication-alter-slot nil "slot")))
  (dolist (value '(0 "true" :yes))
    (assert-signals 'parameter-error
                   (lambda ()
                     (replication-alter-slot
                      nil "slot" :two-phase-p value))))
  (assert-signals 'parameter-error
                 (lambda ()
                   (replication-alter-slot
                    nil "slot" :options (cons '("TWO_PHASE") 2)))))

(deftest replication-start-helpers
  (let* ((input
           (join-octets
            (make-frame #\W (copy-response-payload))
            (make-frame #\c #())
            (make-frame #\C (cstring "START_REPLICATION"))
            (make-frame #\Z (octets (char-code #\I)))
            (make-frame #\W (copy-response-payload))
            (make-frame #\c #())
            (make-frame #\C (cstring "START_REPLICATION"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (let ((physical
                 (replication-start-physical connection "slot" "1/2"
                                              :timeline 3)))
           (is (string= "START_REPLICATION"
                        (replication-finish physical)))
           (let ((logical
                   (replication-start-logical
                    connection "slot" "1/2"
                    :options '(("proto_version" . "1")
                               ("publication_names" . "pub1,pub2")))))
             (is (string= "START_REPLICATION"
                          (replication-finish logical)))
             (is (equalp
                  (memory-transport-output (connection-transport connection))
                  (join-octets
                   (encode-query-message
                    "START_REPLICATION SLOT \"slot\" PHYSICAL 1/2 TIMELINE 3")
                   (encode-copy-done-message)
                   (encode-query-message
                    "START_REPLICATION SLOT \"slot\" LOGICAL 1/2 (PROTO_VERSION E'1', PUBLICATION_NAMES E'pub1,pub2')")
                   (encode-copy-done-message))))))
      (disconnect connection))))

(deftest copy-formats-are-validated
  (assert-signals 'protocol-error
                 (lambda () (parse-copy-response #())))
  (assert-signals 'protocol-error
                 (lambda () (parse-copy-response (octets 2 0 0))))
  (assert-signals 'protocol-error
                 (lambda () (parse-copy-response (octets 0 0 1 0 2)))))
