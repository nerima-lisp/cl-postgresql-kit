(in-package #:cl-postgresql-kit/test)

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
    (is-case-each
        ((marker expected-kind)
         '((#\P :prepare)
           (#\K :commit-prepared)
           (#\r :rollback-prepared)
           (#\p :stream-prepare)))
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
        (is (string= "gid" (logical-replication-message-gid message)))))))

(deftest logical-replication-message-validation
  (labels ((payload (marker writer)
             (let ((builder (make-octet-builder)))
               (append-u8 builder (char-code marker))
               (funcall writer builder)
               (builder-octets builder))))
    (it-signals-each 'parameter-error
        ((:protocol-version-too-small)
         (:protocol-version-too-large)
         (:streamed-p-type)
         (:parallel-streaming-p-without-streaming))
      "~a rejects invalid parser options."
      (case)
      (ecase case
        (:protocol-version-too-small
         (parse-logical-replication-message #() :protocol-version 0))
        (:protocol-version-too-large
         (parse-logical-replication-message #() :protocol-version 5))
        (:streamed-p-type
         (parse-logical-replication-message #() :streamed-p :yes))
        (:parallel-streaming-p-without-streaming
         (parse-logical-replication-message #() :parallel-streaming-p t))))
    (it-signals-each 'unsupported-feature
        ((:stream-start-in-v1)
         (:stream-stop-in-v1)
         (:begin-stream-in-v2)
         (:stream-abort-in-v3-parallel))
      "~a rejects unsupported logical replication messages."
      (case)
      (ecase case
        (:stream-start-in-v1
         (parse-logical-replication-message
          (octets (char-code #\S) 0 0 0 1 0)
          :protocol-version 1))
        (:stream-stop-in-v1
         (parse-logical-replication-message
          (octets (char-code #\E))
          :protocol-version 1))
        (:begin-stream-in-v2
         (parse-logical-replication-message
          (octets (char-code #\b) 0)
          :protocol-version 2))
        (:stream-abort-in-v3-parallel
         (parse-logical-replication-message
          (octets (char-code #\A) 0 0 0 1 0 0 0 2 0)
          :protocol-version 3
          :streamed-p t
          :parallel-streaming-p t))))
    (it-signals-each 'protocol-error
        ((:unknown-message-tag)
         (:stream-stop-truncated)
         (:message-negative-content-length)
         (:insert-invalid-tuple-kind)
         (:update-invalid-old-tuple-kind)
         (:delete-invalid-old-tuple-kind)
         (:truncate-negative-relation-count)
         (:relation-negative-column-count))
      "~a rejects malformed logical replication payloads."
      (case)
      (ecase case
        (:unknown-message-tag
         (parse-logical-replication-message
          (octets (char-code #\x))))
        (:stream-stop-truncated
         (parse-logical-replication-message
          (octets (char-code #\E) 0)
          :protocol-version 2))
        (:message-negative-content-length
         (parse-logical-replication-message
          (payload #\M
                   (lambda (builder)
                     (append-u8 builder 0)
                     (append-u64 builder 1)
                     (append-octets builder (cstring "x"))
                     (append-i32 builder -1)))))
        (:insert-invalid-tuple-kind
         (parse-logical-replication-message
          (payload #\I
                   (lambda (builder)
                     (append-u32 builder 1)
                     (append-u8 builder (char-code #\X))))))
        (:update-invalid-old-tuple-kind
         (parse-logical-replication-message
          (payload #\U
                   (lambda (builder)
                     (append-u32 builder 1)
                     (append-u8 builder (char-code #\X))))))
        (:delete-invalid-old-tuple-kind
         (parse-logical-replication-message
          (payload #\D
                   (lambda (builder)
                     (append-u32 builder 1)
                     (append-u8 builder (char-code #\N))))))
        (:truncate-negative-relation-count
         (parse-logical-replication-message
          (payload #\T
                   (lambda (builder)
                     (append-i32 builder -1)
                     (append-u8 builder 0)))))
        (:relation-negative-column-count
         (parse-logical-replication-message
          (payload #\R
                   (lambda (builder)
                     (append-u32 builder 1)
                     (append-octets builder (cstring "public"))
                     (append-octets builder (cstring "items"))
                     (append-u8 builder 0)
                     (append-i16 builder -1)))))))))
