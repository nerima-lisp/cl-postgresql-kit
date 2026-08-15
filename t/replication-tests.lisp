(in-package #:cl-postgresql-kit/test)

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
    (it-signals-each 'protocol-error
        ((:unknown-kind)
         (:manifest-trailing-bytes)
         (:truncated-new-archive)
         (:oversized-progress-payload))
      "rejects malformed base backup message case ~A"
      (label)
      (parse-base-backup-message
       (ecase label
         (:unknown-kind
          (octets (char-code #\x)))
         (:manifest-trailing-bytes
          (join-octets (octets (char-code #\m))
                       (octets 0)))
         (:truncated-new-archive
          (octets (char-code #\n) 1))
         (:oversized-progress-payload
          (octets (char-code #\p) 0 0 0 0 0 0 0 0 1 0)))))))

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
            (make-frame #\Z (octets (char-code #\I))))))
    (with-ready-memory-test-connection (connection :input replication-input)
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
              (encode-copy-done-message))))))))

(deftest replication-malformed-payload-retires-connection
  (let* ((replication-input
           (join-octets
            (make-frame #\W (copy-response-payload))
            (make-frame #\d (octets (char-code #\k))))))
    (with-ready-memory-test-connection (connection :input replication-input)
      (let ((operation
              (replication-start
               connection
               "START_REPLICATION SLOT slot LOGICAL 0/0")))
        (assert-signals 'protocol-error
                        (lambda () (replication-read operation)))
        (is (not (connection-open connection)))
        (is (eq :failed (connection-state connection)))))))
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

(deftest replication-base-backup-requires-copy-done-before-ready
  (let* ((input (join-octets
                 (make-frame #\H (copy-response-payload))
                 (make-frame #\d
                             (join-octets (octets (char-code #\d))
                                          (octets 1 2 3)))
                 (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input))
         (operation (replication-base-backup-start connection)))
    (unwind-protect
         (progn
           (is (eq :data (base-backup-message-kind
                          (base-backup-read operation))))
           (assert-signals
               'copy-error
             (lambda ()
               (base-backup-read operation))))
      (disconnect connection))))
