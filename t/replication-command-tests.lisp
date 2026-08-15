(in-package #:cl-postgresql-kit/test)

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
    (it-signals-each 'parameter-error
        ((-1)
         (#x10000000000000000)
         ("")
         ("/1")
         ("1/")
         ("1/2/3")
         ("1/Z")
         ("G/1")
         (#())
         ("1/100000000")
         ("invalid"))
      "rejects invalid replication LSN input ~S"
      (value)
      (parse-replication-lsn value))))

(deftest replication-sql-validation
  (is (string= "\"slot\"\"name\""
               (cl-postgresql-kit::%replication-quote-identifier "slot\"name")))
  (it-signals-each 'parameter-error
      ((:empty)
       (:embedded-nul))
    "replication quote identifier rejects invalid input case ~A"
    (label)
    (cl-postgresql-kit::%replication-quote-identifier
     (ecase label
       (:empty "")
       (:embedded-nul
        (format nil "bad~Cname" (code-char 0))))))
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
  (it-signals-each 'parameter-error
      ((:embedded-nul))
    "replication quote literal rejects invalid input case ~A"
    (label)
    (declare (ignore label))
    (cl-postgresql-kit::%replication-quote-literal
     (format nil "bad~Cvalue" (code-char 0))))
  (is (string= "PROTO_VERSION"
               (cl-postgresql-kit::%replication-option-name 'proto-version)))
  (is (string= "PUBLICATION_NAMES"
               (cl-postgresql-kit::%replication-option-name "publication-names")))
  (it-signals-each 'parameter-error
      (("")
       ("1bad")
       ("bad.name")
       (:bad?))
    "replication option name rejects invalid input ~S"
    (value)
    (cl-postgresql-kit::%replication-option-name value))
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
  (it-signals-each 'parameter-error
      ((:missing-option)
       (:too-many-option-values)
       (:non-list-options))
    "replication option SQL rejects invalid input case ~A"
    (label)
    (ecase label
      (:missing-option
       (cl-postgresql-kit::%replication-option-sql nil))
      (:too-many-option-values
       (cl-postgresql-kit::%replication-option-sql
        '("flag" 1 2)))
      (:non-list-options
       (cl-postgresql-kit::%replication-options-sql
        (cons '("flag" 1) 2)))))
  (is (null (cl-postgresql-kit::%replication-timeline-sql nil)))
  (is (string= " TIMELINE 1"
               (cl-postgresql-kit::%replication-timeline-sql 1)))
  (is (string= " TIMELINE 4294967295"
               (cl-postgresql-kit::%replication-timeline-sql #xffffffff)))
  (it-signals-each 'parameter-error
      ((0)
       (-1)
       (#x100000000)
       ("1"))
    "replication timeline SQL rejects invalid input ~S"
    (value)
    (cl-postgresql-kit::%replication-timeline-sql value)))

(cl-weave:it-each
    ((("flag") "FLAG")
     (("flag" 7) "FLAG 7")
     (("flag" t) "FLAG TRUE")
     (("flag" nil) "FLAG"))
  "replication option SQL rendering preserves scalar and boolean forms"
  (option expected)
  (is (string= expected
               (cl-postgresql-kit::%replication-option-sql option))))

(cl-weave:it-each
    (((:two-phase-p nil :failover-p t)
      "(TWO_PHASE FALSE, FAILOVER TRUE)")
     ((:label "nightly" :progress-p t :wait-p nil :max-rate 32 :incremental-p t)
      "BASE_BACKUP (LABEL E'nightly', PROGRESS TRUE, WAIT FALSE, MAX_RATE 32, INCREMENTAL)"))
  "replication command SQL keeps option order stable"
  (arguments expected)
  (let ((actual
          (apply (if (search "BASE_BACKUP" expected)
                     #'cl-postgresql-kit::%replication-base-backup-sql
                     #'cl-postgresql-kit::%replication-alter-slot-options-sql)
                 (if (search "BASE_BACKUP" expected)
                     arguments
                     (list (getf arguments :two-phase-p)
                           (member :two-phase-p arguments)
                           (getf arguments :failover-p)
                           (member :failover-p arguments)
                           nil)))))
    (is (string= expected actual))))

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

(it-signals-each
    'parameter-error
    (((:target :invalid))
     ((:max-rate 31))
     ((:progress-p :yes)))
  "BASE_BACKUP SQL rejects invalid option values"
  (arguments)
  (apply #'cl-postgresql-kit::%replication-base-backup-sql
         arguments))

(deftest replication-alter-slot-validates-options
  (assert-signals 'parameter-error
                 (lambda ()
                   (replication-alter-slot nil "slot")))
  (it-signals-each
      'parameter-error
      (((:two-phase-p 0))
       ((:two-phase-p "true"))
       ((:two-phase-p :yes))
       ((:options #1=(("TWO_PHASE") . 2))))
    "ALTER_REPLICATION_SLOT rejects invalid option payloads"
    (arguments)
    (apply #'replication-alter-slot nil "slot" arguments)))

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
