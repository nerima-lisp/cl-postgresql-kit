(in-package #:cl-postgresql-kit)

(defun %parse-logical-replication-stream-commit (payload protocol-version)
  (%logical-replication-require-version protocol-version 2 :stream-commit)
  (%replication-require-length payload 30 :pgoutput-stream-commit)
  (%ensure-payload-end payload 30 :pgoutput-stream-commit)
  (multiple-value-bind (xid position) (%read-u32 payload 1)
    (let ((flags (%octet-at payload position)))
      (incf position)
      (multiple-value-bind (commit-lsn position) (%read-u64 payload position)
        (multiple-value-bind (end-lsn position) (%read-u64 payload position)
          (multiple-value-bind (commit-time position) (%read-i64 payload position)
            (declare (ignore position))
            (%logical-replication-message
             :stream-commit protocol-version
             :xid xid
             :flags flags
             :commit-lsn commit-lsn
             :end-lsn end-lsn
             :commit-time commit-time)))))))

(defun %parse-logical-replication-stream-abort
    (payload protocol-version parallel-streaming-p)
  (%logical-replication-require-version protocol-version 2 :stream-abort)
  (if parallel-streaming-p
      (progn
        (%replication-require-length payload 25 :pgoutput-stream-abort)
        (%ensure-payload-end payload 25 :pgoutput-stream-abort))
      (progn
        (%replication-require-length payload 9 :pgoutput-stream-abort)
        (%ensure-payload-end payload 9 :pgoutput-stream-abort)))
  (multiple-value-bind (xid position) (%read-u32 payload 1)
    (multiple-value-bind (subtransaction-id position)
        (%read-u32 payload position)
      (if parallel-streaming-p
          (multiple-value-bind (abort-lsn position) (%read-u64 payload position)
            (multiple-value-bind (abort-time position) (%read-i64 payload position)
              (declare (ignore position))
              (%logical-replication-message
               :stream-abort protocol-version
               :xid xid
               :subtransaction-id subtransaction-id
               :abort-lsn abort-lsn
               :abort-time abort-time)))
          (%logical-replication-message
           :stream-abort protocol-version
           :xid xid
           :subtransaction-id subtransaction-id)))))

(defun %parse-logical-replication-begin-prepare (payload protocol-version)
  (%logical-replication-require-version protocol-version 3 :begin-prepare)
  (%replication-require-length payload 29 :pgoutput-begin-prepare)
  (let ((position 1))
    (%with-logical-replication-prepared-message
        (payload position :pgoutput-begin-prepare
         (prepare-lsn (%read-u64 payload position))
         (prepare-end-lsn (%read-u64 payload position))
         (prepare-time (%read-i64 payload position)))
      (%logical-replication-message
       :begin-prepare protocol-version
       :xid xid
       :prepare-lsn prepare-lsn
       :prepare-end-lsn prepare-end-lsn
       :prepare-time prepare-time
       :gid gid))))

(defun %parse-logical-replication-prepare-message
    (payload protocol-version kind version-kind context)
  (%logical-replication-require-version protocol-version 3 version-kind)
  (%replication-require-length payload 30 context)
  (let ((flags (%octet-at payload 1)))
    (let ((position 2))
      (%with-logical-replication-prepared-message
          (payload position context
           (prepare-lsn (%read-u64 payload position))
           (prepare-end-lsn (%read-u64 payload position))
           (prepare-time (%read-i64 payload position)))
        (%logical-replication-message
         kind protocol-version
         :flags flags
         :xid xid
         :prepare-lsn prepare-lsn
         :prepare-end-lsn prepare-end-lsn
         :prepare-time prepare-time
         :gid gid)))))

(defun %parse-logical-replication-commit-prepared (payload protocol-version)
  (%logical-replication-require-version protocol-version 3 :commit-prepared)
  (%replication-require-length payload 30 :pgoutput-commit-prepared)
  (let ((flags (%octet-at payload 1)))
    (let ((position 2))
      (%with-logical-replication-prepared-message
          (payload position :pgoutput-commit-prepared
           (commit-lsn (%read-u64 payload position))
           (end-lsn (%read-u64 payload position))
           (commit-time (%read-i64 payload position)))
        (%logical-replication-message
         :commit-prepared protocol-version
         :flags flags
         :xid xid
         :commit-lsn commit-lsn
         :end-lsn end-lsn
         :commit-time commit-time
         :gid gid)))))

(defun %parse-logical-replication-rollback-prepared (payload protocol-version)
  (%logical-replication-require-version protocol-version 3 :rollback-prepared)
  (%replication-require-length payload 38 :pgoutput-rollback-prepared)
  (let ((flags (%octet-at payload 1)))
    (let ((position 2))
      (%with-logical-replication-prepared-message
          (payload position :pgoutput-rollback-prepared
           (prepare-end-lsn (%read-u64 payload position))
           (rollback-end-lsn (%read-u64 payload position))
           (prepare-time (%read-i64 payload position))
           (rollback-time (%read-i64 payload position)))
        (%logical-replication-message
         :rollback-prepared protocol-version
         :flags flags
         :xid xid
         :prepare-end-lsn prepare-end-lsn
         :rollback-end-lsn rollback-end-lsn
         :prepare-time prepare-time
         :rollback-time rollback-time
         :gid gid)))))

(defun %parse-logical-replication-begin (payload protocol-version)
  (%replication-require-length payload 21 :pgoutput-begin)
  (%ensure-payload-end payload 21 :pgoutput-begin)
  (let ((position 1))
    (%with-replication-fields
        (position
         (final-lsn (%read-u64 payload position))
         (commit-time (%read-i64 payload position))
         (xid (%read-u32 payload position)))
      (%logical-replication-message
       :begin protocol-version
       :xid xid
       :final-lsn final-lsn
       :commit-time commit-time))))

(defun %parse-logical-replication-message-body
    (payload protocol-version streamed-p)
  (%with-logical-replication-stream-context
      (payload position streamed-p xid)
    (let ((flags (%octet-at payload position)))
      (incf position)
      (multiple-value-bind (message-lsn next) (%read-u64 payload position)
        (setf position next)
        (multiple-value-bind (prefix next) (%read-cstring payload position)
          (setf position next)
          (multiple-value-bind (content next)
              (%logical-replication-read-byte-data
               payload position :pgoutput-message)
            (setf position next)
            (%ensure-payload-end payload position :pgoutput-message)
            (%logical-replication-message
             :message protocol-version
             :xid xid
             :flags flags
             :message-lsn message-lsn
             :prefix prefix
             :content content)))))))

(defun %parse-logical-replication-commit (payload protocol-version)
  (%replication-require-length payload 26 :pgoutput-commit)
  (%ensure-payload-end payload 26 :pgoutput-commit)
  (let ((flags (%octet-at payload 1)))
    (let ((position 2))
      (%with-replication-fields
          (position
           (commit-lsn (%read-u64 payload position))
           (end-lsn (%read-u64 payload position))
           (commit-time (%read-i64 payload position)))
        (%logical-replication-message
         :commit protocol-version
         :flags flags
         :commit-lsn commit-lsn
         :end-lsn end-lsn
         :commit-time commit-time)))))

(defun %parse-logical-replication-origin (payload protocol-version)
  (%replication-require-length payload 10 :pgoutput-origin)
  (let ((position 1))
    (%with-replication-fields
        (position
         (origin-lsn (%read-u64 payload position))
         (origin-name (%read-cstring payload position)))
      (%ensure-payload-end payload position :pgoutput-origin)
      (%logical-replication-message
       :origin protocol-version
       :origin-lsn origin-lsn
       :origin-name origin-name))))

(defun %parse-logical-replication-stream-start (payload protocol-version)
  (%logical-replication-require-version protocol-version 2 :stream-start)
  (%replication-require-length payload 6 :pgoutput-stream-start)
  (%ensure-payload-end payload 6 :pgoutput-stream-start)
  (multiple-value-bind (xid position) (%read-u32 payload 1)
    (let ((first-segment (%octet-at payload position)))
      (%logical-replication-message
       :stream-start protocol-version
       :xid xid
       :first-segment (= first-segment 1)))))

(defun %parse-logical-replication-stream-stop (protocol-version)
  (%logical-replication-require-version protocol-version 2 :stream-stop)
  (%logical-replication-message
   :stream-stop protocol-version))

(defun parse-logical-replication-message
    (payload &key (protocol-version 1) (streamed-p nil)
                     (parallel-streaming-p nil))
  "Parse one PostgreSQL pgoutput logical-replication message payload.

The payload is the marker-prefixed data inside an XLogData message.  Set
STREAMED-P when the payload belongs to a streamed transaction under
pgoutput protocol version 2 or newer; this enables the transaction ID
prefixes defined by PostgreSQL.  Set PARALLEL-STREAMING-P for the extended
Abort payload introduced by protocol version 4.  TupleData text and binary
values remain octet vectors so callers can decode them with the relation's
type registry instead of guessing at this protocol boundary."
  (%wire-check-octets payload)
  (%logical-replication-check-options protocol-version streamed-p
                                      parallel-streaming-p)
  (let ((kind (%octet-at payload 0)))
    (case kind
      (#x42 (%parse-logical-replication-begin payload protocol-version))
      (#x4d (%parse-logical-replication-message-body
             payload protocol-version streamed-p))
      (#x43 (%parse-logical-replication-commit payload protocol-version))
      (#x4f (%parse-logical-replication-origin payload protocol-version))
      (#x52 (%parse-logical-replication-relation
             payload protocol-version streamed-p))
      (#x59 (%parse-logical-replication-type
             payload protocol-version streamed-p))
      (#x49 (%parse-logical-replication-insert
             payload protocol-version streamed-p))
      (#x55 (%parse-logical-replication-update
             payload protocol-version streamed-p))
      (#x44 (%parse-logical-replication-delete
             payload protocol-version streamed-p))
      (#x54 (%parse-logical-replication-truncate
             payload protocol-version streamed-p))
      (#x53 (%parse-logical-replication-stream-start payload protocol-version))
      (#x45
       (%ensure-payload-end payload 1 :pgoutput-stream-stop)
       (%parse-logical-replication-stream-stop protocol-version))
      (#x63
       (%parse-logical-replication-stream-commit payload protocol-version))
      (#x41
       (%parse-logical-replication-stream-abort
        payload protocol-version parallel-streaming-p))
      (#x62
       (%parse-logical-replication-begin-prepare payload protocol-version))
      (#x50
       (%parse-logical-replication-prepare-message
        payload protocol-version :prepare :prepare :pgoutput-prepare))
      (#x4b
       (%parse-logical-replication-commit-prepared payload protocol-version))
      (#x72
       (%parse-logical-replication-rollback-prepared
        payload protocol-version))
      (#x70
       (%parse-logical-replication-prepare-message
        payload protocol-version :stream-prepare
        :stream-prepare :pgoutput-stream-prepare))
      (otherwise
       (error 'protocol-error
              :message "Unknown PostgreSQL pgoutput message type"
              :context :pgoutput
              :expected '(#x42 #x4d #x43 #x4f #x52 #x59 #x49 #x55 #x44
                          #x54 #x53 #x45 #x63 #x41 #x62 #x50 #x4b #x72 #x70)
              :actual kind)))))
