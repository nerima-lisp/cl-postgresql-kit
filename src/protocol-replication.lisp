(in-package #:cl-postgresql-kit)

(defstruct (replication-message
             (:constructor %make-replication-message
                 (&key kind wal-start wal-end send-time reply-requested data
                       client-time write-lsn flush-lsn apply-lsn xmin xmin-epoch
                       catalog-xmin catalog-xmin-epoch)))
  kind
  wal-start
  wal-end
  send-time
  reply-requested
  data
  client-time
  write-lsn
  flush-lsn
  apply-lsn
  xmin
  xmin-epoch
  catalog-xmin
  catalog-xmin-epoch)

(defstruct (base-backup-message
             (:constructor %make-base-backup-message
                 (&key kind archive-name tablespace-path data bytes-completed)))
  "One PostgreSQL BASE_BACKUP CopyData record.

The KIND is one of :NEW-ARCHIVE, :MANIFEST, :DATA, or :PROGRESS.  The
corresponding payload is available through ARCHIVE-NAME and TABLESPACE-PATH,
DATA, or BYTES-COMPLETED respectively."
  kind
  archive-name
  tablespace-path
  data
  bytes-completed)

(defstruct (logical-replication-field
             (:constructor %make-logical-replication-field (&key kind data)))
  "One pgoutput TupleData field.

KIND is one of :NULL, :UNCHANGED-TOAST, :TEXT, or :BINARY.  DATA is
either +SQL-NULL+, NIL, or a newly allocated octet vector.  Text values are
kept as octets because decoding them requires the relation's PostgreSQL
type metadata, which is intentionally outside this wire-level parser."
  kind
  data)

(defstruct (logical-replication-tuple
             (:constructor %make-logical-replication-tuple (&key fields)))
  "A pgoutput TupleData value represented as a vector of fields."
  fields)

(defstruct (logical-replication-column
             (:constructor %make-logical-replication-column
                 (&key flags name type-oid type-modifier)))
  "A column definition from a pgoutput Relation message."
  flags
  name
  type-oid
  type-modifier)

(defstruct (logical-replication-message
             (:constructor %make-logical-replication-message
                 (&key kind protocol-version xid final-lsn commit-lsn end-lsn
                       commit-time flags relation-id namespace relation-name
                       replica-identity columns type-oid type-namespace type-name
                       new-tuple old-tuple old-tuple-kind relation-ids options
                       first-segment subtransaction-id abort-lsn abort-time
                       message-lsn prefix content origin-lsn origin-name
                       prepare-lsn prepare-end-lsn prepare-time rollback-end-lsn
                       rollback-time gid)))
  "A parsed pgoutput logical-replication message.

Only slots relevant to KIND are non-NIL.  Metadata strings are decoded as
UTF-8 cstrings at the PostgreSQL wire boundary, while TupleData payloads
remain octets until a relation/type registry can interpret them."
  kind
  protocol-version
  xid
  final-lsn
  commit-lsn
  end-lsn
  commit-time
  flags
  relation-id
  namespace
  relation-name
  replica-identity
  columns
  type-oid
  type-namespace
  type-name
  new-tuple
  old-tuple
  old-tuple-kind
  relation-ids
  options
  first-segment
  subtransaction-id
  abort-lsn
  abort-time
  message-lsn
  prefix
  content
  origin-lsn
  origin-name
  prepare-lsn
  prepare-end-lsn
  prepare-time
  rollback-end-lsn
  rollback-time
  gid)

(defun %replication-require-length (payload minimum context)
  (when (< (length payload) minimum)
    (error 'protocol-error
           :message "PostgreSQL replication message is truncated"
           :context context
           :expected minimum
           :actual (length payload))))

(defun %replication-reply-requested (value context)
  (unless (member value '(0 1))
    (error 'protocol-error
           :message "PostgreSQL replication reply flag must be zero or one"
           :context context
           :expected '(0 1)
           :actual value))
  (= value 1))

(defun parse-replication-message (payload)
  "Parse one PostgreSQL streaming-replication COPY DATA payload.

The returned REPLICATION-MESSAGE represents XLogData, primary keepalive,
standby status update, or hot standby feedback.  WAL positions and XIDs are
returned as non-negative integers, while PostgreSQL timestamps are signed
microseconds since 2000-01-01 UTC."
  (%wire-check-octets payload)
  (let ((kind (%octet-at payload 0)))
    (case kind
      (#x77
       (%replication-require-length payload 25 :xlog-data)
       (multiple-value-bind (wal-start position) (%read-u64 payload 1)
         (multiple-value-bind (wal-end position) (%read-u64 payload position)
           (multiple-value-bind (send-time position) (%read-i64 payload position)
             (%make-replication-message
              :kind :xlog-data
              :wal-start wal-start
              :wal-end wal-end
              :send-time send-time
              :data (subseq payload position))))))
      (#x6b
       (%replication-require-length payload 18 :primary-keepalive)
       (%ensure-payload-end payload 18 :primary-keepalive)
       (multiple-value-bind (wal-end position) (%read-u64 payload 1)
         (multiple-value-bind (send-time position) (%read-i64 payload position)
           (%make-replication-message
            :kind :primary-keepalive
            :wal-end wal-end
            :send-time send-time
            :reply-requested
            (%replication-reply-requested
             (%octet-at payload position) :primary-keepalive)))))
      (#x72
       (%replication-require-length payload 34 :standby-status-update)
       (%ensure-payload-end payload 34 :standby-status-update)
       (multiple-value-bind (write-lsn position) (%read-u64 payload 1)
         (multiple-value-bind (flush-lsn position) (%read-u64 payload position)
           (multiple-value-bind (apply-lsn position) (%read-u64 payload position)
             (multiple-value-bind (client-time position)
                 (%read-i64 payload position)
               (%make-replication-message
                :kind :standby-status-update
                :write-lsn write-lsn
                :flush-lsn flush-lsn
                :apply-lsn apply-lsn
                :client-time client-time
                :reply-requested
                (%replication-reply-requested
                 (%octet-at payload position) :standby-status-update)))))))
      (#x68
       (%replication-require-length payload 25 :hot-standby-feedback)
       (%ensure-payload-end payload 25 :hot-standby-feedback)
       (multiple-value-bind (client-time position) (%read-i64 payload 1)
         (multiple-value-bind (xmin position) (%read-u32 payload position)
           (multiple-value-bind (xmin-epoch position) (%read-u32 payload position)
             (multiple-value-bind (catalog-xmin position)
                 (%read-u32 payload position)
               (multiple-value-bind (catalog-xmin-epoch position)
                   (%read-u32 payload position)
                 (declare (ignore position))
                 (%make-replication-message
                  :kind :hot-standby-feedback
                  :client-time client-time
                  :xmin xmin
                  :xmin-epoch xmin-epoch
                  :catalog-xmin catalog-xmin
                  :catalog-xmin-epoch catalog-xmin-epoch)))))))
      (otherwise
       (error 'protocol-error
              :message "Unknown PostgreSQL replication message type"
              :context :replication
              :expected '(#x77 #x6b #x72 #x68)
              :actual kind)))))

(defun parse-base-backup-message (payload)
  "Parse one BASE_BACKUP CopyData payload.

PostgreSQL prefixes each BASE_BACKUP payload with a one-byte marker.  The
returned BASE-BACKUP-MESSAGE keeps archive metadata, manifest bytes, backup
data, and progress values distinct so callers do not have to interpret the
wire marker themselves."
  (%wire-check-octets payload)
  (%replication-require-length payload 1 :base-backup)
  (case (%octet-at payload 0)
    (#x6e
     (multiple-value-bind (archive-name position)
         (%read-cstring payload 1)
       (multiple-value-bind (tablespace-path position)
           (%read-cstring payload position)
         (%ensure-payload-end payload position :base-backup-new-archive)
         (%make-base-backup-message
          :kind :new-archive
          :archive-name archive-name
          :tablespace-path tablespace-path))))
    (#x6d
     (%ensure-payload-end payload 1 :base-backup-manifest)
     (%make-base-backup-message :kind :manifest))
    (#x64
     (%make-base-backup-message
      :kind :data
      :data (subseq payload 1)))
    (#x70
     (%replication-require-length payload 9 :base-backup-progress)
     (%ensure-payload-end payload 9 :base-backup-progress)
     (%make-base-backup-message
      :kind :progress
      :bytes-completed (nth-value 0 (%read-u64 payload 1))))
    (otherwise
     (error 'protocol-error
            :message "Unknown PostgreSQL BASE_BACKUP CopyData marker"
            :context :base-backup
            :expected '(#x6e #x6d #x64 #x70)
            :actual (%octet-at payload 0)))))

(defun %logical-replication-check-options (protocol-version streamed-p
                                           parallel-streaming-p)
  (unless (and (integerp protocol-version)
               (<= 1 protocol-version 4))
    (error 'parameter-error
           :parameter protocol-version
           :message "pgoutput protocol version must be an integer from 1 through 4"))
  (unless (member streamed-p '(nil t))
    (error 'parameter-error
           :parameter streamed-p
           :message "pgoutput streamed-p must be NIL or T"))
  (unless (member parallel-streaming-p '(nil t))
    (error 'parameter-error
           :parameter parallel-streaming-p
           :message "pgoutput parallel-streaming-p must be NIL or T"))
  (when streamed-p
    (when (< protocol-version 2)
      (error 'unsupported-feature
             :feature :pgoutput-streaming
             :message "pgoutput streamed messages require protocol version 2 or newer")))
  (when parallel-streaming-p
    (unless streamed-p
      (error 'parameter-error
             :parameter parallel-streaming-p
             :message "Parallel pgoutput streaming requires streamed-p"))
    (when (< protocol-version 4)
      (error 'unsupported-feature
             :feature :pgoutput-parallel-streaming
             :message "Parallel pgoutput streaming requires protocol version 4")))
  protocol-version)

(defun %logical-replication-require-version (protocol-version minimum kind)
  (when (< protocol-version minimum)
    (error 'unsupported-feature
           :feature (list :pgoutput kind)
           :message (format nil
                            "pgoutput ~A requires protocol version ~D"
                            kind minimum)))
  protocol-version)

(defun %logical-replication-expect-marker (payload position expected context)
  (let ((actual (%octet-at payload position)))
    (unless (= actual expected)
      (error 'protocol-error
             :message "Unexpected pgoutput message marker"
             :context context
             :expected expected
             :actual actual))
    (1+ position)))

(defun %logical-replication-read-stream-xid (payload position streamed-p)
  (if streamed-p
      (%read-u32 payload position)
      (values nil position)))

(defun %logical-replication-read-byte-data (payload position context)
  (multiple-value-bind (length next) (%read-i32 payload position)
    (when (minusp length)
      (error 'protocol-error
             :message "pgoutput byte data length must not be negative"
             :context context
             :expected ">= 0"
             :actual length))
    (let ((end (+ next length)))
      (when (> end (length payload))
        (error 'protocol-error
               :message "pgoutput byte data exceeds its payload"
               :context context
               :expected length
               :actual (- (length payload) next)))
      (values (subseq payload next end) end))))

(defun %logical-replication-read-tuple (payload position context)
  (multiple-value-bind (count position) (%read-i16 payload position)
    (%ensure-count-capacity payload position count 1 context)
    (let ((fields (make-array count)))
      (loop for index from 0 below count
            do (let ((marker (%octet-at payload position)))
                 (incf position)
                 (setf (aref fields index)
                       (case marker
                         (#x6e
                          (%make-logical-replication-field
                           :kind :null
                           :data +sql-null+))
                         (#x75
                          (%make-logical-replication-field
                           :kind :unchanged-toast))
                         (#x74
                          (multiple-value-bind (data next)
                              (%logical-replication-read-byte-data
                               payload position context)
                            (setf position next)
                            (%make-logical-replication-field
                             :kind :text
                             :data data)))
                         (#x62
                          (multiple-value-bind (data next)
                              (%logical-replication-read-byte-data
                               payload position context)
                            (setf position next)
                            (%make-logical-replication-field
                             :kind :binary
                             :data data)))
                         (otherwise
                          (error 'protocol-error
                                 :message "Unknown pgoutput TupleData field marker"
                                 :context context
                                 :expected '(#x6e #x75 #x74 #x62)
                                 :actual marker))))))
      (values (%make-logical-replication-tuple :fields fields) position))))

(defun %logical-replication-read-column (payload position context)
  (declare (ignore context))
  (let ((flags (%octet-at payload position)))
    (incf position)
    (multiple-value-bind (name position) (%read-cstring payload position)
      (multiple-value-bind (type-oid position) (%read-u32 payload position)
        (multiple-value-bind (type-modifier position)
            (%read-i32 payload position)
          (values (%make-logical-replication-column
                   :flags flags
                   :name name
                   :type-oid type-oid
                   :type-modifier type-modifier)
                  position))))))

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
      (#x42
       (%replication-require-length payload 21 :pgoutput-begin)
       (%ensure-payload-end payload 21 :pgoutput-begin)
       (multiple-value-bind (final-lsn position) (%read-u64 payload 1)
         (multiple-value-bind (commit-time position) (%read-i64 payload position)
           (multiple-value-bind (xid position) (%read-u32 payload position)
             (declare (ignore position))
             (%make-logical-replication-message
              :kind :begin
              :protocol-version protocol-version
              :xid xid
              :final-lsn final-lsn
              :commit-time commit-time)))))
      (#x4d
       (let ((position 1)
             (xid nil))
         (multiple-value-setq (xid position)
           (%logical-replication-read-stream-xid payload position streamed-p))
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
                 (%make-logical-replication-message
                  :kind :message
                  :protocol-version protocol-version
                  :xid xid
                  :flags flags
                  :message-lsn message-lsn
                  :prefix prefix
                  :content content)))))))
      (#x43
       (%replication-require-length payload 26 :pgoutput-commit)
       (%ensure-payload-end payload 26 :pgoutput-commit)
       (let ((flags (%octet-at payload 1)))
         (multiple-value-bind (commit-lsn position) (%read-u64 payload 2)
           (multiple-value-bind (end-lsn position) (%read-u64 payload position)
             (multiple-value-bind (commit-time position) (%read-i64 payload position)
               (declare (ignore position))
               (%make-logical-replication-message
                :kind :commit
                :protocol-version protocol-version
                :flags flags
                :commit-lsn commit-lsn
                :end-lsn end-lsn
                :commit-time commit-time))))))
      (#x4f
       (%replication-require-length payload 10 :pgoutput-origin)
       (multiple-value-bind (origin-lsn position) (%read-u64 payload 1)
         (multiple-value-bind (origin-name position)
             (%read-cstring payload position)
           (%ensure-payload-end payload position :pgoutput-origin)
           (%make-logical-replication-message
            :kind :origin
            :protocol-version protocol-version
            :origin-lsn origin-lsn
            :origin-name origin-name))))
      (#x52
       (let ((position 1))
         (multiple-value-bind (xid next)
             (%logical-replication-read-stream-xid payload position streamed-p)
           (setf position next)
           (multiple-value-bind (relation-id next) (%read-u32 payload position)
             (setf position next)
             (multiple-value-bind (namespace next) (%read-cstring payload position)
               (setf position next)
               (multiple-value-bind (relation-name next)
                   (%read-cstring payload position)
                 (setf position next)
                 (let ((replica-identity (%octet-at payload position)))
                   (incf position)
                   (multiple-value-bind (count next) (%read-i16 payload position)
                     (setf position next)
                     (%ensure-count-capacity payload position count 10
                                             :pgoutput-relation)
                     (let ((columns (make-array count)))
                       (loop for index from 0 below count
                             do (multiple-value-bind (column next)
                                    (%logical-replication-read-column
                                     payload position :pgoutput-relation)
                                  (setf position next
                                        (aref columns index) column)))
                       (%ensure-payload-end payload position :pgoutput-relation)
                       (%make-logical-replication-message
                        :kind :relation
                        :protocol-version protocol-version
                        :xid xid
                        :relation-id relation-id
                        :namespace namespace
                        :relation-name relation-name
                        :replica-identity replica-identity
                        :columns columns))))))))))
      (#x59
       (let ((position 1))
         (multiple-value-bind (xid next)
             (%logical-replication-read-stream-xid payload position streamed-p)
           (setf position next)
           (multiple-value-bind (type-oid next) (%read-u32 payload position)
             (setf position next)
             (multiple-value-bind (type-namespace next)
                 (%read-cstring payload position)
               (setf position next)
               (multiple-value-bind (type-name next)
                   (%read-cstring payload position)
                 (setf position next)
                 (%ensure-payload-end payload position :pgoutput-type)
                 (%make-logical-replication-message
                  :kind :type
                  :protocol-version protocol-version
                  :xid xid
                  :type-oid type-oid
                  :type-namespace type-namespace
                  :type-name type-name)))))))
      (#x49
       (let ((position 1))
         (multiple-value-bind (xid next)
             (%logical-replication-read-stream-xid payload position streamed-p)
           (setf position next)
           (multiple-value-bind (relation-id next) (%read-u32 payload position)
             (setf position next)
             (setf position
                   (%logical-replication-expect-marker
                    payload position #x4e :pgoutput-insert))
             (multiple-value-bind (new-tuple next)
                 (%logical-replication-read-tuple
                  payload position :pgoutput-insert)
               (%ensure-payload-end payload next :pgoutput-insert)
               (%make-logical-replication-message
                :kind :insert
                :protocol-version protocol-version
                :xid xid
                :relation-id relation-id
                :new-tuple new-tuple))))))
      (#x55
       (let ((position 1)
             (old-tuple nil)
             (old-tuple-kind nil))
         (multiple-value-bind (xid next)
             (%logical-replication-read-stream-xid payload position streamed-p)
           (setf position next)
           (multiple-value-bind (relation-id next) (%read-u32 payload position)
             (setf position next)
             (let ((marker (%octet-at payload position)))
               (when (member marker '(#x4b #x4f))
                 (setf old-tuple-kind (if (= marker #x4b) :key :old)
                       position (1+ position))
                 (multiple-value-setq (old-tuple position)
                   (%logical-replication-read-tuple
                    payload position :pgoutput-update)))
               (setf position
                     (%logical-replication-expect-marker
                      payload position #x4e :pgoutput-update))
               (multiple-value-bind (new-tuple next)
                   (%logical-replication-read-tuple
                    payload position :pgoutput-update)
                 (%ensure-payload-end payload next :pgoutput-update)
                 (%make-logical-replication-message
                  :kind :update
                  :protocol-version protocol-version
                  :xid xid
                  :relation-id relation-id
                  :old-tuple old-tuple
                  :old-tuple-kind old-tuple-kind
                  :new-tuple new-tuple)))))))
      (#x44
       (let ((position 1))
         (multiple-value-bind (xid next)
             (%logical-replication-read-stream-xid payload position streamed-p)
           (setf position next)
           (multiple-value-bind (relation-id next) (%read-u32 payload position)
             (setf position next)
             (let ((marker (%octet-at payload position)))
               (unless (member marker '(#x4b #x4f))
                 (error 'protocol-error
                        :message "pgoutput Delete requires a key or old tuple"
                        :context :pgoutput-delete
                        :expected '(#x4b #x4f)
                        :actual marker))
               (let ((old-tuple-kind (if (= marker #x4b) :key :old)))
                 (multiple-value-bind (old-tuple next)
                     (%logical-replication-read-tuple
                      payload (1+ position) :pgoutput-delete)
                   (%ensure-payload-end payload next :pgoutput-delete)
                   (%make-logical-replication-message
                    :kind :delete
                    :protocol-version protocol-version
                    :xid xid
                    :relation-id relation-id
                    :old-tuple old-tuple
                    :old-tuple-kind old-tuple-kind))))))))
      (#x54
       (let ((position 1))
         (multiple-value-bind (xid next)
             (%logical-replication-read-stream-xid payload position streamed-p)
           (setf position next)
           (multiple-value-bind (count next) (%read-i32 payload position)
             (setf position next)
             (let ((options (%octet-at payload position)))
               (incf position)
               (%ensure-count-capacity payload position count 4
                                       :pgoutput-truncate)
               (let ((relation-ids (make-array count)))
                 (loop for index from 0 below count
                       do (multiple-value-bind (relation-id next)
                              (%read-u32 payload position)
                            (setf position next
                                  (aref relation-ids index) relation-id)))
                 (%ensure-payload-end payload position :pgoutput-truncate)
                 (%make-logical-replication-message
                  :kind :truncate
                  :protocol-version protocol-version
                  :xid xid
                  :relation-ids relation-ids
                  :options options)))))))
      (#x53
       (%logical-replication-require-version protocol-version 2 :stream-start)
       (%replication-require-length payload 6 :pgoutput-stream-start)
       (%ensure-payload-end payload 6 :pgoutput-stream-start)
       (multiple-value-bind (xid position) (%read-u32 payload 1)
         (let ((first-segment (%octet-at payload position)))
           (%make-logical-replication-message
            :kind :stream-start
            :protocol-version protocol-version
            :xid xid
            :first-segment (= first-segment 1)))))
      (#x45
       (%logical-replication-require-version protocol-version 2 :stream-stop)
       (%ensure-payload-end payload 1 :pgoutput-stream-stop)
       (%make-logical-replication-message
        :kind :stream-stop
        :protocol-version protocol-version))
      (#x63
       (%logical-replication-require-version protocol-version 2 :stream-commit)
       (%replication-require-length payload 30 :pgoutput-stream-commit)
       (%ensure-payload-end payload 30 :pgoutput-stream-commit)
       (multiple-value-bind (xid position) (%read-u32 payload 1)
         (let ((flags (%octet-at payload position)))
           (incf position)
           (multiple-value-bind (commit-lsn position) (%read-u64 payload position)
             (multiple-value-bind (end-lsn position) (%read-u64 payload position)
               (multiple-value-bind (commit-time position)
                   (%read-i64 payload position)
                 (declare (ignore position))
                 (%make-logical-replication-message
                  :kind :stream-commit
                  :protocol-version protocol-version
                  :xid xid
                  :flags flags
                  :commit-lsn commit-lsn
                  :end-lsn end-lsn
                  :commit-time commit-time)))))))
      (#x41
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
               (multiple-value-bind (abort-lsn position)
                   (%read-u64 payload position)
                 (multiple-value-bind (abort-time position)
                     (%read-i64 payload position)
                   (declare (ignore position))
                   (%make-logical-replication-message
                    :kind :stream-abort
                    :protocol-version protocol-version
                    :xid xid
                    :subtransaction-id subtransaction-id
                    :abort-lsn abort-lsn
                    :abort-time abort-time)))
               (%make-logical-replication-message
                :kind :stream-abort
                :protocol-version protocol-version
                :xid xid
                :subtransaction-id subtransaction-id)))))
      (#x62
       (%logical-replication-require-version protocol-version 3 :begin-prepare)
       (%replication-require-length payload 29 :pgoutput-begin-prepare)
       (multiple-value-bind (prepare-lsn position) (%read-u64 payload 1)
         (multiple-value-bind (prepare-end-lsn position)
             (%read-u64 payload position)
           (multiple-value-bind (prepare-time position) (%read-i64 payload position)
             (multiple-value-bind (xid position) (%read-u32 payload position)
               (multiple-value-bind (gid position) (%read-cstring payload position)
                 (%ensure-payload-end payload position :pgoutput-begin-prepare)
                 (%make-logical-replication-message
                  :kind :begin-prepare
                  :protocol-version protocol-version
                  :xid xid
                  :prepare-lsn prepare-lsn
                  :prepare-end-lsn prepare-end-lsn
                  :prepare-time prepare-time
                  :gid gid)))))))
      (#x50
       (%logical-replication-require-version protocol-version 3 :prepare)
       (%replication-require-length payload 30 :pgoutput-prepare)
       (let ((flags (%octet-at payload 1)))
         (multiple-value-bind (prepare-lsn position) (%read-u64 payload 2)
           (multiple-value-bind (prepare-end-lsn position)
               (%read-u64 payload position)
             (multiple-value-bind (prepare-time position)
                 (%read-i64 payload position)
               (multiple-value-bind (xid position) (%read-u32 payload position)
                 (multiple-value-bind (gid position)
                     (%read-cstring payload position)
                   (%ensure-payload-end payload position :pgoutput-prepare)
                   (%make-logical-replication-message
                    :kind :prepare
                    :protocol-version protocol-version
                    :flags flags
                    :xid xid
                    :prepare-lsn prepare-lsn
                    :prepare-end-lsn prepare-end-lsn
                    :prepare-time prepare-time
                    :gid gid))))))))
      (#x4b
       (%logical-replication-require-version protocol-version 3 :commit-prepared)
       (%replication-require-length payload 30 :pgoutput-commit-prepared)
       (let ((flags (%octet-at payload 1)))
         (multiple-value-bind (commit-lsn position) (%read-u64 payload 2)
           (multiple-value-bind (end-lsn position) (%read-u64 payload position)
             (multiple-value-bind (commit-time position) (%read-i64 payload position)
               (multiple-value-bind (xid position) (%read-u32 payload position)
                 (multiple-value-bind (gid position) (%read-cstring payload position)
                   (%ensure-payload-end payload position :pgoutput-commit-prepared)
                   (%make-logical-replication-message
                    :kind :commit-prepared
                    :protocol-version protocol-version
                    :flags flags
                    :xid xid
                    :commit-lsn commit-lsn
                    :end-lsn end-lsn
                    :commit-time commit-time
                    :gid gid))))))))
      (#x72
       (%logical-replication-require-version protocol-version 3 :rollback-prepared)
       (%replication-require-length payload 38 :pgoutput-rollback-prepared)
       (let ((flags (%octet-at payload 1)))
         (multiple-value-bind (prepare-end-lsn position) (%read-u64 payload 2)
           (multiple-value-bind (rollback-end-lsn position)
               (%read-u64 payload position)
             (multiple-value-bind (prepare-time position) (%read-i64 payload position)
               (multiple-value-bind (rollback-time position)
                   (%read-i64 payload position)
                 (multiple-value-bind (xid position) (%read-u32 payload position)
                   (multiple-value-bind (gid position)
                       (%read-cstring payload position)
                     (%ensure-payload-end payload position :pgoutput-rollback-prepared)
                     (%make-logical-replication-message
                      :kind :rollback-prepared
                      :protocol-version protocol-version
                      :flags flags
                      :xid xid
                      :prepare-end-lsn prepare-end-lsn
                      :rollback-end-lsn rollback-end-lsn
                      :prepare-time prepare-time
                      :rollback-time rollback-time
                      :gid gid)))))))))
      (#x70
       (%logical-replication-require-version protocol-version 3 :stream-prepare)
       (%replication-require-length payload 30 :pgoutput-stream-prepare)
       (let ((flags (%octet-at payload 1)))
         (multiple-value-bind (prepare-lsn position) (%read-u64 payload 2)
           (multiple-value-bind (prepare-end-lsn position)
               (%read-u64 payload position)
             (multiple-value-bind (prepare-time position)
                 (%read-i64 payload position)
               (multiple-value-bind (xid position) (%read-u32 payload position)
                 (multiple-value-bind (gid position) (%read-cstring payload position)
                   (%ensure-payload-end payload position :pgoutput-stream-prepare)
                   (%make-logical-replication-message
                    :kind :stream-prepare
                    :protocol-version protocol-version
                    :flags flags
                    :xid xid
                    :prepare-lsn prepare-lsn
                    :prepare-end-lsn prepare-end-lsn
                    :prepare-time prepare-time
                    :gid gid))))))))
      (otherwise
       (error 'protocol-error
              :message "Unknown PostgreSQL pgoutput message type"
              :context :pgoutput
              :expected '(#x42 #x4d #x43 #x4f #x52 #x59 #x49 #x55 #x44
                          #x54 #x53 #x45 #x63 #x41 #x62 #x50 #x4b #x72 #x70)
              :actual kind)))))

(defun %postgresql-client-time (&optional (universal-time (get-universal-time)))
  (unless (integerp universal-time)
    (error 'parameter-error :parameter universal-time
           :message "PostgreSQL client time must be an integer universal time"))
  (* (- universal-time (encode-universal-time 0 0 0 1 1 2000 0))
     1000000))

(defun encode-standby-status-update (write-lsn flush-lsn apply-lsn
                                      &key client-time (reply-requested nil))
  "Encode a standby status update as a replication COPY DATA payload."
  (let ((builder (%protocol-builder 40)))
    (append-u8 builder (char-code #\r))
    (append-u64 builder write-lsn)
    (append-u64 builder flush-lsn)
    (append-u64 builder apply-lsn)
    (append-i64 builder (or client-time (%postgresql-client-time)))
    (append-u8 builder (if reply-requested 1 0))
    (builder-octets builder)))

(defun encode-hot-standby-feedback (xmin xmin-epoch catalog-xmin catalog-xmin-epoch
                                    &key client-time)
  "Encode hot standby feedback as a replication COPY DATA payload."
  (let ((builder (%protocol-builder 26)))
    (append-u8 builder (char-code #\h))
    (append-i64 builder (or client-time (%postgresql-client-time)))
    (append-u32 builder xmin)
    (append-u32 builder xmin-epoch)
    (append-u32 builder catalog-xmin)
    (append-u32 builder catalog-xmin-epoch)
    (builder-octets builder)))

(defun parse-function-call-response (payload)
  (multiple-value-bind (length position) (%read-i32 payload 0)
    (cond ((= length -1)
           (%ensure-payload-end payload position :function-call-response)
           +sql-null+)
          ((minusp length)
           (error 'protocol-error
                  :message "Invalid FunctionCallResponse result length."
                  :context :function-call-response
                  :expected "-1 or a non-negative length"
                  :actual length))
          (t
           (let ((end (+ position length)))
             (when (> end (length payload))
               (error 'protocol-error
                      :message "FunctionCallResponse result exceeds its payload."
                      :context :function-call-response
                      :expected length
                      :actual (- (length payload) position)))
             (%ensure-payload-end payload end :function-call-response)
             (subseq payload position end))))))

(defun parse-parameter-description (payload)
  (multiple-value-bind (count position) (%read-u16 payload 0)
    (%ensure-count-capacity payload position count 4 :parameter-description)
    (let ((oids (make-array count)))
      (loop for index from 0 below count
            do (multiple-value-bind (oid next) (%read-u32 payload position)
                 (setf position next (aref oids index) oid)))
      (%ensure-payload-end payload position :parameter-description)
      oids)))
