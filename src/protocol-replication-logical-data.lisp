(in-package #:cl-postgresql-kit)

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

(defmacro %with-logical-replication-stream-context
    ((payload position streamed-p xid) &body body)
  (let ((next-position (gensym "POSITION")))
    `(let ((,position 1))
       (multiple-value-bind (,xid ,next-position)
           (%logical-replication-read-stream-xid
            ,payload ,position ,streamed-p)
         (let ((,position ,next-position))
           ,@body)))))

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

(defmacro %logical-replication-message (kind protocol-version &rest initargs)
  `(%make-logical-replication-message
    :kind ,kind
    :protocol-version ,protocol-version
    ,@initargs))

(defmacro %with-logical-replication-prepared-message
    ((payload position context &rest fields) &body body)
  `(%with-replication-fields
       (,position
        ,@fields
        (xid (%read-u32 ,payload ,position))
        (gid (%read-cstring ,payload ,position)))
     (%ensure-payload-end ,payload ,position ,context)
     ,@body))
