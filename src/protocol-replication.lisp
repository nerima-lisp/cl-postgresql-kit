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

(defun %replication-require-exact-length (payload expected context)
  (%replication-require-length payload expected context)
  (%ensure-payload-end payload expected context))

(defmacro %with-replication-fields ((position &rest bindings) &body body)
  (labels ((expand-bindings (bindings)
             (if (endp bindings)
                 `(progn ,@body)
                 (destructuring-bind (name value-form) (car bindings)
                   (let ((next-position (gensym "POSITION")))
                     `(multiple-value-bind (,name ,next-position) ,value-form
                        (let ((,position ,next-position))
                          ,(expand-bindings (cdr bindings)))))))))
    (expand-bindings bindings)))

(defun %parse-xlog-data-message (payload)
  (%replication-require-length payload 25 :xlog-data)
  (let ((position 1))
    (%with-replication-fields
        (position
         (wal-start (%read-u64 payload position))
         (wal-end (%read-u64 payload position))
         (send-time (%read-i64 payload position)))
      (%make-replication-message
       :kind :xlog-data
       :wal-start wal-start
       :wal-end wal-end
       :send-time send-time
       :data (subseq payload position)))))

(defun %parse-primary-keepalive-message (payload)
  (%replication-require-exact-length payload 18 :primary-keepalive)
  (let ((position 1))
    (%with-replication-fields
        (position
         (wal-end (%read-u64 payload position))
         (send-time (%read-i64 payload position)))
      (%make-replication-message
       :kind :primary-keepalive
       :wal-end wal-end
       :send-time send-time
       :reply-requested
       (%replication-reply-requested
        (%octet-at payload position) :primary-keepalive)))))

(defun %parse-standby-status-update-message (payload)
  (%replication-require-exact-length payload 34 :standby-status-update)
  (let ((position 1))
    (%with-replication-fields
        (position
         (write-lsn (%read-u64 payload position))
         (flush-lsn (%read-u64 payload position))
         (apply-lsn (%read-u64 payload position))
         (client-time (%read-i64 payload position)))
      (%make-replication-message
       :kind :standby-status-update
       :write-lsn write-lsn
       :flush-lsn flush-lsn
       :apply-lsn apply-lsn
       :client-time client-time
       :reply-requested
       (%replication-reply-requested
        (%octet-at payload position) :standby-status-update)))))

(defun %parse-hot-standby-feedback-message (payload)
  (%replication-require-exact-length payload 25 :hot-standby-feedback)
  (let ((position 1))
    (%with-replication-fields
        (position
         (client-time (%read-i64 payload position))
         (xmin (%read-u32 payload position))
         (xmin-epoch (%read-u32 payload position))
         (catalog-xmin (%read-u32 payload position))
         (catalog-xmin-epoch (%read-u32 payload position)))
      (%make-replication-message
       :kind :hot-standby-feedback
       :client-time client-time
       :xmin xmin
       :xmin-epoch xmin-epoch
       :catalog-xmin catalog-xmin
       :catalog-xmin-epoch catalog-xmin-epoch))))

(defun %parse-base-backup-new-archive-message (payload)
  (let ((position 1))
    (%with-replication-fields
        (position
         (archive-name (%read-cstring payload position))
         (tablespace-path (%read-cstring payload position)))
      (%ensure-payload-end payload position :base-backup-new-archive)
      (%make-base-backup-message
       :kind :new-archive
       :archive-name archive-name
       :tablespace-path tablespace-path))))

(defun %parse-base-backup-progress-message (payload)
  (%replication-require-exact-length payload 9 :base-backup-progress)
  (%make-base-backup-message
   :kind :progress
   :bytes-completed (nth-value 0 (%read-u64 payload 1))))

(defun parse-replication-message (payload)
  "Parse one PostgreSQL streaming-replication COPY DATA payload.

The returned REPLICATION-MESSAGE represents XLogData, primary keepalive,
standby status update, or hot standby feedback.  WAL positions and XIDs are
returned as non-negative integers, while PostgreSQL timestamps are signed
microseconds since 2000-01-01 UTC."
  (%wire-check-octets payload)
  (let ((kind (%octet-at payload 0)))
    (case kind
      (#x77 (%parse-xlog-data-message payload))
      (#x6b (%parse-primary-keepalive-message payload))
      (#x72 (%parse-standby-status-update-message payload))
      (#x68 (%parse-hot-standby-feedback-message payload))
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
    (#x6e (%parse-base-backup-new-archive-message payload))
    (#x6d
     (%ensure-payload-end payload 1 :base-backup-manifest)
     (%make-base-backup-message :kind :manifest))
    (#x64
     (%make-base-backup-message
      :kind :data
      :data (subseq payload 1)))
    (#x70 (%parse-base-backup-progress-message payload))
    (otherwise
     (error 'protocol-error
            :message "Unknown PostgreSQL BASE_BACKUP CopyData marker"
            :context :base-backup
            :expected '(#x6e #x6d #x64 #x70)
            :actual (%octet-at payload 0)))))

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
