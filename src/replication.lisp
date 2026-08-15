(in-package #:cl-postgresql-kit)

(defun replication-start (connection start-replication-sql)
  "Start a physical or logical PostgreSQL replication stream.

START-REPLICATION-SQL is passed to PostgreSQL as-is and must be a valid
START_REPLICATION command.  The returned operation uses the COPY BOTH
lifecycle."
  (copy-both-start connection start-replication-sql))

(defun replication-read (operation)
  "Read and parse the next replication message, or NIL after CopyDone."
  (let ((payload (copy-both-read operation)))
    (and payload
         (handler-case
             (parse-replication-message payload)
           (error (condition)
             (%copy-abort operation)
             (error condition))))))

(defun replication-send-status (operation write-lsn flush-lsn apply-lsn
                                &key client-time (reply-requested nil))
  "Send a standby status update on an active replication stream."
  (copy-both-write
   operation
   (encode-standby-status-update
    write-lsn flush-lsn apply-lsn
    :client-time client-time
    :reply-requested reply-requested)))

(defun replication-send-hot-standby-feedback
    (operation xmin xmin-epoch catalog-xmin catalog-xmin-epoch
     &key client-time)
  "Send hot standby feedback on an active replication stream."
  (copy-both-write
   operation
   (encode-hot-standby-feedback
    xmin xmin-epoch catalog-xmin catalog-xmin-epoch
    :client-time client-time)))

(defun replication-finish (operation)
  "Finish an active replication stream and return its server command tag."
  (copy-both-finish operation))

(defun %replication-query-one-row (connection sql)
  (let ((result (query connection sql)))
    (unless (and (query-result-p result)
                 (= 1 (result-row-count result)))
      (error 'protocol-error
             :message "A replication command did not return exactly one row."
             :context :replication
             :expected 1
             :actual (and (query-result-p result)
                          (result-row-count result))))
    result))

(defun %replication-required-value (result column)
  (let ((value (row-value result 0 column)))
    (if (sql-null-p value)
        (error 'protocol-error
               :message "A required replication result column was NULL."
               :context :replication
               :expected column)
        value)))

(defun replication-identify-system (connection)
  "Return the server identity and current replication position.

The result is a property list with :SYSTEM-ID, :TIMELINE, :XLOG-POSITION,
and :DATABASE keys.  A NULL database is returned as NIL."
  (let ((result (%replication-query-one-row connection "IDENTIFY_SYSTEM")))
    (list :system-id (%replication-required-value result 0)
          :timeline (%replication-required-value result 1)
          :xlog-position
          (format-replication-lsn (%replication-required-value result 2))
          :database (let ((value (row-value result 0 3)))
                      (if (sql-null-p value) nil value)))))

(defun replication-create-slot (connection slot-name
                                &key (type :physical) temporary-p
                                  reserve-wal-p two-phase-p failover-p snapshot
                                  output-plugin options)
  "Create a physical or logical replication slot and return its query result.

For a logical slot, OUTPUT-PLUGIN is required.  OPTIONS is an alist-like
list whose entries are option names with string, integer, boolean, or bare
values; names may be strings or symbols."
  (let* ((physical-p (eq type :physical))
         (logical-p (eq type :logical))
         (quoted-slot (%replication-quote-identifier slot-name)))
    (unless (or physical-p logical-p)
      (error 'parameter-error :parameter type
             :message "Replication slot type must be :PHYSICAL or :LOGICAL."))
    (when (and physical-p
               (or two-phase-p failover-p snapshot output-plugin options))
      (error 'parameter-error :parameter type
             :message "Logical replication slot options cannot be used for a physical slot."))
    (when (and logical-p reserve-wal-p)
      (error 'parameter-error :parameter reserve-wal-p
             :message "RESERVE_WAL is only valid for a physical replication slot."))
    (when (and logical-p (null output-plugin))
      (error 'parameter-error :parameter output-plugin
             :message "A logical replication slot requires an output plugin."))
    (unless (loop for tail = options then (cdr tail)
                  while (consp tail)
                  finally (return (null tail)))
      (error 'parameter-error :parameter options
             :message "Replication options must be a proper list."))
    (let ((option-pairs nil))
      (when reserve-wal-p
        (push '("RESERVE_WAL") option-pairs))
      (when two-phase-p
        (push '("TWO_PHASE") option-pairs))
      (when failover-p
        (push '("FAILOVER") option-pairs))
      (when snapshot
        (unless (member snapshot '(:export :use :nothing))
          (error 'parameter-error :parameter snapshot
                 :message "Replication slot snapshot must be :EXPORT, :USE, or :NOTHING."))
        (push (list "SNAPSHOT" (string-downcase (symbol-name snapshot)))
              option-pairs))
      (setf option-pairs (append (nreverse option-pairs) options))
      (let ((sql
              (with-output-to-string (stream)
                (format stream "CREATE_REPLICATION_SLOT ~A" quoted-slot)
                (when temporary-p
                  (write-string " TEMPORARY" stream))
                (if physical-p
                    (write-string " PHYSICAL" stream)
                    (format stream " LOGICAL ~A"
                            (%replication-quote-identifier output-plugin)))
                (let ((options-sql (%replication-options-sql option-pairs)))
                  (when options-sql
                    (format stream " ~A" options-sql))))))
        (query connection sql)))))

(defun replication-read-slot (connection slot-name)
  "Read replication slot metadata and return the raw query result.

PostgreSQL returns no row when SLOT-NAME does not exist; callers can inspect
the returned result's row count and SQL NULL values."
  (query connection
         (format nil "READ_REPLICATION_SLOT ~A"
                 (%replication-quote-identifier slot-name))))

(defun replication-drop-slot (connection slot-name &key wait-p)
  "Drop a replication slot and return the raw query result."
  (query connection
         (format nil "DROP_REPLICATION_SLOT ~A~:[~; WAIT~]"
                 (%replication-quote-identifier slot-name)
                 wait-p)))

(defun replication-alter-slot (connection slot-name
                               &key (two-phase-p nil two-phase-supplied-p)
                                 (failover-p nil failover-supplied-p)
                                 options)
  "Alter a logical replication slot and return its raw query result.

TWO-PHASE-P and FAILOVER-P are tri-state keyword arguments: when omitted, the
corresponding option is not sent; when supplied, T selects the feature and NIL
clears it.  OPTIONS can carry additional PostgreSQL replication options."
  (let ((options-sql
          (%replication-alter-slot-options-sql
           two-phase-p two-phase-supplied-p
           failover-p failover-supplied-p
           options)))
    (query connection
           (format nil "ALTER_REPLICATION_SLOT ~A ~A"
                   (%replication-quote-identifier slot-name)
                   options-sql))))

(defun replication-show (connection name)
  "Return the result of SHOW NAME in replication mode."
  (query connection
         (format nil "SHOW ~A" (%replication-quote-identifier name))))

(defun %replication-timeline-number (timeline)
  (unless (and (integerp timeline) (<= 1 timeline #xffffffff))
    (error 'parameter-error :parameter timeline
           :message "A replication timeline must be an integer from 1 through 2^32-1."))
  timeline)

(defun replication-timeline-history (connection timeline)
  "Return the timeline history for TIMELINE as a query result."
  (query connection
         (format nil "TIMELINE_HISTORY ~D"
                 (%replication-timeline-number timeline))))

(defun replication-upload-manifest-start (connection)
  "Start an incremental-backup manifest upload and return a COPY handle."
  (copy-in-start connection "UPLOAD_MANIFEST"))

(defun replication-upload-manifest (connection manifest)
  "Upload MANIFEST and return the server command tag.

MANIFEST is accepted in any representation supported by COPY-IN-WRITE,
including a string or an octet vector."
  (let ((operation (replication-upload-manifest-start connection)))
    (copy-in-write operation manifest)
    (copy-in-finish operation)))

(defun %replication-timeline-sql (timeline)
  (when timeline
    (format nil " TIMELINE ~D" (%replication-timeline-number timeline))))

(defun replication-start-physical (connection slot-name lsn &key timeline)
  "Start a physical replication stream at LSN and return a COPY BOTH handle."
  (replication-start
   connection
   (format nil "START_REPLICATION SLOT ~A PHYSICAL ~A~A"
           (%replication-quote-identifier slot-name)
           (format-replication-lsn lsn)
           (or (%replication-timeline-sql timeline) ""))))
