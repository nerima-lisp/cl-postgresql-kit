(in-package #:cl-postgresql-kit)

(defun %replication-quote-identifier (value)
  (check-type value string)
  (when (or (zerop (length value))
            (find #\Null value))
    (error 'parameter-error :parameter value
           :message "A replication identifier must be non-empty and must not contain NUL."))
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop for character across value
          do (when (char= character #\")
               (write-char #\" stream))
             (write-char character stream))
    (write-char #\" stream)))

(defun %replication-quote-literal (value)
  (check-type value string)
  (when (find #\Null value)
    (error 'parameter-error :parameter value
           :message "A replication SQL literal must not contain NUL."))
  (with-output-to-string (stream)
    (write-string "E'" stream)
    (loop for character across value
          do (case character
               (#\\
                (write-char #\\ stream)
                (write-char #\\ stream))
               (#\'
                (write-char #\\ stream)
                (write-char #\' stream))
               (#\Newline (write-string "\\n" stream))
               (#\Return (write-string "\\r" stream))
               (#\Tab (write-string "\\t" stream))
               (otherwise (write-char character stream))))
    (write-char #\' stream)))

(defun %replication-option-name (name)
  (let ((text (etypecase name
                (string name)
                (symbol (symbol-name name)))))
    (setf text (substitute #\_ #\- (string-upcase text)))
    (unless (and (plusp (length text))
                 (or (alpha-char-p (char text 0))
                     (char= (char text 0) #\_))
                 (loop for index from 1 below (length text)
                       always (or (alphanumericp (char text index))
                                  (char= (char text index) #\_))))
      (error 'parameter-error :parameter name
             :message "A replication option name must contain only letters, digits, underscores, and hyphens."))
    text))

(defun %replication-option-pair (option)
  (unless (consp option)
    (error 'parameter-error :parameter option
           :message "A replication option must be a cons or a one/two-element list."))
  (let ((tail (cdr option)))
    (cond ((null tail)
           (values (car option) nil))
          ((consp tail)
           (if (null (cddr option))
               (values (car option) (cadr option))
               (error 'parameter-error :parameter option
                      :message "A replication option list must contain at most two elements.")))
          (t
           (values (car option) tail)))))

(defun %replication-option-value-sql (value)
  (cond ((stringp value) (%replication-quote-literal value))
        ((integerp value) (format nil "~D" value))
        ((eq value t) "TRUE")
        ((null value) nil)
        (t (error 'parameter-error :parameter value
                  :message "Replication option values must be strings, integers, T, or NIL."))))

(defun %replication-option-sql (option)
  (multiple-value-bind (name value)
      (%replication-option-pair option)
    (let ((option-name (%replication-option-name name))
          (option-value (%replication-option-value-sql value)))
      (if option-value
          (format nil "~A ~A" option-name option-value)
          option-name))))

(defun %replication-option-sql-list (options)
  (unless (loop for tail = options then (cdr tail)
                while (consp tail)
                finally (return (null tail)))
    (error 'parameter-error :parameter options
           :message "Replication options must be a proper list."))
  (mapcar #'%replication-option-sql options))

(defun %replication-options-sql (options)
  (let ((option-sql (%replication-option-sql-list options)))
    (and option-sql
         (format nil "(~{~A~^, ~})" option-sql))))

(defun %replication-boolean-option-sql (name value)
  (unless (or (eq value t) (null value))
    (error 'parameter-error :parameter value
           :message "A replication slot boolean option must be T or NIL."))
  (format nil "~A ~:[FALSE~;TRUE~]"
          (%replication-option-name name)
          value))

(defun %replication-alter-slot-options-sql
    (two-phase-p two-phase-supplied-p failover-p failover-supplied-p options)
  (let ((option-sql nil))
    (when two-phase-supplied-p
      (push (%replication-boolean-option-sql "TWO_PHASE" two-phase-p)
            option-sql))
    (when failover-supplied-p
      (push (%replication-boolean-option-sql "FAILOVER" failover-p)
            option-sql))
    (dolist (option (%replication-option-sql-list options))
      (push option option-sql))
    (unless option-sql
      (error 'parameter-error :parameter options
             :message "ALTER_REPLICATION_SLOT requires at least one option."))
    (format nil "(~{~A~^, ~})" (nreverse option-sql))))

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

(defun %replication-base-backup-enum (value parameter allowed)
  (let ((text (string-downcase
               (etypecase value
                 (string value)
                 (symbol (symbol-name value))))))
    (unless (member text allowed :test #'string=)
      (error 'parameter-error :parameter value
             :message (format nil "Invalid BASE_BACKUP ~A value." parameter)))
    text))

(defun %replication-base-backup-string (value parameter)
  (unless (stringp value)
    (error 'parameter-error :parameter value
           :message (format nil "BASE_BACKUP ~A must be a string." parameter)))
  value)

(defun %replication-base-backup-detail-sql (value parameter)
  (cond ((stringp value) (%replication-quote-literal value))
        ((integerp value) (format nil "~D" value))
        (t (error 'parameter-error :parameter value
                  :message (format nil "BASE_BACKUP ~A must be a string or integer."
                                   parameter)))))

(defun %replication-base-backup-value-sql (name value)
  (format nil "~A ~A"
          (%replication-option-name name)
          (%replication-quote-literal value)))

(defun %replication-base-backup-option-sql-list
    (label target target-detail progress-p progress-supplied-p checkpoint
     wal-p wal-supplied-p wait-p wait-supplied-p compression compression-detail
     max-rate tablespace-map-p tablespace-map-supplied-p verify-checksums-p
     verify-checksums-supplied-p manifest manifest-checksums incremental-p options)
  (let ((option-sql nil))
    (when label
      (push (%replication-base-backup-value-sql
             "LABEL" (%replication-base-backup-string label :label))
            option-sql))
    (when target
      (push (%replication-base-backup-value-sql
             "TARGET"
             (%replication-base-backup-enum
              target :target '("client" "server" "blackhole")))
            option-sql))
    (when target-detail
      (push (%replication-base-backup-value-sql
             "TARGET_DETAIL"
             (%replication-base-backup-string target-detail :target-detail))
            option-sql))
    (when progress-supplied-p
      (push (%replication-boolean-option-sql "PROGRESS" progress-p)
            option-sql))
    (when checkpoint
      (push (%replication-base-backup-value-sql
             "CHECKPOINT"
             (%replication-base-backup-enum
              checkpoint :checkpoint '("fast" "spread")))
            option-sql))
    (when wal-supplied-p
      (push (%replication-boolean-option-sql "WAL" wal-p) option-sql))
    (when wait-supplied-p
      (push (%replication-boolean-option-sql "WAIT" wait-p) option-sql))
    (when compression
      (push (%replication-base-backup-value-sql
             "COMPRESSION"
             (%replication-base-backup-enum
              compression :compression '("gzip" "lz4" "zstd")))
            option-sql))
    (when compression-detail
      (push (format nil "COMPRESSION_DETAIL ~A"
                    (%replication-base-backup-detail-sql
                     compression-detail :compression-detail))
            option-sql))
    (when max-rate
      (unless (and (integerp max-rate)
                   (or (zerop max-rate) (<= 32 max-rate 1048576)))
        (error 'parameter-error :parameter max-rate
               :message "BASE_BACKUP MAX_RATE must be zero or between 32 and 1048576 kB/s."))
      (push (format nil "MAX_RATE ~D" max-rate) option-sql))
    (when tablespace-map-supplied-p
      (push (%replication-boolean-option-sql
             "TABLESPACE_MAP" tablespace-map-p)
            option-sql))
    (when verify-checksums-supplied-p
      (push (%replication-boolean-option-sql
             "VERIFY_CHECKSUMS" verify-checksums-p)
            option-sql))
    (when manifest
      (push (%replication-base-backup-value-sql
             "MANIFEST"
             (%replication-base-backup-enum
              manifest :manifest '("yes" "no" "force-encode")))
            option-sql))
    (when manifest-checksums
      (push (%replication-base-backup-value-sql
             "MANIFEST_CHECKSUMS"
             (%replication-base-backup-enum
              manifest-checksums :manifest-checksums
              '("none" "crc32c" "sha224" "sha256" "sha384" "sha512")))
            option-sql))
    (unless (or (null incremental-p) (eq incremental-p t))
      (error 'parameter-error :parameter incremental-p
             :message "BASE_BACKUP INCREMENTAL must be T or NIL."))
    (when incremental-p
      (push "INCREMENTAL" option-sql))
    (dolist (option (%replication-option-sql-list options))
      (push option option-sql))
    (nreverse option-sql)))

(defun %replication-base-backup-sql
    (&key label target target-detail
          ((:progress-p progress-p) nil progress-key-supplied-p)
          ((:progress-supplied-p progress-supplied-override-p)
           nil progress-override-key-supplied-p)
          checkpoint
          ((:wal-p wal-p) nil wal-key-supplied-p)
          ((:wal-supplied-p wal-supplied-override-p)
           nil wal-override-key-supplied-p)
          ((:wait-p wait-p) nil wait-key-supplied-p)
          ((:wait-supplied-p wait-supplied-override-p)
           nil wait-override-key-supplied-p)
          compression compression-detail max-rate
          ((:tablespace-map-p tablespace-map-p) nil tablespace-map-key-supplied-p)
          ((:tablespace-map-supplied-p tablespace-map-supplied-override-p)
           nil tablespace-map-override-key-supplied-p)
          ((:verify-checksums-p verify-checksums-p)
           nil verify-checksums-key-supplied-p)
          ((:verify-checksums-supplied-p verify-checksums-supplied-override-p)
           nil verify-checksums-override-key-supplied-p)
          manifest manifest-checksums incremental-p options)
  (let ((option-sql
          (%replication-base-backup-option-sql-list
           label target target-detail progress-p
           (if progress-override-key-supplied-p
               progress-supplied-override-p
               progress-key-supplied-p)
           checkpoint wal-p
           (if wal-override-key-supplied-p
               wal-supplied-override-p
               wal-key-supplied-p)
           wait-p
           (if wait-override-key-supplied-p
               wait-supplied-override-p
               wait-key-supplied-p)
           compression compression-detail max-rate tablespace-map-p
           (if tablespace-map-override-key-supplied-p
               tablespace-map-supplied-override-p
               tablespace-map-key-supplied-p)
           verify-checksums-p
           (if verify-checksums-override-key-supplied-p
               verify-checksums-supplied-override-p
               verify-checksums-key-supplied-p)
           manifest manifest-checksums incremental-p options)))
    (with-output-to-string (stream)
      (write-string "BASE_BACKUP" stream)
      (when option-sql
        (format stream " (~{~A~^, ~})" option-sql)))))

(defclass %base-backup-operation (%copy-operation)
  ((start-results :initarg :start-results :reader base-backup-start-results)
   (final-results :initform nil :accessor base-backup-final-results)
   (phase :initform :copy :accessor %base-backup-phase)
   (result-columns :initform #() :accessor %base-backup-result-columns)
   (result-raw-rows :initform nil :accessor %base-backup-result-raw-rows)
   (result-command-tag :initform nil :accessor %base-backup-result-command-tag)
   (result-portal-suspended-p
    :initform nil :accessor %base-backup-result-portal-suspended-p)
   (result-notices :initform nil :accessor %base-backup-result-notices)
   (result-active-p :initform nil :accessor %base-backup-result-active-p)
   (results :initform nil :accessor %base-backup-results)))

(defun %make-base-backup-query-result
    (connection columns raw-rows command-tag portal-suspended-p notices)
  (let ((decoded-rows
          (coerce
           (loop for raw-row in (nreverse raw-rows)
                 collect (%decode-query-row connection columns raw-row))
           'vector)))
    (%make-query-result
     :columns columns
     :rows decoded-rows
     :command-tag command-tag
     :row-count (length decoded-rows)
     :transaction-status (connection-transaction-status connection)
     :notices (%query-notice-list notices)
     :portal-suspended-p portal-suspended-p)))

(defun %base-backup-finish-current-result (operation)
  (when (%base-backup-result-active-p operation)
    (let* ((connection (%copy-connection operation))
           (result
             (%make-base-backup-query-result
              connection
              (%base-backup-result-columns operation)
              (%base-backup-result-raw-rows operation)
              (%base-backup-result-command-tag operation)
              (%base-backup-result-portal-suspended-p operation)
              (%base-backup-result-notices operation))))
      (push result (%base-backup-results operation))
      (setf (connection-last-result connection) result
            (%base-backup-result-columns operation) #()
            (%base-backup-result-raw-rows operation) nil
            (%base-backup-result-command-tag operation) nil
            (%base-backup-result-portal-suspended-p operation) nil
            (%base-backup-result-notices operation) nil
            (%base-backup-result-active-p operation) nil)
      result)))

(defun replication-base-backup-start
    (connection &key label target target-detail
                   (progress-p nil progress-supplied-p) checkpoint
                   (wal-p nil wal-supplied-p) (wait-p nil wait-supplied-p)
                   compression compression-detail max-rate
                   (tablespace-map-p nil tablespace-map-supplied-p)
                   (verify-checksums-p nil verify-checksums-supplied-p)
                   manifest manifest-checksums incremental-p options)
  "Start BASE_BACKUP and return a stream handle.

BASE-BACKUP-START-RESULTS contains the ordinary result sets that precede the
COPY stream (notably the two result sets required by INCREMENTAL).  Call
BASE-BACKUP-READ until it returns NIL, then inspect BASE-BACKUP-FINAL-RESULTS
for the ordinary result sets that follow the backup stream."
  (check-type connection connection)
  (let ((sql
          (%replication-base-backup-sql
           :label label :target target :target-detail target-detail
           :progress-p progress-p :progress-supplied-p progress-supplied-p
           :checkpoint checkpoint :wal-p wal-p :wal-supplied-p wal-supplied-p
           :wait-p wait-p :wait-supplied-p wait-supplied-p
           :compression compression :compression-detail compression-detail
           :max-rate max-rate :tablespace-map-p tablespace-map-p
           :tablespace-map-supplied-p tablespace-map-supplied-p
           :verify-checksums-p verify-checksums-p
           :verify-checksums-supplied-p verify-checksums-supplied-p
           :manifest manifest :manifest-checksums manifest-checksums
           :incremental-p incremental-p :options options)))
    (%with-exchange-failure-retirement (connection)
      (cl-concurrent-kit:with-lock-held ((connection--lock connection))
        (%require-open-connection connection)
        (%require-no-active-copy connection)
        (%require-no-active-cursors connection)
        (setf (connection--pending-error connection) nil)
        (%send-frontend-message connection (encode-query-message sql))
        (let ((columns #())
              (raw-rows nil)
              (command-tag nil)
              (portal-suspended-p nil)
              (notices nil)
              (results nil)
              (current-result-p nil))
          (labels ((finish-current-result ()
                     (when current-result-p
                       (push
                        (%make-base-backup-query-result
                         connection columns raw-rows command-tag
                         portal-suspended-p notices)
                        results)
                       (setf columns #()
                             raw-rows nil
                             command-tag nil
                             portal-suspended-p nil
                             notices nil
                             current-result-p nil))))
            (loop for message = (%read-backend-message connection)
                  do (multiple-value-bind (kind value)
                         (%process-backend-message connection message)
                       (case kind
                         (:row-description
                          (setf columns
                                (parse-row-description
                                 (backend-message-payload message))
                                current-result-p t))
                         (:data-row
                          (push (parse-data-row
                                 (backend-message-payload message))
                                raw-rows)
                          (setf current-result-p t))
                         (:command-complete
                          (setf command-tag
                                (parse-command-complete
                                 (backend-message-payload message))
                                current-result-p t)
                          (finish-current-result))
                         (:empty-query-response
                          (setf current-result-p t)
                          (finish-current-result))
                         (:portal-suspended
                          (setf portal-suspended-p t
                                current-result-p t))
                         (:notice-response (push value notices))
                         (:error-response nil)
                         (:copy-out-response
                          (finish-current-result)
                          (let ((operation
                                  (make-instance
                                   '%base-backup-operation
                                   :connection connection
                                   :kind :copy-out-response
                                   :response
                                   (parse-copy-response
                                    (backend-message-payload message))
                                   :start-results (nreverse results))))
                            (setf (connection--active-copy connection) operation)
                            (return operation)))
                         ((:copy-in-response :copy-both-response)
                          (%copy-protocol-error
                           "BASE_BACKUP must start with COPY TO STDOUT."))
                         (:ready-for-query
                          (let ((condition (%copy-pending-error connection)))
                            (if condition
                                (error condition)
                                (%copy-protocol-error
                                 "The PostgreSQL server did not start BASE_BACKUP."))))
                         (otherwise nil))))))))))

(defun base-backup-read (operation)
  "Read the next BASE_BACKUP stream event, or NIL after final results.

The returned event is a BASE-BACKUP-MESSAGE.  Final ordinary result sets are
stored in BASE-BACKUP-FINAL-RESULTS after NIL is returned."
  (check-type operation %base-backup-operation)
  (let ((connection (%copy-connection operation)))
    (cl-concurrent-kit:with-lock-held ((connection--lock connection))
      (%copy-check-operation operation :copy-out-response)
      (let ((returned-event-p nil)
            (completed-p nil))
        (unwind-protect
             (loop for message = (%read-backend-message connection)
                   do (multiple-value-bind (kind value)
                          (%process-backend-message connection message)
                        (case kind
                          (:copy-data
                           (setf returned-event-p t)
                           (return
                             (parse-base-backup-message
                              (backend-message-payload message))))
                          (:copy-done
                           (setf (%base-backup-phase operation) :between-copy))
                          (:copy-out-response
                           (parse-copy-response
                            (backend-message-payload message))
                           (setf (%base-backup-phase operation) :copy))
                          (:row-description
                           (setf (%base-backup-result-columns operation)
                                 (parse-row-description
                                  (backend-message-payload message))
                                 (%base-backup-result-active-p operation) t))
                          (:data-row
                           (push (parse-data-row
                                  (backend-message-payload message))
                                 (%base-backup-result-raw-rows operation))
                           (setf (%base-backup-result-active-p operation) t))
                          (:command-complete
                           (setf (%base-backup-result-command-tag operation)
                                 (parse-command-complete
                                  (backend-message-payload message))
                                 (%base-backup-result-active-p operation) t)
                           (%base-backup-finish-current-result operation))
                          (:empty-query-response
                           (setf (%base-backup-result-active-p operation) t)
                           (%base-backup-finish-current-result operation))
                          (:portal-suspended
                           (setf (%base-backup-result-portal-suspended-p operation) t
                                 (%base-backup-result-active-p operation) t))
                          (:notice-response
                           (push value (%base-backup-result-notices operation)))
                          (:error-response nil)
                          (:copy-fail
                           (%copy-protocol-error
                            (%copy-failure-message
                             (backend-message-payload message))))
                          (:ready-for-query
                           (unless (eq (%base-backup-phase operation) :between-copy)
                             (%copy-protocol-error
                              "The PostgreSQL server ended BASE_BACKUP without CopyDone."))
                           (%base-backup-finish-current-result operation)
                           (setf (base-backup-final-results operation)
                                 (nreverse (%base-backup-results operation))
                                 (%copy-state operation) :finished
                                 (connection--active-copy connection) nil
                                 completed-p t)
                           (let ((condition (%copy-pending-error connection)))
                             (when condition
                               (error condition)))
                           (return nil))
                          ((:copy-in-response :copy-both-response)
                           (%copy-protocol-error
                            "The PostgreSQL server returned an invalid BASE_BACKUP COPY direction."))
                          (otherwise nil))))
          (unless (or returned-event-p completed-p)
            (%copy-abort operation)))))))

(defun base-backup-finish (operation)
  "Drain a BASE_BACKUP operation and return its final result sets."
  (loop while (base-backup-read operation)
        finally (return (base-backup-final-results operation))))

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

(defun replication-start-logical (connection slot-name lsn &key options)
  "Start a logical replication stream at LSN and return a COPY BOTH handle."
  (let ((options-sql (%replication-options-sql options)))
    (replication-start
     connection
     (format nil "START_REPLICATION SLOT ~A LOGICAL ~A~:[~; ~A~]"
             (%replication-quote-identifier slot-name)
             (format-replication-lsn lsn)
             options-sql
             options-sql))))

(defclass logical-replication-unchanged-toast ()
  ()
  (:documentation
   "Sentinel used when pgoutput leaves a TOAST value unchanged."))

(defparameter +logical-replication-unchanged-toast+
  (make-instance 'logical-replication-unchanged-toast)
  "The value returned for an unchanged TOAST field.")

(defun logical-replication-unchanged-toast-p (value)
  (typep value 'logical-replication-unchanged-toast))

(defstruct (logical-replication-decoder
             (:constructor %make-logical-replication-decoder
                 (&key type-registry relations lock)))
  "State used to decode pgoutput relation and row messages."
  (type-registry nil)
  (relations nil)
  (lock nil))

(defun make-logical-replication-decoder
    (&key (type-registry (default-type-registry)))
  "Create a stateful decoder backed by TYPE-REGISTRY.

The decoder maintains relation metadata received from pgoutput Relation
messages.  Its relation cache is protected by a CL-CONCURRENT-KIT lock and
is safe to share between stream-reading threads."
  (check-type type-registry type-registry)
  (%make-logical-replication-decoder
   :type-registry type-registry
   :relations (make-hash-table :test #'eql)
   :lock (cl-concurrent-kit:make-lock
          :name "cl-postgresql-kit logical replication decoder")))

(defun %logical-replication-check-relation-id (relation-id)
  (unless (and (integerp relation-id)
               (<= 0 relation-id #xffffffff))
    (error 'parameter-error
           :parameter relation-id
           :message "Logical replication relation ID must be an unsigned 32-bit integer."))
  relation-id)

(defun register-logical-replication-relation (decoder relation)
  "Register a parsed Relation message in DECODER and return RELATION."
  (check-type decoder logical-replication-decoder)
  (check-type relation logical-replication-message)
  (unless (eq :relation (logical-replication-message-kind relation))
    (error 'parameter-error
           :parameter relation
           :message "Only a logical replication Relation message can be registered."))
  (let ((relation-id
          (%logical-replication-check-relation-id
           (logical-replication-message-relation-id relation))))
    (cl-concurrent-kit:with-lock-held
        ((logical-replication-decoder-lock decoder))
      (setf (gethash relation-id
                     (logical-replication-decoder-relations decoder))
            relation))
    relation))

(defun find-logical-replication-relation (decoder relation-id)
  "Return the cached relation metadata for RELATION-ID, or NIL."
  (check-type decoder logical-replication-decoder)
  (let ((relation-id (%logical-replication-check-relation-id relation-id)))
    (cl-concurrent-kit:with-lock-held
        ((logical-replication-decoder-lock decoder))
      (gethash relation-id (logical-replication-decoder-relations decoder)))))

(defun forget-logical-replication-relation (decoder relation-id)
  "Remove RELATION-ID from DECODER and return true when it was present."
  (check-type decoder logical-replication-decoder)
  (let ((relation-id (%logical-replication-check-relation-id relation-id)))
    (cl-concurrent-kit:with-lock-held
        ((logical-replication-decoder-lock decoder))
      (remhash relation-id (logical-replication-decoder-relations decoder)))))

(defun clear-logical-replication-relations (decoder)
  "Remove all relation metadata from DECODER and return DECODER."
  (check-type decoder logical-replication-decoder)
  (cl-concurrent-kit:with-lock-held
      ((logical-replication-decoder-lock decoder))
    (clrhash (logical-replication-decoder-relations decoder)))
  decoder)

(defstruct (logical-replication-event
             (:constructor %make-logical-replication-event
                 (&key kind message relation new-values old-values
                       old-values-kind)))
  "A decoded logical replication event.

KIND is the pgoutput message kind.  NEW-VALUES and OLD-VALUES are vectors of
decoded values for row events; OLD-VALUES-KIND identifies whether OLD-VALUES
came from a :KEY or :OLD tuple."
  kind
  message
  relation
  new-values
  old-values
  old-values-kind)

(defun %logical-replication-relation-or-error (decoder relation-id)
  (or (find-logical-replication-relation decoder relation-id)
      (error 'protocol-error
             :context :logical-replication
             :expected :relation-message
             :actual relation-id
             :message
             "A logical replication row message referenced an unknown relation.")))

(defun %logical-replication-tuple-columns (relation tuple-kind)
  (let ((columns (logical-replication-message-columns relation)))
    (case tuple-kind
      (:new columns)
      (:key
       (coerce
        (loop for column across columns
              when (not (zerop (logand
                                (logical-replication-column-flags column)
                                #x01)))
                collect column)
        'vector))
      (:old
       (coerce
        (loop for column across columns
              when (not (zerop (logand
                                (logical-replication-column-flags column)
                                #x02)))
                collect column)
        'vector))
      (otherwise
       (error 'parameter-error
              :parameter tuple-kind
              :message "Logical replication tuple kind must be :NEW, :KEY, or :OLD.")))))

(defun %logical-replication-octet-vector-p (value)
  (typep value '(vector (unsigned-byte 8))))

(defun decode-logical-replication-tuple
    (decoder relation-id tuple &key (tuple-kind :new))
  "Decode TUPLE using cached RELATION-ID metadata.

TUPLE-KIND selects all relation columns (:NEW), replica identity key columns
(:KEY), or replica identity columns (:OLD).  Unknown PostgreSQL type OIDs
remain octet vectors, as they do in DECODE-VALUE."
  (check-type decoder logical-replication-decoder)
  (check-type tuple logical-replication-tuple)
  (let* ((relation (%logical-replication-relation-or-error decoder relation-id))
         (columns (%logical-replication-tuple-columns relation tuple-kind))
         (fields (logical-replication-tuple-fields tuple)))
    (unless (= (length columns) (length fields))
      (error 'protocol-error
             :context :logical-replication
             :expected (length columns)
             :actual (length fields)
             :message "Logical replication tuple field count does not match relation metadata."))
    (let ((values (make-array (length fields))))
      (loop for index below (length fields)
            for field = (aref fields index)
            for column = (aref columns index)
            do
               (check-type field logical-replication-field)
               (setf (aref values index)
                     (case (logical-replication-field-kind field)
                       (:null +sql-null+)
                       (:unchanged-toast
                        +logical-replication-unchanged-toast+)
                       ((:text :binary)
                        (let ((data (logical-replication-field-data field)))
                          (unless (%logical-replication-octet-vector-p data)
                            (error 'protocol-error
                                   :context :logical-replication
                                   :expected '(vector (unsigned-byte 8))
                                   :actual data
                                   :message
                                   "Logical replication field data must be an octet vector."))
                          (decode-value
                           (logical-replication-decoder-type-registry decoder)
                           (logical-replication-column-type-oid column)
                           data
                           :format (if (eq :text
                                            (logical-replication-field-kind field))
                                       0
                                       1))))
                       (otherwise
                        (error 'protocol-error
                               :context :logical-replication
                               :expected '(:null :unchanged-toast :text :binary)
                               :actual (logical-replication-field-kind field)
                               :message
                               "Unknown logical replication field kind.")))))
      values)))

(defun %logical-replication-required-tuple (tuple role)
  (or tuple
      (error 'protocol-error
             :context :logical-replication
             :expected role
             :actual nil
             :message "A logical replication row message is missing its tuple.")))

(defun decode-logical-replication-message (decoder message)
  "Decode a parsed pgoutput MESSAGE and return a logical replication event.

Relation messages update DECODER's metadata cache.  Insert, update, and
delete messages decode their row fields through the registered type codecs;
other message kinds are returned as metadata-only events."
  (check-type decoder logical-replication-decoder)
  (check-type message logical-replication-message)
  (case (logical-replication-message-kind message)
    (:relation
     (%make-logical-replication-event
      :kind :relation
      :message message
      :relation (register-logical-replication-relation decoder message)))
    (:insert
     (let* ((relation-id (logical-replication-message-relation-id message))
            (relation (%logical-replication-relation-or-error decoder relation-id))
            (tuple (%logical-replication-required-tuple
                    (logical-replication-message-new-tuple message)
                    :new)))
       (%make-logical-replication-event
        :kind :insert
        :message message
        :relation relation
        :new-values (decode-logical-replication-tuple
                     decoder relation-id tuple :tuple-kind :new))))
    (:update
     (let* ((relation-id (logical-replication-message-relation-id message))
            (relation (%logical-replication-relation-or-error decoder relation-id))
            (new-tuple (%logical-replication-required-tuple
                        (logical-replication-message-new-tuple message)
                        :new))
            (old-tuple (logical-replication-message-old-tuple message))
            (old-kind (logical-replication-message-old-tuple-kind message)))
       (%make-logical-replication-event
        :kind :update
        :message message
        :relation relation
        :new-values (decode-logical-replication-tuple
                     decoder relation-id new-tuple :tuple-kind :new)
        :old-values (when old-tuple
                      (decode-logical-replication-tuple
                       decoder relation-id old-tuple :tuple-kind old-kind))
        :old-values-kind old-kind)))
    (:delete
     (let* ((relation-id (logical-replication-message-relation-id message))
            (relation (%logical-replication-relation-or-error decoder relation-id))
            (old-kind (logical-replication-message-old-tuple-kind message))
            (old-tuple (%logical-replication-required-tuple
                        (logical-replication-message-old-tuple message)
                        old-kind)))
       (%make-logical-replication-event
        :kind :delete
        :message message
        :relation relation
        :old-values (decode-logical-replication-tuple
                     decoder relation-id old-tuple :tuple-kind old-kind)
        :old-values-kind old-kind)))
    (otherwise
     (%make-logical-replication-event
      :kind (logical-replication-message-kind message)
      :message message))))

(defstruct (logical-replication-stream-event
             (:constructor %make-logical-replication-stream-event
                 (&key kind replication-message logical-message logical-event)))
  "One decoded event from a physical PostgreSQL replication stream.

KIND is the physical replication envelope kind.  XLOG-DATA records also have
LOGICAL-MESSAGE and LOGICAL-EVENT slots populated; other envelope kinds such
as PRIMARY-KEEPALIVE leave those slots NIL."
  kind
  replication-message
  logical-message
  logical-event)

(defun decode-logical-replication-stream-message
    (decoder replication-message
     &key (protocol-version 1) streamed-p parallel-streaming-p)
  "Decode one physical replication envelope into a typed stream event.

For an XLogData envelope, parse its pgoutput payload using PROTOCOL-VERSION
and decode the resulting message through DECODER.  Keepalive and standby
feedback envelopes are returned as physical events without a logical payload.
The caller owns the COPY BOTH lifecycle; this function only decodes one
already-read envelope."
  (check-type decoder logical-replication-decoder)
  (check-type replication-message replication-message)
  (if (eq :xlog-data (replication-message-kind replication-message))
      (let* ((logical-message
               (parse-logical-replication-message
                (replication-message-data replication-message)
                :protocol-version protocol-version
                :streamed-p streamed-p
                :parallel-streaming-p parallel-streaming-p))
             (logical-event
               (decode-logical-replication-message decoder logical-message)))
        (%make-logical-replication-stream-event
         :kind :xlog-data
         :replication-message replication-message
         :logical-message logical-message
         :logical-event logical-event))
      (%make-logical-replication-stream-event
       :kind (replication-message-kind replication-message)
       :replication-message replication-message)))

(defun logical-replication-read
    (operation decoder
     &key (protocol-version 1) streamed-p parallel-streaming-p)
  "Read and decode the next event from a logical replication operation.

The operation must be a COPY BOTH handle returned by REPLICATION-START or
one of the logical replication start helpers.  Return NIL after the server's
CopyDone.  A malformed pgoutput message or a value-decoding error retires the
connection, matching REPLICATION-READ's error behavior."
  (check-type decoder logical-replication-decoder)
  (let ((replication-message (replication-read operation)))
    (when replication-message
      (handler-case
          (decode-logical-replication-stream-message
           decoder replication-message
           :protocol-version protocol-version
           :streamed-p streamed-p
           :parallel-streaming-p parallel-streaming-p)
        (error (condition)
          (%copy-abort operation)
          (error condition))))))
