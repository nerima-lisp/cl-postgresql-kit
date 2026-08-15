(in-package #:cl-postgresql-kit)

(defun %replication-base-backup-max-rate-option-sql (max-rate)
  (unless (and (integerp max-rate)
               (or (zerop max-rate) (<= 32 max-rate 1048576)))
    (error 'parameter-error :parameter max-rate
           :message "BASE_BACKUP MAX_RATE must be zero or between 32 and 1048576 kB/s."))
  (format nil "MAX_RATE ~D" max-rate))

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
    (%replication-collect-option-sql (option-sql)
      (:value "LABEL" label
       (lambda (value)
         (%replication-base-backup-string value :label)))
      (:value "TARGET" target
       (lambda (value)
         (%replication-base-backup-enum
          value :target '("client" "server" "blackhole"))))
      (:value "TARGET_DETAIL" target-detail
       (lambda (value)
         (%replication-base-backup-string value :target-detail)))
      (:boolean progress-supplied-p "PROGRESS" progress-p)
      (:value "CHECKPOINT" checkpoint
       (lambda (value)
         (%replication-base-backup-enum
          value :checkpoint '("fast" "spread"))))
      (:boolean wal-supplied-p "WAL" wal-p)
      (:boolean wait-supplied-p "WAIT" wait-p)
      (:value "COMPRESSION" compression
       (lambda (value)
         (%replication-base-backup-enum
          value :compression '("gzip" "lz4" "zstd"))))
      (:push compression-detail
       (format nil "COMPRESSION_DETAIL ~A"
               (%replication-base-backup-detail-sql
                compression-detail :compression-detail)))
      (:push max-rate
       (%replication-base-backup-max-rate-option-sql max-rate))
      (:boolean tablespace-map-supplied-p
       "TABLESPACE_MAP" tablespace-map-p)
      (:boolean verify-checksums-supplied-p
       "VERIFY_CHECKSUMS" verify-checksums-p)
      (:value "MANIFEST" manifest
       (lambda (value)
         (%replication-base-backup-enum
          value :manifest '("yes" "no" "force-encode"))))
      (:value "MANIFEST_CHECKSUMS" manifest-checksums
       (lambda (value)
         (%replication-base-backup-enum
          value :manifest-checksums
          '("none" "crc32c" "sha224" "sha256" "sha384" "sha512")))))
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
           (%replication-key-supplied-p
            progress-override-key-supplied-p
            progress-supplied-override-p
            progress-key-supplied-p)
           checkpoint wal-p
           (%replication-key-supplied-p
            wal-override-key-supplied-p
            wal-supplied-override-p
            wal-key-supplied-p)
           wait-p
           (%replication-key-supplied-p
            wait-override-key-supplied-p
            wait-supplied-override-p
            wait-key-supplied-p)
           compression compression-detail max-rate tablespace-map-p
           (%replication-key-supplied-p
            tablespace-map-override-key-supplied-p
            tablespace-map-supplied-override-p
            tablespace-map-key-supplied-p)
           verify-checksums-p
           (%replication-key-supplied-p
            verify-checksums-override-key-supplied-p
            verify-checksums-supplied-override-p
            verify-checksums-key-supplied-p)
           manifest manifest-checksums incremental-p options)))
    (with-output-to-string (stream)
      (write-string "BASE_BACKUP" stream)
      (when option-sql
        (format stream " (~{~A~^, ~})" option-sql)))))
