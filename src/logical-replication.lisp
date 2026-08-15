(in-package #:cl-postgresql-kit)

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
   :relations (make-hash-table)
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
