(in-package #:cl-postgresql-kit)

(defun %parse-logical-replication-relation
    (payload protocol-version streamed-p)
  (%with-logical-replication-stream-context
      (payload position streamed-p xid)
    (%with-replication-fields
        (position
         (relation-id (%read-u32 payload position))
         (namespace (%read-cstring payload position))
         (relation-name (%read-cstring payload position)))
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
            (%logical-replication-message
             :relation protocol-version
             :xid xid
             :relation-id relation-id
             :namespace namespace
             :relation-name relation-name
             :replica-identity replica-identity
             :columns columns)))))))

(defun %parse-logical-replication-type (payload protocol-version streamed-p)
  (%with-logical-replication-stream-context
      (payload position streamed-p xid)
    (%with-replication-fields
        (position
         (type-oid (%read-u32 payload position))
         (type-namespace (%read-cstring payload position))
         (type-name (%read-cstring payload position)))
      (%ensure-payload-end payload position :pgoutput-type)
      (%logical-replication-message
       :type protocol-version
       :xid xid
       :type-oid type-oid
       :type-namespace type-namespace
       :type-name type-name))))

(defun %parse-logical-replication-insert
    (payload protocol-version streamed-p)
  (%with-logical-replication-stream-context
      (payload position streamed-p xid)
    (%with-replication-fields
        (position
         (relation-id (%read-u32 payload position)))
      (setf position
            (%logical-replication-expect-marker
             payload position #x4e :pgoutput-insert))
      (multiple-value-bind (new-tuple next)
          (%logical-replication-read-tuple
           payload position :pgoutput-insert)
        (%ensure-payload-end payload next :pgoutput-insert)
        (%logical-replication-message
         :insert protocol-version
         :xid xid
         :relation-id relation-id
         :new-tuple new-tuple)))))

(defun %parse-logical-replication-update
    (payload protocol-version streamed-p)
  (let ((position 1)
        (old-tuple nil)
        (old-tuple-kind nil))
    (multiple-value-bind (xid position)
        (%logical-replication-read-stream-xid payload position streamed-p)
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
            (%logical-replication-message
             :update protocol-version
             :xid xid
             :relation-id relation-id
             :old-tuple old-tuple
             :old-tuple-kind old-tuple-kind
             :new-tuple new-tuple)))))))

(defun %parse-logical-replication-delete
    (payload protocol-version streamed-p)
  (%with-logical-replication-stream-context
      (payload position streamed-p xid)
    (%with-replication-fields
        (position
         (relation-id (%read-u32 payload position)))
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
            (%logical-replication-message
             :delete protocol-version
             :xid xid
             :relation-id relation-id
             :old-tuple old-tuple
             :old-tuple-kind old-tuple-kind)))))))

(defun %parse-logical-replication-truncate
    (payload protocol-version streamed-p)
  (%with-logical-replication-stream-context
      (payload position streamed-p xid)
    (%with-replication-fields
        (position
         (count (%read-i32 payload position)))
      (let ((options (%octet-at payload position))
            (position (1+ position)))
        (%ensure-count-capacity payload position count 4
                                :pgoutput-truncate)
        (let ((relation-ids (make-array count)))
          (loop for index from 0 below count
                do (multiple-value-bind (relation-id next)
                       (%read-u32 payload position)
                     (setf position next
                           (aref relation-ids index) relation-id)))
          (%ensure-payload-end payload position :pgoutput-truncate)
          (%logical-replication-message
           :truncate protocol-version
           :xid xid
           :relation-ids relation-ids
           :options options))))))
