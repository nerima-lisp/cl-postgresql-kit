(in-package #:cl-postgresql-kit)

(defclass logical-replication-unchanged-toast ()
  ()
  (:documentation
   "Sentinel used when pgoutput leaves a TOAST value unchanged."))

(defparameter +logical-replication-unchanged-toast+
  (make-instance 'logical-replication-unchanged-toast)
  "The value returned for an unchanged TOAST field.")

(defun logical-replication-unchanged-toast-p (value)
  (typep value 'logical-replication-unchanged-toast))

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

(defun %logical-replication-field-format (field-kind)
  (case field-kind
    (:text 0)
    (:binary 1)
    (otherwise
     (error 'protocol-error
            :context :logical-replication
            :expected '(:text :binary)
            :actual field-kind
            :message
            "Logical replication field format must be :TEXT or :BINARY."))))

(defun %logical-replication-field-data-or-error (field)
  (let ((data (logical-replication-field-data field)))
    (unless (%logical-replication-octet-vector-p data)
      (error 'protocol-error
             :context :logical-replication
             :expected '(vector (unsigned-byte 8))
             :actual data
             :message
             "Logical replication field data must be an octet vector."))
    data))

(defun %logical-replication-decode-field (decoder column field)
  (let ((field-kind (logical-replication-field-kind field)))
    (case field-kind
      (:null +sql-null+)
      (:unchanged-toast
       +logical-replication-unchanged-toast+)
      ((:text :binary)
       (decode-value
        (logical-replication-decoder-type-registry decoder)
        (logical-replication-column-type-oid column)
        (%logical-replication-field-data-or-error field)
        :format (%logical-replication-field-format field-kind)))
      (otherwise
       (error 'protocol-error
              :context :logical-replication
              :expected '(:null :unchanged-toast :text :binary)
              :actual field-kind
              :message
              "Unknown logical replication field kind.")))))

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
                     (%logical-replication-decode-field
                      decoder column field)))
      values)))

(defun %logical-replication-required-tuple (tuple role)
  (or tuple
      (error 'protocol-error
             :context :logical-replication
             :expected role
             :actual nil
             :message "A logical replication row message is missing its tuple.")))

(defun %logical-replication-row-context (decoder message)
  (let ((relation-id (logical-replication-message-relation-id message)))
    (values relation-id
            (%logical-replication-relation-or-error decoder relation-id))))

(defun %logical-replication-decode-row-values
    (decoder relation-id tuple tuple-kind &key requiredp)
  (let ((tuple (if requiredp
                   (%logical-replication-required-tuple tuple tuple-kind)
                   tuple)))
    (when tuple
      (decode-logical-replication-tuple
       decoder relation-id tuple :tuple-kind tuple-kind))))

(defmacro %logical-replication-row-event
    ((kind decoder message relation-id relation)
     &key new-values old-values old-values-kind)
  `(multiple-value-bind (,relation-id ,relation)
       (%logical-replication-row-context ,decoder ,message)
     (%make-logical-replication-event
      :kind ,kind
      :message ,message
      :relation ,relation
      ,@(when new-values
          `(:new-values ,new-values))
      ,@(when old-values
          `(:old-values ,old-values))
      ,@(when old-values-kind
          `(:old-values-kind ,old-values-kind)))))

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
     (%logical-replication-row-event
         (:insert decoder message relation-id relation)
       :new-values (%logical-replication-decode-row-values
                    decoder relation-id
                    (logical-replication-message-new-tuple message)
                    :new
                    :requiredp t)))
    (:update
     (%logical-replication-row-event
         (:update decoder message relation-id relation)
       :new-values (%logical-replication-decode-row-values
                    decoder relation-id
                    (logical-replication-message-new-tuple message)
                    :new
                    :requiredp t)
       :old-values (let ((old-tuple (logical-replication-message-old-tuple message))
                         (old-kind (logical-replication-message-old-tuple-kind message)))
                     (%logical-replication-decode-row-values
                      decoder relation-id old-tuple old-kind))
       :old-values-kind (logical-replication-message-old-tuple-kind message)))
    (:delete
     (%logical-replication-row-event
         (:delete decoder message relation-id relation)
       :old-values (let ((old-kind (logical-replication-message-old-tuple-kind message)))
                     (%logical-replication-decode-row-values
                      decoder relation-id
                      (logical-replication-message-old-tuple message)
                      old-kind
                      :requiredp t))
       :old-values-kind (logical-replication-message-old-tuple-kind message)))
    (otherwise
     (%make-logical-replication-event
      :kind (logical-replication-message-kind message)
      :message message))))
