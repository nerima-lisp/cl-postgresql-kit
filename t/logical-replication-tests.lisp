(in-package #:cl-postgresql-kit/test)

(deftest logical-replication-relation-cache-lifecycle
  (with-logical-replication-decoder
      (decoder :type-registry (make-type-registry :include-defaults nil))
    (with-registered-logical-replication-relation
        (decoder relation :relation-id 42 :columns #())
      (is (null (find-logical-replication-relation decoder 7)))
      (is (eq relation (find-logical-replication-relation decoder 42)))
      (is (forget-logical-replication-relation decoder 42))
      (is (null (find-logical-replication-relation decoder 42)))
      (register-logical-replication-relation decoder relation)
      (clear-logical-replication-relations decoder)
      (is (null (find-logical-replication-relation decoder 42))))))

(deftest logical-replication-tuple-decoding-preserves-wire-values
  (with-logical-replication-decoder
      (decoder :type-registry (make-type-registry :include-defaults nil))
    (let* ((wire-value (make-array 2
                                   :element-type '(unsigned-byte 8)
                                   :initial-contents '(49 50)))
           (column (cl-postgresql-kit::%make-logical-replication-column
                    :flags 1 :name "id" :type-oid 99999 :type-modifier -1))
           (tuple (cl-postgresql-kit::%make-logical-replication-tuple
                   :fields (vector
                            (cl-postgresql-kit::%make-logical-replication-field
                             :kind :text
                             :data wire-value)))))
      (with-registered-logical-replication-relation
          (decoder relation :relation-id 7 :columns (vector column))
        (let ((values (decode-logical-replication-tuple decoder 7 tuple)))
          (is (= 1 (length values)))
          (is (equalp wire-value (aref values 0))))
        (let ((values
                (decode-logical-replication-tuple
                 decoder 7
                 (cl-postgresql-kit::%make-logical-replication-tuple
                  :fields (vector
                           (cl-postgresql-kit::%make-logical-replication-field
                            :kind :unchanged-toast))))))
          (is (logical-replication-unchanged-toast-p (aref values 0))))))))

(deftest logical-replication-message-decoding-emits-events
  (with-logical-replication-decoder
      (decoder :type-registry (make-type-registry :include-defaults nil))
    (let* ((relation (cl-postgresql-kit::%make-logical-replication-message
                      :kind :relation :relation-id 9 :columns #()))
           (event (decode-logical-replication-message decoder relation)))
      (is (eq :relation
              (cl-postgresql-kit::logical-replication-event-kind event)))
      (is (eq relation
              (cl-postgresql-kit::logical-replication-event-message event)))
      (is (eq relation
              (cl-postgresql-kit::logical-replication-event-relation event))))))

(deftest logical-replication-stream-message-decoding-preserves-physical-envelopes
  (with-logical-replication-decoder
      (decoder :type-registry (make-type-registry :include-defaults nil))
    (let* ((replication-message
             (cl-postgresql-kit::%make-replication-message
              :kind :primary-keepalive
              :wal-end 5
              :send-time 6
              :reply-requested t))
           (event
             (decode-logical-replication-stream-message
              decoder replication-message)))
      (is (logical-replication-stream-event-p event))
      (is (eq :primary-keepalive
              (logical-replication-stream-event-kind event)))
      (is (eq replication-message
              (logical-replication-stream-event-replication-message event)))
      (is (null (logical-replication-stream-event-logical-message event)))
      (is (null (logical-replication-stream-event-logical-event event))))))

(deftest logical-replication-stream-message-decoding-decodes-xlog-data
  (with-logical-replication-decoder
      (decoder :type-registry (make-type-registry :include-defaults nil))
    (let* ((payload
           (let ((builder (make-octet-builder)))
               (append-u8 builder (char-code #\B))
               (append-u64 builder 11)
               (append-i64 builder 12)
               (append-u32 builder 13)
               (builder-octets builder)))
           (replication-message
             (cl-postgresql-kit::%make-replication-message
              :kind :xlog-data
              :data payload))
           (stream-event
             (decode-logical-replication-stream-message
              decoder replication-message))
           (logical-message
             (logical-replication-stream-event-logical-message stream-event))
           (logical-event
             (logical-replication-stream-event-logical-event stream-event)))
      (is (logical-replication-stream-event-p stream-event))
      (is (eq :xlog-data
              (logical-replication-stream-event-kind stream-event)))
      (is (eq replication-message
              (logical-replication-stream-event-replication-message
               stream-event)))
      (is (eq :begin (logical-replication-message-kind logical-message)))
      (is (= 11 (logical-replication-message-final-lsn logical-message)))
      (is (= 12 (logical-replication-message-commit-time logical-message)))
      (is (= 13 (logical-replication-message-xid logical-message)))
      (is (eq :begin (cl-postgresql-kit::logical-replication-event-kind
                      logical-event)))
      (is (eq logical-message
              (cl-postgresql-kit::logical-replication-event-message
               logical-event))))))

(deftest logical-replication-row-message-decoding-emits-old-and-new-values
  (with-logical-replication-decoder
      (decoder :type-registry (make-type-registry :include-defaults nil))
    (let* ((columns (vector
                     (cl-postgresql-kit::%make-logical-replication-column
                      :flags 1 :name "id" :type-oid 100 :type-modifier -1)
                     (cl-postgresql-kit::%make-logical-replication-column
                      :flags 2 :name "body" :type-oid 101 :type-modifier -1)))
           (new-tuple (cl-postgresql-kit::%make-logical-replication-tuple
                       :fields (vector
                                (cl-postgresql-kit::%make-logical-replication-field
                                 :kind :text
                                 :data (make-array 1
                                                   :element-type '(unsigned-byte 8)
                                                   :initial-contents '(49)))
                                (cl-postgresql-kit::%make-logical-replication-field
                                 :kind :binary
                                 :data (make-array 2
                                                   :element-type '(unsigned-byte 8)
                                                   :initial-contents '(50 51))))))
           (old-tuple (cl-postgresql-kit::%make-logical-replication-tuple
                       :fields (vector
                                (cl-postgresql-kit::%make-logical-replication-field
                                 :kind :text
                                 :data (make-array 1
                                                   :element-type '(unsigned-byte 8)
                                                   :initial-contents '(49))))))
           (update-message (cl-postgresql-kit::%make-logical-replication-message
                            :kind :update
                            :relation-id 10
                            :new-tuple new-tuple
                            :old-tuple old-tuple
                            :old-tuple-kind :key))
           (delete-message (cl-postgresql-kit::%make-logical-replication-message
                            :kind :delete
                            :relation-id 10
                            :old-tuple old-tuple
                            :old-tuple-kind :key)))
      (with-registered-logical-replication-relation
          (decoder relation :relation-id 10 :columns columns)
        (let ((update-event
                (decode-logical-replication-message decoder update-message)))
          (is (eq :update
                  (cl-postgresql-kit::logical-replication-event-kind update-event)))
          (is (equalp #(49) (aref (cl-postgresql-kit::logical-replication-event-new-values
                                   update-event)
                                  0)))
          (is (equalp #(50 51)
                      (aref (cl-postgresql-kit::logical-replication-event-new-values
                             update-event)
                            1)))
          (is (eq :key
                  (cl-postgresql-kit::logical-replication-event-old-values-kind
                   update-event)))
          (is (equalp #(49)
                      (aref (cl-postgresql-kit::logical-replication-event-old-values
                             update-event)
                            0))))
        (let ((delete-event
                (decode-logical-replication-message decoder delete-message)))
          (is (eq :delete
                  (cl-postgresql-kit::logical-replication-event-kind delete-event)))
          (is (eq :key
                  (cl-postgresql-kit::logical-replication-event-old-values-kind
                   delete-event)))
          (is (equalp #(49)
                      (aref (cl-postgresql-kit::logical-replication-event-old-values
                             delete-event)
                            0))))))))

(deftest logical-replication-unchanged-toast-is-a-distinct-sentinel
  (is (logical-replication-unchanged-toast-p
       cl-postgresql-kit::+logical-replication-unchanged-toast+))
  (is (not (logical-replication-unchanged-toast-p nil))))

(deftest logical-replication-tuple-decoding-covers-null-binary-and-identity-columns
  (with-logical-replication-decoder
      (decoder :type-registry (make-type-registry :include-defaults nil))
    (let ((columns (vector
                    (cl-postgresql-kit::%make-logical-replication-column
                     :flags 1 :name "id" :type-oid 100 :type-modifier -1)
                    (cl-postgresql-kit::%make-logical-replication-column
                     :flags 2 :name "old" :type-oid 101 :type-modifier -1))))
      (with-registered-logical-replication-relation
          (decoder relation :relation-id 11 :columns columns)
        (let ((values
                (decode-logical-replication-tuple
                 decoder 11
                 (cl-postgresql-kit::%make-logical-replication-tuple
                  :fields (vector
                           (cl-postgresql-kit::%make-logical-replication-field
                            :kind :null)
                           (cl-postgresql-kit::%make-logical-replication-field
                            :kind :binary
                            :data (make-array 2
                                              :element-type '(unsigned-byte 8)
                                              :initial-contents '(1 2)))))
                 :tuple-kind :new)))
          (is (eq +sql-null+ (aref values 0)))
          (is (equalp #(1 2) (aref values 1))))
        (is-case-each
            ((tuple-kind)
             '((:key)
               (:old)))
          (let ((values
                  (decode-logical-replication-tuple
                   decoder 11
                   (cl-postgresql-kit::%make-logical-replication-tuple
                    :fields (vector
                             (cl-postgresql-kit::%make-logical-replication-field
                              :kind :null)))
                   :tuple-kind tuple-kind)))
            (is (eq +sql-null+ (aref values 0)))))))))

(deftest logical-replication-rejects-invalid-tuples-and-relations
  (with-logical-replication-decoder
      (decoder :type-registry (make-type-registry :include-defaults nil))
    (it-signals-each 'parameter-error
        ((:invalid-relation-id)
         (:invalid-relation-message))
      "rejects invalid logical replication relation case ~A"
      (label)
      (case label
        (:invalid-relation-id
         (find-logical-replication-relation decoder -1))
        (:invalid-relation-message
         (register-logical-replication-relation
          decoder
          (cl-postgresql-kit::%make-logical-replication-message
           :kind :insert :relation-id 12)))))
    (with-registered-logical-replication-relation
        (decoder relation
                 :relation-id 12
                 :columns (vector
                           (cl-postgresql-kit::%make-logical-replication-column
                            :flags 0 :name "value"
                            :type-oid 100 :type-modifier -1)))
      (it-signals-each 'parameter-error
          ((:invalid-tuple-kind))
        "rejects invalid logical replication tuple case ~A"
        (label)
        (declare (ignore label))
        (decode-logical-replication-tuple
         decoder 12
         (cl-postgresql-kit::%make-logical-replication-tuple
          :fields #())
         :tuple-kind :invalid))
      (assert-signals 'type-error
                      (lambda ()
                        (decode-logical-replication-tuple
                         decoder 12
                         (cl-postgresql-kit::%make-logical-replication-tuple
                          :fields #(nil)))))
      (assert-signals 'protocol-error
                      (lambda ()
                        (decode-logical-replication-message
                         decoder
                         (cl-postgresql-kit::%make-logical-replication-message
                          :kind :insert :relation-id 99)))))))
