(in-package #:cl-postgresql-kit/test)

(deftest logical-replication-value-decoder
  (labels ((payload (marker writer)
             (let ((builder (make-octet-builder)))
               (append-u8 builder (char-code marker))
               (funcall writer builder)
               (builder-octets builder)))
           (column (flags name type-oid type-modifier)
             (let ((builder (make-octet-builder)))
               (append-u8 builder flags)
               (append-octets builder (cstring name))
               (append-u32 builder type-oid)
               (append-i32 builder type-modifier)
               (builder-octets builder)))
           (tuple (fields)
             (let ((builder (make-octet-builder)))
               (append-i16 builder (length fields))
               (dolist (field fields)
                 (cond
                   ((eq field :null)
                    (append-u8 builder (char-code #\n)))
                   ((eq field :unchanged-toast)
                    (append-u8 builder (char-code #\u)))
                   ((and (consp field) (eq (first field) :text))
                    (append-u8 builder (char-code #\t))
                    (append-i32 builder (length (second field)))
                    (append-octets builder (second field)))
                   ((and (consp field) (eq (first field) :binary))
                    (append-u8 builder (char-code #\b))
                    (append-i32 builder (length (second field)))
                    (append-octets builder (second field)))
                   (t
                    (error "Unknown test TupleData field: ~S" field))))
               (builder-octets builder))))
    (let* ((relation-message
             (parse-logical-replication-message
              (payload #\R
                       (lambda (builder)
                         (append-u32 builder 200)
                         (append-octets builder (cstring "public"))
                         (append-octets builder (cstring "items"))
                         (append-u8 builder 3)
                         (append-i16 builder 3)
                         (append-octets builder (column #x03 "id" 23 -1))
                         (append-octets builder (column #x02 "body" 25 -1))
                         (append-octets builder (column #x00 "note" 25 -1))))))
           (decoder (make-logical-replication-decoder))
           (relation-event
             (decode-logical-replication-message decoder relation-message)))
      (is (eq :relation (logical-replication-event-kind relation-event)))
      (is (eq relation-message
              (logical-replication-event-relation relation-event)))
      (is (eq relation-message
              (find-logical-replication-relation decoder 200)))
      (let* ((message
               (parse-logical-replication-message
                (payload #\I
                         (lambda (builder)
                           (append-u32 builder 200)
                           (append-u8 builder (char-code #\N))
                           (append-octets
                            builder
                            (tuple (list (list :text (octets 52 50))
                                         (list :text
                                               (octets 104 101 108 108 111))
                                         :null)))))))
             (event (decode-logical-replication-message decoder message))
             (values (logical-replication-event-new-values event)))
        (is (eq :insert (logical-replication-event-kind event)))
        (is (= 42 (aref values 0)))
        (is (string= "hello" (aref values 1)))
        (is (eq +sql-null+ (aref values 2))))
      (let* ((message
               (parse-logical-replication-message
                (payload #\I
                         (lambda (builder)
                           (append-u32 builder 200)
                           (append-u8 builder (char-code #\N))
                           (append-octets
                            builder
                            (tuple (list (list :binary (octets 0 0 0 42))
                                         (list :binary
                                               (octets 119 111 114 108 100))
                                         :null)))))))
             (event (decode-logical-replication-message decoder message)))
        (is (= 42
               (aref (logical-replication-event-new-values event) 0)))
        (is (string= "world"
                     (aref (logical-replication-event-new-values event) 1))))
      (let* ((message
               (parse-logical-replication-message
                (payload #\U
                         (lambda (builder)
                           (append-u32 builder 200)
                           (append-u8 builder (char-code #\K))
                           (append-octets
                            builder
                            (tuple (list (list :text (octets 52 50)))))
                           (append-u8 builder (char-code #\N))
                           (append-octets
                            builder
                            (tuple (list (list :text (octets 52 51))
                                         :unchanged-toast
                                         (list :text
                                               (octets 119 111 114 108 100)))))))))
             (event (decode-logical-replication-message decoder message))
             (new-values (logical-replication-event-new-values event))
             (old-values (logical-replication-event-old-values event)))
        (is (eq :update (logical-replication-event-kind event)))
        (is (= 43 (aref new-values 0)))
        (is (eq +logical-replication-unchanged-toast+
                (aref new-values 1)))
        (is (string= "world" (aref new-values 2)))
        (is (= 1 (length old-values)))
        (is (= 42 (aref old-values 0)))
        (is (eq :key
                (logical-replication-event-old-values-kind event))))
      (let* ((message
               (parse-logical-replication-message
                (payload #\D
                         (lambda (builder)
                           (append-u32 builder 200)
                           (append-u8 builder (char-code #\O))
                           (append-octets
                            builder
                            (tuple (list (list :text (octets 52 51))
                                         (list :text
                                               (octets 119 111 114 108 100)))))))))
             (event (decode-logical-replication-message decoder message))
             (old-values (logical-replication-event-old-values event)))
        (is (eq :delete (logical-replication-event-kind event)))
        (is (= 2 (length old-values)))
        (is (= 43 (aref old-values 0)))
        (is (string= "world" (aref old-values 1)))
        (is (eq :old
                (logical-replication-event-old-values-kind event))))
      (let ((unknown-message
              (parse-logical-replication-message
               (payload #\I
                        (lambda (builder)
                          (append-u32 builder 999)
                          (append-u8 builder (char-code #\N))
                          (append-octets
                           builder
                           (tuple (list (list :text (octets 52 50))))))))))
        (assert-signals
         'protocol-error
         (lambda ()
           (decode-logical-replication-message decoder unknown-message))))
      (let ((mismatch-message
              (parse-logical-replication-message
               (payload #\I
                        (lambda (builder)
                          (append-u32 builder 200)
                          (append-u8 builder (char-code #\N))
                          (append-octets
                           builder
                           (tuple (list (list :text (octets 52 50))
                                        (list :text (octets 120))))))))))
        (assert-signals
         'protocol-error
         (lambda ()
           (decode-logical-replication-message decoder mismatch-message))))
      (is (forget-logical-replication-relation decoder 200))
      (is (null (find-logical-replication-relation decoder 200)))
      (is (eq decoder (clear-logical-replication-relations decoder))))))

(deftest logical-replication-stream-decoder
  (labels ((payload (marker writer)
             (let ((builder (make-octet-builder)))
               (append-u8 builder (char-code marker))
               (funcall writer builder)
               (builder-octets builder)))
           (tuple (fields)
             (let ((builder (make-octet-builder)))
               (append-i16 builder (length fields))
               (dolist (field fields)
                 (append-u8 builder (char-code #\t))
                 (append-i32 builder (length field))
                 (append-octets builder field))
               (builder-octets builder)))
           (relation-payload ()
             (payload
              #\R
              (lambda (builder)
                (append-u32 builder 200)
                (append-octets builder (cstring "public"))
                (append-octets builder (cstring "items"))
                (append-u8 builder 3)
                (append-i16 builder 3)
                (dolist (column '((#x03 "id" 23 -1)
                                  (#x02 "body" 25 -1)
                                  (#x00 "note" 25 -1)))
                  (append-u8 builder (first column))
                  (append-octets builder (cstring (second column)))
                  (append-u32 builder (third column))
                  (append-i32 builder (fourth column))))))
           (insert-payload ()
             (payload
              #\I
              (lambda (builder)
                (append-u32 builder 200)
                (append-u8 builder (char-code #\N))
                (append-octets
                 builder
                 (tuple (list (octets 52 50)
                              (octets 104 101 108 108 111)
                              (octets 110 111 116 101)))))))
           (xlog-payload (logical-payload)
             (let ((builder (make-octet-builder)))
               (append-u8 builder (char-code #\w))
               (append-u64 builder 10)
               (append-u64 builder 12)
               (append-i64 builder 123)
               (append-octets builder logical-payload)
               (builder-octets builder))))
    (let* ((replication-input
             (join-octets
              (make-frame #\W (copy-response-payload))
              (make-frame #\d (xlog-payload (relation-payload)))
              (make-frame #\d (xlog-payload (insert-payload)))
              (make-frame #\d
                          (let ((builder (make-octet-builder)))
                            (append-u8 builder (char-code #\k))
                            (append-u64 builder 12)
                            (append-i64 builder 124)
                            (append-u8 builder 1)
                            (builder-octets builder)))
              (make-frame #\c #())
              (make-frame #\C (cstring "START_REPLICATION"))
              (make-frame #\Z (octets (char-code #\I)))))
           (connection (ready-memory-connection :input replication-input))
           (decoder (make-logical-replication-decoder)))
      (unwind-protect
           (let ((operation
                   (replication-start
                    connection
                    "START_REPLICATION SLOT slot LOGICAL 0/0")))
             (let* ((relation-stream
                      (logical-replication-read operation decoder))
                    (insert-stream
                      (logical-replication-read operation decoder))
                    (keepalive-stream
                      (logical-replication-read operation decoder)))
               (is (eq :xlog-data
                       (logical-replication-stream-event-kind relation-stream)))
               (is (= 10
                      (replication-message-wal-start
                       (logical-replication-stream-event-replication-message
                        relation-stream))))
               (is (eq :relation
                       (logical-replication-message-kind
                        (logical-replication-stream-event-logical-message
                         relation-stream))))
               (is (eq :relation
                       (logical-replication-event-kind
                        (logical-replication-stream-event-logical-event
                         relation-stream))))
               (is (eq :insert
                       (logical-replication-message-kind
                        (logical-replication-stream-event-logical-message
                         insert-stream))))
               (is (= 42
                      (aref
                       (logical-replication-event-new-values
                        (logical-replication-stream-event-logical-event
                         insert-stream))
                       0)))
               (is (string= "hello"
                            (aref
                             (logical-replication-event-new-values
                              (logical-replication-stream-event-logical-event
                               insert-stream))
                             1)))
               (is (eq :primary-keepalive
                       (logical-replication-stream-event-kind keepalive-stream)))
               (is (null
                    (logical-replication-stream-event-logical-message
                     keepalive-stream)))
               (is (null
                    (logical-replication-stream-event-logical-event
                     keepalive-stream))))
             (is (null (logical-replication-read operation decoder)))
             (is (string= "START_REPLICATION"
                          (replication-finish operation))))
        (disconnect connection)))))
