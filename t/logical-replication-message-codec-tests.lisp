(in-package #:cl-postgresql-kit/test)

(deftest logical-replication-message-codecs
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
    (let ((message
           (parse-logical-replication-message
            (payload #\B
                     (lambda (builder)
                       (append-u64 builder 10)
                       (append-i64 builder -11)
                       (append-u32 builder 12))))))
      (is (eq :begin (logical-replication-message-kind message)))
      (is (= 1 (logical-replication-message-protocol-version message)))
      (is (= 10 (logical-replication-message-final-lsn message)))
      (is (= -11 (logical-replication-message-commit-time message)))
      (is (= 12 (logical-replication-message-xid message))))
    (let ((message
           (parse-logical-replication-message
            (payload #\M
                     (lambda (builder)
                       (append-u8 builder 1)
                       (append-u64 builder 20)
                       (append-octets builder (cstring "prefix"))
                       (append-i32 builder 3)
                       (append-octets builder (octets 1 2 3)))))))
      (is (eq :message (logical-replication-message-kind message)))
      (is (= 1 (logical-replication-message-flags message)))
      (is (= 20 (logical-replication-message-message-lsn message)))
      (is (string= "prefix" (logical-replication-message-prefix message)))
      (is (equalp (octets 1 2 3)
                  (logical-replication-message-content message))))
    (let ((message
           (parse-logical-replication-message
            (payload #\C
                     (lambda (builder)
                       (append-u8 builder 2)
                       (append-u64 builder 30)
                       (append-u64 builder 31)
                       (append-i64 builder -32))))))
      (is (eq :commit (logical-replication-message-kind message)))
      (is (= 2 (logical-replication-message-flags message)))
      (is (= 30 (logical-replication-message-commit-lsn message)))
      (is (= 31 (logical-replication-message-end-lsn message)))
      (is (= -32 (logical-replication-message-commit-time message))))
    (let ((message
           (parse-logical-replication-message
            (payload #\O
                     (lambda (builder)
                       (append-u64 builder 40)
                       (append-octets builder (cstring "origin")))))))
      (is (eq :origin (logical-replication-message-kind message)))
      (is (= 40 (logical-replication-message-origin-lsn message)))
      (is (string= "origin" (logical-replication-message-origin-name message))))
    (let* ((message
            (parse-logical-replication-message
             (payload #\R
                      (lambda (builder)
                        (append-u32 builder 50)
                        (append-octets builder (cstring "public"))
                        (append-octets builder (cstring "items"))
                        (append-u8 builder 2)
                        (append-i16 builder 2)
                        (append-octets builder (column 1 "id" 23 -1))
                        (append-octets builder (column 0 "body" 25 -1))))))
           (columns (logical-replication-message-columns message)))
      (is (eq :relation (logical-replication-message-kind message)))
      (is (= 50 (logical-replication-message-relation-id message)))
      (is (string= "public" (logical-replication-message-namespace message)))
      (is (string= "items" (logical-replication-message-relation-name message)))
      (is (= 2 (logical-replication-message-replica-identity message)))
      (is (= 2 (length columns)))
      (is (= 1 (logical-replication-column-flags (aref columns 0))))
      (is (string= "id" (logical-replication-column-name (aref columns 0))))
      (is (= 23 (logical-replication-column-type-oid (aref columns 0))))
      (is (= -1 (logical-replication-column-type-modifier (aref columns 0))))
      (is (string= "body" (logical-replication-column-name (aref columns 1)))))
    (let ((message
           (parse-logical-replication-message
            (payload #\Y
                     (lambda (builder)
                       (append-u32 builder 60)
                       (append-octets builder (cstring "pg_catalog"))
                       (append-octets builder (cstring "my_type")))))))
      (is (eq :type (logical-replication-message-kind message)))
      (is (= 60 (logical-replication-message-type-oid message)))
      (is (string= "pg_catalog"
                   (logical-replication-message-type-namespace message)))
      (is (string= "my_type"
                   (logical-replication-message-type-name message))))
    (let ((message
           (parse-logical-replication-message
            (payload #\I
                     (lambda (builder)
                       (append-u32 builder 70)
                       (append-u8 builder (char-code #\N))
                       (append-octets
                        builder
                        (tuple (list :null
                                     :unchanged-toast
                                     (list :text (octets 1 2))
                                     (list :binary (octets 3 4 5))))))))))
      (let ((tuple (logical-replication-message-new-tuple message)))
        (is (eq :insert (logical-replication-message-kind message)))
        (is (= 70 (logical-replication-message-relation-id message)))
        (is (= 4 (length (logical-replication-tuple-fields tuple))))
        (is (eq :null
                (logical-replication-field-kind
                 (aref (logical-replication-tuple-fields tuple) 0))))
        (is (eq +sql-null+
                (logical-replication-field-data
                 (aref (logical-replication-tuple-fields tuple) 0))))
        (is (eq :unchanged-toast
                (logical-replication-field-kind
                 (aref (logical-replication-tuple-fields tuple) 1))))
        (is (null
             (logical-replication-field-data
              (aref (logical-replication-tuple-fields tuple) 1))))
        (is (equalp (octets 1 2)
                    (logical-replication-field-data
                     (aref (logical-replication-tuple-fields tuple) 2))))
        (is (eq :binary
                (logical-replication-field-kind
                 (aref (logical-replication-tuple-fields tuple) 3))))))
    (let ((message
           (parse-logical-replication-message
            (payload #\U
                     (lambda (builder)
                       (append-u32 builder 71)
                       (append-u8 builder (char-code #\K))
                       (append-octets builder (tuple (list (list :text (octets 9)))))
                       (append-u8 builder (char-code #\N))
                       (append-octets builder (tuple (list (list :text (octets 8))))))))))
      (is (eq :update (logical-replication-message-kind message)))
      (is (= 71 (logical-replication-message-relation-id message)))
      (is (eq :key (logical-replication-message-old-tuple-kind message)))
      (is (equalp (octets 9)
                  (logical-replication-field-data
                   (aref (logical-replication-tuple-fields
                          (logical-replication-message-old-tuple message))
                         0))))
      (is (equalp (octets 8)
                  (logical-replication-field-data
                   (aref (logical-replication-tuple-fields
                          (logical-replication-message-new-tuple message))
                         0)))))
    (let ((message
           (parse-logical-replication-message
            (payload #\U
                     (lambda (builder)
                       (append-u32 builder 72)
                       (append-u8 builder (char-code #\N))
                       (append-octets builder (tuple (list (list :text (octets 7))))))))))
      (is (eq :update (logical-replication-message-kind message)))
      (is (null (logical-replication-message-old-tuple message)))
      (is (null (logical-replication-message-old-tuple-kind message))))
    (let ((message
           (parse-logical-replication-message
            (payload #\D
                     (lambda (builder)
                       (append-u32 builder 73)
                       (append-u8 builder (char-code #\O))
                       (append-octets builder (tuple (list (list :binary (octets 6))))))))))
      (is (eq :delete (logical-replication-message-kind message)))
      (is (eq :old (logical-replication-message-old-tuple-kind message)))
      (is (= 73 (logical-replication-message-relation-id message))))
    (let ((message
           (parse-logical-replication-message
            (payload #\T
                     (lambda (builder)
                       (append-i32 builder 2)
                       (append-u8 builder 3)
                       (append-u32 builder 80)
                       (append-u32 builder 81))))))
      (is (eq :truncate (logical-replication-message-kind message)))
      (is (= 3 (logical-replication-message-options message)))
      (is (equalp #(80 81)
                  (logical-replication-message-relation-ids message))))))
