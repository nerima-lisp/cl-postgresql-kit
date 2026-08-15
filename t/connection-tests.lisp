(in-package #:cl-postgresql-kit/test)

(deftest transaction-isolation-is-whitelisted
  (let ((connection (ready-memory-connection)))
    (unwind-protect
         (progn
           (is (string= (cl-postgresql-kit::%transaction-isolation-sql
                         :repeatable-read)
                        "REPEATABLE READ"))
           (assert-signals 'parameter-error
                          (lambda ()
                            (begin-transaction
                             connection
                             :isolation "SERIALIZABLE; DROP TABLE accounts"))))
      (disconnect connection))))

(deftest with-transaction-is-hygienic-and-evaluates-connection-once
  (let ((committed :caller-binding)
        (connection-evaluations 0)
        (events nil))
    (flet ((begin-transaction (connection &key isolation read-only deferrable)
             (declare (ignore connection isolation read-only deferrable))
             (push :begin events))
           (commit-transaction (connection)
             (declare (ignore connection))
             (push :commit events))
           (rollback-transaction (connection)
             (declare (ignore connection))
             (push :rollback events)))
      (multiple-value-bind (value marker)
          (with-transaction ((progn
                              (incf connection-evaluations)
                              :connection))
            (push committed events)
            (values :value :marker))
        (is (eql connection-evaluations 1))
        (is (equal events '(:commit :caller-binding :begin)))
        (is (eql value :value))
        (is (eql marker :marker))))))

(deftest with-transaction-rolls-back-when-the-body-signals
  (let ((events nil))
    (flet ((begin-transaction (connection &key isolation read-only deferrable)
             (declare (ignore connection isolation read-only deferrable))
             (push :begin events))
           (commit-transaction (connection)
             (declare (ignore connection))
             (push :commit events))
           (rollback-transaction (connection)
             (declare (ignore connection))
             (push :rollback events)))
      (assert-signals 'simple-error
                      (lambda ()
                        (with-transaction (:connection)
                          (push :body events)
                          (error "transaction body failed"))))
      (is (equal events '(:rollback :body :begin))))))

(deftest connection-secrets-clear-on-disconnect
  (let ((connection (ready-memory-connection :password "secret")))
    (setf (connection-backend-process-id connection) 42
          (connection-backend-secret-key connection) 99)
    (disconnect connection)
    (is (null (connection-password connection)))
    (is (null (connection-backend-process-id connection)))
    (is (null (connection-backend-secret-key connection)))
    (is (null (connection-tls-established-p connection)))))

(deftest disconnect-clears-session-state
  (let ((connection (ready-memory-connection)))
    (setf (gethash "name" (connection-parameters connection)) "value"
          (gethash "statement"
                   (cl-postgresql-kit::connection--prepared-statements connection))
          t
          (connection-last-result connection) :sentinel
          (cl-postgresql-kit::connection--active-copy connection) :sentinel
          (cl-postgresql-kit::connection--pending-error connection) :sentinel)
    (disconnect connection)
    (is (zerop (hash-table-count (connection-parameters connection))))
    (is (zerop
           (hash-table-count
            (cl-postgresql-kit::connection--prepared-statements connection))))
    (is (null (connection-last-result connection)))
    (is (null (cl-postgresql-kit::connection--active-copy connection)))
    (is (null (cl-postgresql-kit::connection--pending-error connection)))
    (is (eq (connection-transaction-status connection) :idle))))

(deftest notify-uses-parameterized-query
  (let ((writes nil)
        (payload "a\\b'payload")
        (connection nil))
    (setf connection
          (ready-memory-connection
           :on-write
           (lambda (transport octets)
             (push (copy-seq octets) writes)
             (when (and (plusp (length octets))
                        (= (aref octets 0) (char-code #\S)))
               (memory-transport-append-input
                transport
                (join-octets
                 (make-frame #\1 #())
                 (make-frame #\2 #())
                 (make-frame #\T (octets 0 0))
                 (make-frame #\C (cstring "SELECT 1"))
                 (make-frame #\Z (octets (char-code #\I)))))))))
    (unwind-protect
         (progn
           (notify connection "events" payload)
           (let* ((ordered-writes (reverse writes))
                  (types (mapcar (lambda (wire)
                                   (code-char (aref wire 0)))
                                 ordered-writes))
                  (payload-octets
                    (cl-codec-kit:string-to-octets payload :encoding :utf-8)))
             (is (equal '(#\P #\B #\D #\E #\S) types))
             (is (not (search payload-octets (first ordered-writes))))
             (is (search payload-octets (second ordered-writes))))
          (it-signals-each 'parameter-error
              ((:empty-channel)
               (:nul-in-channel)
               (:nul-in-payload))
            "rejects invalid NOTIFY input case ~A"
            (label)
            (case label
              (:empty-channel
               (notify connection ""))
              (:nul-in-channel
               (notify connection
                       (format nil "bad~C" #\Null)))
              (:nul-in-payload
               (notify connection "events"
                       (format nil "bad~C" #\Null)))))
          (assert-signals
           'type-error
           (lambda ()
             (notify connection "events" 1)))
          )
      (disconnect connection))))
