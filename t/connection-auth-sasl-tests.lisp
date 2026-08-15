(in-package #:cl-postgresql-kit/test)

(deftest scram-attributes-are-strict
  (it-signals-each 'authentication-error
      ((:duplicate-name "r=one,r=two" nil)
       (:unexpected-name "r=one,x=two" ("r"))
       (:empty-value "r=" nil))
    "rejects invalid scram attribute case ~A"
    (label input allowed-names)
    (declare (ignore label))
    (if allowed-names
        (cl-postgresql-kit::%scram-attributes
         input
         :allowed-names allowed-names)
        (cl-postgresql-kit::%scram-attributes input))))

(deftest scram-crypto-boundaries
  (is (cl-postgresql-kit::%constant-time-string= "same" "same"))
  (is (not (cl-postgresql-kit::%constant-time-string= "same" "different")))
  (is (not (cl-postgresql-kit::%constant-time-string= "same" "short")))
  (is (cl-postgresql-kit::%constant-time-octets=
       (octets 1 2)
       (octets 1 2)))
  (is (not (cl-postgresql-kit::%constant-time-octets=
            (octets 1 2)
            (octets 1 3))))
  (is (not (cl-postgresql-kit::%constant-time-octets=
            (octets 1 2)
            (octets 1))))
  (is (string= "alice=3Dadmin=2Cops"
               (cl-postgresql-kit::%scram-username "alice=admin,ops")))
  (let ((attributes (cl-postgresql-kit::%scram-attributes
                     "r=one,s=two"
                     :allowed-names '("r" "s"))))
    (is (string= "one"
                 (cl-postgresql-kit::%scram-attribute attributes "r")))
    (is (string= "two"
                 (cl-postgresql-kit::%scram-attribute attributes "s"))))
  (is (equalp (octets 3 1)
              (cl-postgresql-kit::%xor-octet-vectors
               (octets 1 2)
               (octets 2 3))))
  (assert-signals 'protocol-error
                  (lambda ()
                    (cl-postgresql-kit::%xor-octet-vectors
                     (octets 1)
                     (octets 1 2)))))

(deftest channel-binding-policy
  (let ((connection
          (make-connection
           :channel-binding :require
           :transport (make-memory-transport))))
    (unwind-protect
         (assert-signals 'authentication-error
                         (lambda ()
                           (cl-postgresql-kit::%handle-authentication
                            connection
                            '(:type :sasl
                              :mechanisms ("SCRAM-SHA-256")))))
      (disconnect connection)))
  (let* ((transport (make-memory-transport))
         (connection
           (make-connection
            :channel-binding :disable
            :transport transport)))
    (transport-open transport)
    (setf (connection-open connection) t
          (connection-state connection) :ready
          (connection-tls-established-p connection) t)
    (unwind-protect
         (assert-signals 'authentication-error
                         (lambda ()
                           (cl-postgresql-kit::%handle-authentication
                            connection
                            '(:type :sasl
                              :mechanisms ("SCRAM-SHA-256-PLUS")))))
      (disconnect connection))))

(deftest scram-input-limits-are-enforced
  (let ((*maximum-scram-iterations* 2))
    (assert-signals 'parameter-error
                    (lambda () (scram-hi "password" "salt" 3))))
  (it-signals-each 'authentication-error
      ((:message-length)
       (:attribute-count)
       (:attribute-length))
    "rejects SCRAM input limit case ~A"
    (label)
    (cl-postgresql-kit::%scram-attributes
     (case label
       (:message-length
        (make-string (1+ cl-postgresql-kit::*maximum-scram-message-length*)
                     :initial-element #\x))
       (:attribute-count
        (format nil "~{~A~^,~}"
                (loop for index below
                          (1+ cl-postgresql-kit::*maximum-scram-attribute-count*)
                      collect (format nil "x~D=v" index))))
       (:attribute-length
        (format nil "r=~A"
                (make-string (1+ cl-postgresql-kit::*maximum-scram-attribute-length*)
                             :initial-element #\x)))))))

(deftest scram-plus-channel-binding-header
  (let* ((binding (octets 1 2 3 4))
         (input (make-frame #\R (octets 0 0 0 11)))
         (transport (make-memory-transport
                     :input input
                     :channel-binding-data binding))
         (connection (make-connection
                      :transport transport
                      :user "alice"
                      :password "secret")))
    (transport-open transport)
    (setf (connection-open connection) t
          (connection-state connection) :ready
          (connection-tls-established-p connection) t)
    (unwind-protect
         (progn
           (assert-signals 'authentication-error
                           (lambda ()
                             (cl-postgresql-kit::%handle-authentication
                              connection
                              '(:type :sasl
                                :mechanisms ("SCRAM-SHA-256-PLUS")))))
           (let ((output (memory-transport-output transport)))
             (is (not (null
                       (search (cl-codec-kit:string-to-octets
                                "SCRAM-SHA-256-PLUS")
                               output))))
             (is (not (null
                       (search (cl-codec-kit:string-to-octets
                                "p=tls-server-end-point,,")
                               output))))))
      (disconnect connection))))

(deftest scram-handshake-reports-server-rejection
  (let ((stage 0))
    (let* ((transport
             (make-memory-transport
              :on-write
              (lambda (transport octets)
                (when (and (plusp (length octets))
                           (= (aref octets 0) (char-code #\p)))
                  (incf stage)
                  (case stage
                    (1
                     (let* ((payload (subseq octets 5))
                            (mechanism-and-next
                             (multiple-value-list
                              (cl-postgresql-kit::%read-cstring payload 0)))
                            (mechanism (first mechanism-and-next))
                            (position (second mechanism-and-next)))
                       (is (equal "SCRAM-SHA-256" mechanism))
                       (multiple-value-bind (response-length response-start)
                           (cl-postgresql-kit::%read-u32 payload position)
                         (let* ((response-end (+ response-start response-length))
                                (client-first
                                 (cl-codec-kit:octets-to-string
                                  (subseq payload response-start response-end)
                                  :encoding :utf-8))
                                (nonce-start (search "r=" client-first)))
                           (is nonce-start "SCRAM client-first did not contain a nonce.")
                           (let ((nonce (subseq client-first (+ nonce-start 2))))
                             (memory-transport-append-input
                              transport
                              (make-frame #\R
                                          (join-octets
                                           (octets 0 0 0 11)
                                           (cl-codec-kit:string-to-octets
                                            (format nil "r=~A,s=c2FsdA==,i=1" nonce)
                                            :encoding :utf-8)))))))))
                    (2
                     (memory-transport-append-input
                      transport
                      (make-frame #\R
                                  (join-octets
                                   (octets 0 0 0 12)
                                   (cl-codec-kit:string-to-octets
                                    "e=invalid-proof"
                                    :encoding :utf-8))))))))))
           (connection (make-connection
                        :transport transport
                        :user "alice"
                        :password "secret")))
      (transport-open transport)
      (setf (connection-open connection) t
            (connection-state connection) :ready)
      (unwind-protect
           (progn
             (assert-signals 'authentication-error
                             (lambda ()
                               (cl-postgresql-kit::%authenticate-scram connection)))
             (is (= 2 stage)))
        (disconnect connection)))))
