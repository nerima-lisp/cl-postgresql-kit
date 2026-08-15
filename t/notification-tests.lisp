(in-package #:cl-postgresql-kit/test)

(deftest notification-queue-snapshot-and-pop
  (let ((connection (ready-memory-connection)))
    (unwind-protect
         (progn
           (cl-postgresql-kit::%handle-notification
            connection
            (join-octets (octets 0 0 0 1) (cstring "events") (cstring "created")))
           (is (= 1 (length (connection-notifications connection))))
           (is (equalp (poll-notification connection)
                       (list :pid 1 :channel "events" :payload "created")))
           (is (null (poll-notification connection))))
      (disconnect connection))))

(deftest notification-queue-limit-keeps-newest
  (let ((connection (ready-memory-connection :max-notifications 2)))
    (unwind-protect
         (flet ((payload (pid)
                  (join-octets (octets 0 0 0 pid)
                               (cstring "events") (cstring "created"))))
           (cl-postgresql-kit::%handle-notification connection (payload 1))
           (cl-postgresql-kit::%handle-notification connection (payload 2))
           (cl-postgresql-kit::%handle-notification connection (payload 3))
           (is (= 2 (connection-max-notifications connection)))
           (is (= 2 (length (connection-notifications connection))))
           (is (= 3 (getf (first (connection-notifications connection)) :pid)))
           (is (= 2 (getf (second (connection-notifications connection)) :pid)))
           (is (= 3 (getf (poll-notification connection) :pid)))
           (is (= 2 (getf (poll-notification connection) :pid)))
           (is (null (poll-notification connection))))
      (disconnect connection))))

(deftest notification-poll-reads-transport
  (let* ((input
           (join-octets
            (make-frame #\A
                        (join-octets (octets 0 0 0 1)
                                     (cstring "events") (cstring "created")))
            (make-frame #\C (cstring "SELECT 1"))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (progn
           (is (equalp (poll-notification connection)
                       (list :pid 1 :channel "events" :payload "created")))
           (is (null (poll-notification connection)))
           (let ((result (query connection "select 1")))
             (is (string= (result-command-tag result) "SELECT 1"))))
      (disconnect connection))))

(deftest notification-poll-preserves-partial-frame
  (let* ((input
           (make-frame #\A
                       (join-octets (octets 0 0 0 2)
                                    (cstring "events") (cstring "created"))))
         (split (floor (length input) 2))
         (connection (ready-memory-connection :input (subseq input 0 split)))
         (transport (connection-transport connection)))
    (unwind-protect
         (progn
           (is (null (poll-notification connection)))
           (memory-transport-append-input transport (subseq input split))
           (is (equalp (poll-notification connection)
                       (list :pid 2 :channel "events" :payload "created"))))
      (disconnect connection))))

(deftest notification-wait-uses-transport-readiness
  (let* ((input
           (make-frame #\A
                       (join-octets (octets 0 0 0 3)
                                    (cstring "events") (cstring "created"))))
         (connection (ready-memory-connection :input input))
         (transport (connection-transport connection)))
    (unwind-protect
         (progn
           (is (transport-wait-readable transport 0))
           (is (equalp (wait-for-notification connection 0)
                       (list :pid 3 :channel "events" :payload "created")))
           (is (not (transport-wait-readable transport 0)))
           (is (null (wait-for-notification connection 0))))
      (disconnect connection))))
