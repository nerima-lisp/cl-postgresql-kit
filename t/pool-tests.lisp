(in-package #:cl-postgresql-kit/test)

(deftest pool-acquire-release-and-exhaustion
  (let ((pool (make-pool
               :max-size 1
               :connection-factory
               (lambda ()
                 (ready-memory-connection
                  :on-write
                  (lambda (transport octets)
                    (declare (ignore octets))
                    (append-ready-command transport "RESET")))))))
    (unwind-protect
         (let ((connection (pool-acquire pool)))
           (is (= (pool-size pool) 1))
           (is (= (pool-in-use-count pool) 1))
           (assert-signals 'pool-exhausted
                          (lambda () (pool-acquire pool :timeout 0)))
           (setf (connection-transaction-status connection) :in-transaction)
           (pool-release pool connection)
           (is (= (pool-available-count pool) 1))
           (is (eq (connection-transaction-status connection) :idle))
           (with-connection (borrowed pool)
             (is (connection-healthy-p borrowed))))
      (pool-close pool))
    (is (pool-closed-p pool))))

(deftest pool-validation-query-and-reap
  (let* ((factory-count 0)
        (query-count 0)
        (pool (make-pool
               :max-size 1
               :min-size 1
               :validation-query "SELECT 1"
               :connection-factory
               (lambda ()
                 (incf factory-count)
                 (ready-memory-connection
                  :on-write
                  (lambda (transport octets)
                    (when (and (plusp (length octets))
                               (= (aref octets 0) (char-code #\Q)))
                      (incf query-count))
                    (append-ready-command transport "OK")))))))
    (unwind-protect
         (progn
           (let ((connection (pool-acquire pool)))
             (is (= factory-count 1))
             (is (= query-count 1))
             (pool-release pool connection)
             (is (= query-count 2)))
           (is (= (pool-reap pool) 0))
           (let ((connection (pool-acquire pool)))
             (is (= factory-count 1))
             (is (= query-count 3))
             (pool-release pool connection)))
      (pool-close pool))))

(deftest pool-idle-and-age-limits-are-lazy
  (dolist (limit-key '(:max-idle-time :max-connection-age))
    (let* ((factory-count 0)
          (pool (apply #'make-pool
                       (list :max-size 1
                             :min-size 1
                             limit-key 0
                             :connection-factory
                             (lambda ()
                               (incf factory-count)
                               (ready-memory-connection
                                :on-write
                                (lambda (transport octets)
                                  (declare (ignore octets))
                                  (append-ready-command transport "RESET"))))))))
      (unwind-protect
           (progn
             (is (= (pool-reap pool) 1))
             (is (= (pool-available-count pool) 0))
             (let ((connection (pool-acquire pool)))
               (is (= factory-count 2))
               (pool-release pool connection)))
        (pool-close pool)))))

(deftest pool-refill-restores-minimum-after-reap
  (let* ((factory-count 0)
         (pool (make-pool
                :max-size 2
                :min-size 1
                :max-idle-time 0
                :connection-factory
                (lambda ()
                  (incf factory-count)
                  (ready-memory-connection
                   :on-write
                   (lambda (transport octets)
                     (declare (ignore octets))
                     (append-ready-command transport "RESET")))))))
    (unwind-protect
         (progn
           (is (= (pool-size pool) 1))
           (is (= (pool-reap pool) 1))
           (is (= (pool-size pool) 0))
           (is (= (pool-refill pool) 1))
           (is (= factory-count 2))
           (is (= (pool-size pool) 1))
           (is (= (pool-available-count pool) 1)))
      (pool-close pool))))

(deftest pool-release-is-single-owner
  (let ((pool (make-pool
               :max-size 1
               :connection-factory
               (lambda ()
                 (ready-memory-connection
                  :on-write
                  (lambda (transport octets)
                    (declare (ignore octets))
                    (append-ready-command transport "RESET")))))))
    (unwind-protect
         (let ((connection (pool-acquire pool)))
           (pool-release pool connection)
           (assert-signals 'pool-error
                          (lambda () (pool-release pool connection)))
           (is (= 1 (pool-size pool)))
           (is (= 1 (pool-available-count pool)))
           (is (= 0 (pool-in-use-count pool))))
      (pool-close pool))))
