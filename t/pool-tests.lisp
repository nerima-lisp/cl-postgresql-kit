(in-package #:cl-postgresql-kit/test)

(defmacro with-test-pool ((variable &rest options) &body body)
  `(let ((,variable (make-pool ,@options)))
     (unwind-protect
          (progn ,@body)
       (pool-close ,variable))))

(defun make-reset-ready-connection ()
  (ready-memory-connection
   :on-write
   (lambda (transport octets)
     (declare (ignore octets))
     (append-ready-command transport "RESET"))))

(deftest pool-configuration-validates-bounds-and-factory
  (is-case-each ((condition-type arguments)
                 '((type-error (:max-size 0))
                   (type-error (:min-size -1))
                   (parameter-error (:max-idle-time -1))
                   (parameter-error (:max-connection-age -1))
                   (parameter-error (:validation-query ""))))
    (assert-signals condition-type
                    (lambda () (apply #'make-pool arguments))))
  (is (handler-case
          (progn (make-pool :connection-factory nil) nil)
        (parameter-error () t)))
  (assert-signals 'parameter-error
                 (lambda () (make-pool :max-size 1 :min-size 2)))
  (with-test-pool (pool :max-size 1 :connection-factory (lambda () nil))
    (pool-close pool)
    (assert-signals 'pool-error
                   (lambda () (pool-acquire pool :timeout 0)))))

(deftest pool-acquire-release-and-exhaustion
  (let (pool)
    (with-test-pool
        (instance
         :max-size 1
         :connection-factory
         #'make-reset-ready-connection)
      (setf pool instance)
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
          (is (connection-healthy-p borrowed)))))
    (is (pool-closed-p pool))))

(deftest pool-validation-query-and-reap
  (let ((factory-count 0)
        (query-count 0))
    (with-test-pool
        (pool
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
              (append-ready-command transport "OK")))))
      (let ((connection (pool-acquire pool)))
        (is (= factory-count 1))
        (is (= query-count 1))
        (pool-release pool connection)
        (is (= query-count 2)))
      (is (= (pool-reap pool) 0))
      (let ((connection (pool-acquire pool)))
        (is (= factory-count 1))
        (is (= query-count 3))
        (pool-release pool connection)))))

(deftest pool-idle-and-age-limits-are-lazy
  (is-each (limit-key '(:max-idle-time :max-connection-age))
    (let ((factory-count 0))
      (with-test-pool
          (pool
           :max-size 1
           :min-size 1
           limit-key 0
           :connection-factory
           (lambda ()
             (incf factory-count)
             (make-reset-ready-connection)))
        (is (= (pool-reap pool) 1))
        (is (= (pool-available-count pool) 0))
        (let ((connection (pool-acquire pool)))
          (is (= factory-count 2))
          (pool-release pool connection))))))

(deftest pool-refill-restores-minimum-after-reap
  (let ((factory-count 0))
    (with-test-pool
        (pool
         :max-size 2
         :min-size 1
         :max-idle-time 0
         :connection-factory
         (lambda ()
           (incf factory-count)
           (make-reset-ready-connection)))
      (is (= (pool-size pool) 1))
      (is (= (pool-reap pool) 1))
      (is (= (pool-size pool) 0))
      (is (= (pool-refill pool) 1))
      (is (= factory-count 2))
      (is (= (pool-size pool) 1))
      (is (= (pool-available-count pool) 1)))))

(deftest pool-release-is-single-owner
  (with-test-pool
      (pool
       :max-size 1
       :connection-factory
       #'make-reset-ready-connection)
    (let ((connection (pool-acquire pool)))
      (pool-release pool connection)
      (assert-signals 'pool-error
                     (lambda () (pool-release pool connection)))
      (is (= 1 (pool-size pool)))
      (is (= 1 (pool-available-count pool)))
      (is (= 0 (pool-in-use-count pool))))))
