(defpackage #:cl-postgresql-kit/test
  (:use #:cl #:cl-postgresql-kit)
  (:shadow #:listen)
  (:export #:run-tests))
