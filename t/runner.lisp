(in-package #:cl-postgresql-kit/test)

(defun run-tests ()
  (unless (cl-weave:run-all
            :reporter :spec
            :stream *standard-output*
            :pass-with-no-tests nil
            :timeout-ms 30000)
    (error "cl-weave reported a test failure."))
  t)
