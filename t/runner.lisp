(in-package #:cl-postgresql-kit/test)

(defun run-tests ()
  (let ((plan (cl-weave:list-tests
                :reporter :json
                :stream (make-string-output-stream)
                :timeout-ms 30000)))
    (let ((runnable-count (count :run plan
                                 :key #'cl-weave:test-plan-entry-status)))
      (unless (plusp runnable-count)
        (error "cl-weave discovered no runnable tests."))
      (format t "Discovered ~D runnable test(s).~%" runnable-count))
    (unless (cl-weave:run-all
              :reporter :spec
              :stream *standard-output*
              :pass-with-no-tests nil
              :timeout-ms 30000)
      (error "cl-weave reported a test failure.")))
  t)
