;;;; run-tests.lisp

(require :asdf)

(defun script-directory ()
  (make-pathname :name nil
                 :type nil
                 :defaults (or *load-truename*
                               *compile-file-truename*
                               (error "run-tests.lisp has no source pathname."))))

(let ((root (script-directory)))
  (pushnew root asdf:*central-registry* :test #'equal)
  (asdf:load-asd (merge-pathnames "cl-postgresql-kit.asd" root))
  (unless (asdf:test-system "cl-postgresql-kit/test")
    (uiop:quit 1)))

(uiop:quit 0)
