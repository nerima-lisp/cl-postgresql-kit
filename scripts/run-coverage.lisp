;;;; scripts/run-coverage.lisp

(require :asdf)
(require :sb-cover)

(defun %coverage-script-directory ()
  (make-pathname :name nil
                 :type nil
                 :defaults (or *load-truename*
                               *compile-file-truename*
                               (error "run-coverage.lisp has no source pathname."))))

(defun %coverage-project-directory ()
  (merge-pathnames "../" (%coverage-script-directory)))

(defun %coverage-policy-symbol ()
  (or (find-symbol "STORE-COVERAGE-DATA" "SB-COVER")
      (error "SBCL does not expose SB-COVER:STORE-COVERAGE-DATA.")))

(defun %required-function-symbol (package-name symbol-name)
  (let ((symbol (find-symbol symbol-name package-name)))
    (unless (and symbol (fboundp symbol))
      (error "Package ~A does not provide function ~A."
             package-name symbol-name))
    symbol))

(defun %coverage-threshold (environment-variable)
  (let ((raw-value (uiop:getenv environment-variable)))
    (when raw-value
      (let ((threshold
              (handler-case
                  (parse-integer
                   (string-trim '(#\Space #\Tab #\Newline #\Return)
                                raw-value)
                   :junk-allowed nil)
                (error () nil))))
        (unless (and (integerp threshold)
                     (<= 0 threshold 100))
          (error "~A must be an integer between 0 and 100, got ~S."
                 environment-variable raw-value))
        threshold))))

(let* ((root (%coverage-project-directory))
       (asd (merge-pathnames "cl-postgresql-kit.asd" root))
       (coverage-root
         (uiop:ensure-directory-pathname
          (or (uiop:getenv "PGKIT_COVERAGE_DIR")
              (merge-pathnames "coverage/" root))))
       (coverage-output
         (merge-pathnames "cl-postgresql-kit.coverage" coverage-root))
       (coverage-report-directory
         (merge-pathnames "report/" coverage-root))
       (minimum-expression
         (%coverage-threshold "PGKIT_COVERAGE_MINIMUM_EXPRESSION"))
       (minimum-branch
         (%coverage-threshold "PGKIT_COVERAGE_MINIMUM_BRANCH")))
  (pushnew root asdf:*central-registry* :test #'equal)
  (asdf:load-asd asd)
  (proclaim `(optimize (,(%coverage-policy-symbol) 3)))
  (asdf:load-system "cl-postgresql-kit" :force t)
  (asdf:load-system "cl-postgresql-kit/test" :force t)
  (ensure-directories-exist coverage-output)
  (ensure-directories-exist
   (merge-pathnames "cover-index.html" coverage-report-directory))
  (let* ((run-all (%required-function-symbol "CL-WEAVE" "RUN-ALL"))
         (coverage-statistics
           (%required-function-symbol "CL-WEAVE" "COVERAGE-STATISTICS"))
         (source-directory
           (merge-pathnames "src/"
                            (asdf:system-source-directory "cl-postgresql-kit")))
         (excluded-source-pathnames
           (list (merge-pathnames "package.lisp" source-directory)))
         (passed
           (funcall run-all
            :reporter :spec
            :stream *standard-output*
            :pass-with-no-tests nil
            :timeout-ms 30000
            :coverage t
            :coverage-output coverage-output
            :coverage-report-directory coverage-report-directory
            :coverage-include-pathnames (list source-directory)
            :coverage-exclude-pathnames excluded-source-pathnames
            :coverage-minimum-expression minimum-expression
            :coverage-minimum-branch minimum-branch))
         (statistics
           (funcall coverage-statistics
            :include-pathnames (list source-directory)
            :exclude-pathnames excluded-source-pathnames))
         (expression-covered (getf statistics :expression-covered))
         (expression-total (getf statistics :expression-total))
         (branch-covered (getf statistics :branch-covered))
         (branch-total (getf statistics :branch-total)))
    (unless passed
      (error "cl-weave reported a test failure while collecting coverage."))
    (unless (plusp expression-total)
      (error "Coverage selected no source expressions under ~A."
             source-directory))
    (format t "COVERAGE-EXPRESSIONS ~D/~D (~,2F%%)~%"
            expression-covered expression-total
            (* 100.0 (/ expression-covered expression-total)))
    (format t "COVERAGE-BRANCHES ~D/~D (~,2F%%)~%"
            branch-covered branch-total
            (if (zerop branch-total)
                100.0
                (* 100.0 (/ branch-covered branch-total))))
    (format t "COVERAGE-MINIMUM-EXPRESSIONS ~A~%"
            (or minimum-expression "disabled"))
    (format t "COVERAGE-MINIMUM-BRANCHES ~A~%"
            (or minimum-branch "disabled"))
    (format t "COVERAGE-OUTPUT ~A~%" coverage-output)
    (format t "COVERAGE-REPORT ~A~%" coverage-report-directory)))
