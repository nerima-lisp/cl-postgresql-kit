(in-package #:cl-postgresql-kit/test)

(defmacro deftest (name &body body)
  (check-type name symbol)
  `(progn
     (cl-weave:it ,(string-capitalize
                    (substitute #\Space #\-
                                (string-downcase (symbol-name name))))
       ,@body)
     (quote ,name)))

(defun fail (format-control &rest arguments)
  (apply #'error format-control arguments))

(defmacro is (condition &optional (message nil message-p))
  (if message-p
      `(let ((actual ,condition))
         (if actual
             (cl-weave:expect actual :to-be-truthy)
             (error "~A" ,message)))
      `(cl-weave:expect ,condition)))

(defmacro is-each ((binding values) &body body)
  `(dolist (,binding ,values)
     ,@body))

(defmacro is-case-each ((bindings values) &body body)
  `(dolist (case ,values)
     (destructuring-bind ,bindings case
       ,@body)))

(defmacro is-funcall-results (function-designator cases
                                 &optional (predicate 'equal))
  `(is-case-each
       ((input expected) ,cases)
     (is (,predicate expected
                     (funcall ,function-designator input)))))

(defmacro assert-funcall-signals-each (condition-type cases
                                       function-designator)
  `(assert-signals-each ,condition-type ,cases
     (funcall ,function-designator value)))

(defmacro is-option-values (options-form &body expectations)
  (let ((options-var (gensym "OPTIONS")))
    `(let ((,options-var ,options-form))
       ,@(mapcar (lambda (expectation)
                   (destructuring-bind (key expected
                                        &optional (predicate 'equal))
                       expectation
                     `(is (,predicate ,expected
                                      (getf ,options-var ,key)))))
                 expectations))))

(defmacro is-connection-values (connection-form &body expectations)
  (let ((connection-var (gensym "CONNECTION")))
    `(let ((,connection-var ,connection-form))
       ,@(mapcar (lambda (expectation)
                   (destructuring-bind (accessor expected
                                        &optional (predicate 'equal))
                       expectation
                     `(is (,predicate ,expected
                                      (,accessor ,connection-var)))))
                 expectations))))

(defmacro is-alist-values (alist-form &body expectations)
  (let ((alist-var (gensym "ALIST")))
    `(let ((,alist-var ,alist-form))
       ,@(mapcar (lambda (expectation)
                   (destructuring-bind (key expected
                                        &optional (predicate 'equal) assoc-test)
                       expectation
                     `(is (,predicate ,expected
                                      (cdr (assoc ,key ,alist-var
                                                  ,@(when assoc-test
                                                      `(:test ,assoc-test))))))))
                 expectations))))

(defun assert-signals (condition-type thunk)
  (handler-case
      (progn
        (funcall thunk)
        (fail "Expected condition ~S was not signaled." condition-type))
    (condition (signaled-condition)
      (if (typep signaled-condition condition-type)
          signaled-condition
          (fail "Expected condition ~S, got ~S: ~A"
                condition-type
                (type-of signaled-condition)
                signaled-condition)))))

(defmacro with-signaled-condition ((condition-var condition-type) &body body)
  `(let ((,condition-var
           (assert-signals ,condition-type
                           (lambda ()
                             ,@body))))
     ,condition-var))

(defmacro assert-signals-each (condition-type values &body body)
  `(dolist (value ,values)
     (assert-signals ,condition-type
                     (lambda ()
                       ,@body))))

(defmacro it-signals-each (condition-type cases description bindings &body body)
  `(cl-weave:it-each
       ,cases
     ,description
     ,bindings
     (assert-signals ,condition-type
                     (lambda ()
                       ,@body))))

(defmacro it-binary-round-trips-each ((registry cases) description bindings)
  `(cl-weave:it-each
       ,cases
     ,description
     ,bindings
     (let ((decoded (decode-value ,registry oid wire :format 1)))
       (is (funcall predicate expected decoded))
       (is (equalp wire
                   (encode-value ,registry oid decoded :format 1))))))

(defmacro with-selected-default-codecs ((registry-var oids) &body body)
  "Bind REGISTRY-VAR to a registry populated with the selected default codecs."
  `(let* ((defaults (default-type-registry))
          (,registry-var (make-type-registry :include-defaults nil)))
     (dolist (oid ,oids)
       (register-type ,registry-var
                      :codec (find-type-codec defaults oid)))
     ,@body))
