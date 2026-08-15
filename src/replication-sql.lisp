(in-package #:cl-postgresql-kit)

(defun %replication-quote-identifier (value)
  (check-type value string)
  (when (or (zerop (length value))
            (find #\Null value))
    (error 'parameter-error :parameter value
           :message "A replication identifier must be non-empty and must not contain NUL."))
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop for character across value
          do (when (char= character #\")
               (write-char #\" stream))
             (write-char character stream))
    (write-char #\" stream)))

(defun %replication-quote-literal (value)
  (check-type value string)
  (when (find #\Null value)
    (error 'parameter-error :parameter value
           :message "A replication SQL literal must not contain NUL."))
  (with-output-to-string (stream)
    (write-string "E'" stream)
    (loop for character across value
          do (case character
               (#\\
                (write-char #\\ stream)
                (write-char #\\ stream))
               (#\'
                (write-char #\\ stream)
                (write-char #\' stream))
               (#\Newline (write-string "\\n" stream))
               (#\Return (write-string "\\r" stream))
               (#\Tab (write-string "\\t" stream))
               (otherwise (write-char character stream))))
    (write-char #\' stream)))

(defun %replication-option-name (name)
  (let ((text (etypecase name
                (string name)
                (symbol (symbol-name name)))))
    (setf text (substitute #\_ #\- (string-upcase text)))
    (unless (and (plusp (length text))
                 (or (alpha-char-p (char text 0))
                     (char= (char text 0) #\_))
                 (loop for index from 1 below (length text)
                       always (or (alphanumericp (char text index))
                                  (char= (char text index) #\_))))
      (error 'parameter-error :parameter name
             :message "A replication option name must contain only letters, digits, underscores, and hyphens."))
    text))

(defun %replication-option-pair (option)
  (unless (consp option)
    (error 'parameter-error :parameter option
           :message "A replication option must be a cons or a one/two-element list."))
  (let ((tail (cdr option)))
    (cond ((null tail)
           (values (car option) nil))
          ((consp tail)
           (if (null (cddr option))
               (values (car option) (cadr option))
               (error 'parameter-error :parameter option
                      :message "A replication option list must contain at most two elements.")))
          (t
           (values (car option) tail)))))

(defun %replication-option-value-sql (value)
  (cond ((stringp value) (%replication-quote-literal value))
        ((integerp value) (format nil "~D" value))
        ((eq value t) "TRUE")
        ((null value) nil)
        (t (error 'parameter-error :parameter value
                  :message "Replication option values must be strings, integers, T, or NIL."))))

(defun %replication-option-sql (option)
  (multiple-value-bind (name value)
      (%replication-option-pair option)
    (let ((option-name (%replication-option-name name))
          (option-value (%replication-option-value-sql value)))
      (if option-value
          (format nil "~A ~A" option-name option-value)
          option-name))))

(defun %replication-option-sql-list (options)
  (unless (loop for tail = options then (cdr tail)
                while (consp tail)
                finally (return (null tail)))
    (error 'parameter-error :parameter options
           :message "Replication options must be a proper list."))
  (mapcar #'%replication-option-sql options))

(defun %replication-options-sql (options)
  (let ((option-sql (%replication-option-sql-list options)))
    (and option-sql
         (format nil "(~{~A~^, ~})" option-sql))))

(defun %replication-boolean-option-sql (name value)
  (unless (or (eq value t) (null value))
    (error 'parameter-error :parameter value
           :message "A replication slot boolean option must be T or NIL."))
  (format nil "~A ~:[FALSE~;TRUE~]"
          (%replication-option-name name)
          value))

(defun %replication-push-option-sql (option-sql sql)
  (if sql
      (cons sql option-sql)
      option-sql))

(defun %replication-push-boolean-option-sql
    (option-sql supplied-p name value)
  (if supplied-p
      (%replication-push-option-sql
       option-sql
       (%replication-boolean-option-sql name value))
      option-sql))

(defun %replication-push-base-backup-value-option-sql
    (option-sql name value validator)
  (if value
      (%replication-push-option-sql
       option-sql
       (%replication-base-backup-value-sql name (funcall validator value)))
      option-sql))

(defmacro %replication-collect-option-sql ((option-sql) &body clauses)
  (labels ((expand-clause (clause)
             (destructuring-bind (kind &rest arguments) clause
               (ecase kind
                 (:boolean
                  `(setf ,option-sql
                         (%replication-push-boolean-option-sql
                          ,option-sql ,@arguments)))
                 (:value
                  (destructuring-bind (name value validator) arguments
                    `(setf ,option-sql
                           (%replication-push-base-backup-value-option-sql
                            ,option-sql ,name ,value ,validator))))
                 (:push
                  (destructuring-bind (test form) arguments
                    `(when ,test
                       (push ,form ,option-sql))))
                 (:dolist
                  (destructuring-bind ((variable values) &body body) arguments
                    `(dolist (,variable ,values)
                       ,@body)))))))
    `(progn
       ,@(mapcar #'expand-clause clauses))))

(defun %replication-key-supplied-p (override-key-supplied-p override-value key-supplied-p)
  (if override-key-supplied-p
      override-value
      key-supplied-p))

(defun %replication-alter-slot-options-sql
    (two-phase-p two-phase-supplied-p failover-p failover-supplied-p options)
  (let ((option-sql nil))
    (%replication-collect-option-sql (option-sql)
      (:boolean two-phase-supplied-p "TWO_PHASE" two-phase-p)
      (:boolean failover-supplied-p "FAILOVER" failover-p)
      (:dolist (option (%replication-option-sql-list options))
        (push option option-sql)))
    (unless option-sql
      (error 'parameter-error :parameter options
             :message "ALTER_REPLICATION_SLOT requires at least one option."))
    (format nil "(~{~A~^, ~})" (nreverse option-sql))))
