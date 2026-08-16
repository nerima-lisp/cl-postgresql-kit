(in-package #:cl-postgresql-kit)

(define-condition postgresql-condition (condition)
  ((message :initarg :message :reader postgresql-condition-message
            :initform nil))
  (:documentation "The common condition superclass for PostgreSQL client events."))

(define-condition postgresql-error (error postgresql-condition)
  ()
  (:report (lambda (condition stream)
             (format stream "~@[~A~]~@[ (~A)~]"
                     (postgresql-condition-message condition)
                     (type-of condition)))))

(define-condition protocol-error (postgresql-error)
  ((context :initarg :context :reader protocol-error-context :initform nil)
   (expected :initarg :expected :reader protocol-error-expected :initform nil)
   (actual :initarg :actual :reader protocol-error-actual :initform nil)))

(define-condition transport-error (postgresql-error)
  ((operation :initarg :operation :reader transport-error-operation :initform nil)
   (cause :initarg :cause :reader transport-error-cause :initform nil)))

(define-condition connection-error (postgresql-error) ())
(define-condition authentication-error (connection-error) ())
(define-condition oauth-discovery-required (authentication-error)
  ((response :initarg :response
             :reader oauth-discovery-response
             :initform nil)
   (server-fields :initarg :server-fields
                  :reader oauth-discovery-server-fields
                  :initform nil)))
(define-condition query-error (postgresql-error) ())
(define-condition multiple-results-error (query-error) ())
(define-condition timeout-error (postgresql-error)
  ((cancel-sent-p :initarg :cancel-sent-p
                  :reader timeout-error-cancel-sent-p
                  :initform nil)
   (connection-retired-p :initarg :connection-retired-p
                         :reader timeout-error-connection-retired-p
                         :initform nil)))
(define-condition tls-error (connection-error)
  ((cause :initarg :cause :reader tls-error-cause :initform nil)))
(define-condition pool-error (postgresql-error) ())
(define-condition pool-exhausted (pool-error)
  ((timeout :initarg :timeout :reader pool-exhausted-timeout :initform nil)))
(define-condition transaction-error (query-error) ())
(define-condition copy-error (query-error) ())
(define-condition unsupported-feature (postgresql-error)
  ((feature :initarg :feature :reader unsupported-feature-name :initform nil)))
(define-condition parameter-error (postgresql-error)
  ((parameter :initarg :parameter :reader parameter-error-parameter :initform nil)))

(define-condition server-error (query-error)
  ((fields :initarg :fields :reader server-error-fields :initform nil)
   (severity :initarg :severity :reader server-error-severity :initform nil)
   (sqlstate :initarg :sqlstate :reader server-error-sqlstate :initform nil)
   (detail :initarg :detail :reader server-error-detail :initform nil)
   (hint :initarg :hint :reader server-error-hint :initform nil)
   (position :initarg :position :reader server-error-position :initform nil)
   (where :initarg :where :reader server-error-where :initform nil)
   (schema :initarg :schema :reader server-error-schema :initform nil)
   (table :initarg :table :reader server-error-table :initform nil)
   (column :initarg :column :reader server-error-column :initform nil)
   (datatype :initarg :datatype :reader server-error-datatype :initform nil)
   (constraint :initarg :constraint :reader server-error-constraint :initform nil)
   (file :initarg :file :reader server-error-file :initform nil)
   (line :initarg :line :reader server-error-line :initform nil)
   (routine :initarg :routine :reader server-error-routine :initform nil)
   (unknown-fields :initarg :unknown-fields
                   :reader server-error-unknown-fields
                   :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ [~A]~]~@[ (~A)~]~@[~%  Detail: ~A~]~@[~%  Hint: ~A~]"
                     (or (server-error-message condition) "PostgreSQL server error")
                     (server-error-sqlstate condition)
                     (server-error-severity condition)
                     (server-error-detail condition)
                     (server-error-hint condition)))))

(defun server-error-message (condition)
  (%field (server-error-fields condition) :message))

(define-condition notice (postgresql-condition)
  ((fields :initarg :fields :reader notice-fields :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ [~A]~]"
                     (or (%field (notice-fields condition) :message)
                         "PostgreSQL notice")
                     (%field (notice-fields condition) :sqlstate)))))

(defun %field (fields key)
  (cdr (assoc key fields :test #'eq)))

(defun %server-error-from-fields (fields)
  (make-condition 'server-error
                  :message (%field fields :message)
                  :fields fields
                  :severity (or (%field fields :severity-localized)
                                (%field fields :severity))
                  :sqlstate (%field fields :sqlstate)
                  :detail (%field fields :detail)
                  :hint (%field fields :hint)
                  :position (%field fields :position)
                  :where (%field fields :where)
                  :schema (%field fields :schema)
                  :table (%field fields :table)
                  :column (%field fields :column)
                  :datatype (%field fields :datatype)
                  :constraint (%field fields :constraint)
                  :file (%field fields :file)
                  :line (%field fields :line)
                  :routine (%field fields :routine)
                  :unknown-fields (%field fields :unknown-fields)))

(defclass sql-null ()
  ()
  (:documentation "The explicit value used for a SQL NULL cell."))

(defparameter +sql-null+ (make-instance 'sql-null)
  "The singleton representation of a SQL NULL value.")

(defun sql-null-p (value)
  (typep value 'sql-null))
