(in-package #:cl-postgresql-kit)

(defclass large-object ()
  ((connection :initarg :connection :reader large-object-connection)
   (oid :initarg :oid :reader large-object-oid)
   (descriptor :initarg :descriptor :reader large-object-descriptor)
   (mode :initarg :mode :reader large-object-mode)
   (closed-p :initform nil :accessor large-object-closed-p)
   (lock :initform (cl-concurrent-kit:make-lock :name "postgresql-large-object")
         :reader %large-object-lock)))

(defun large-object-p (value)
  (typep value 'large-object))

(defun %large-object-octets-p (value)
  (and (vectorp value)
       (subtypep (array-element-type value) '(unsigned-byte 8))))

(defun %large-object-validate-oid (oid)
  (unless (and (integerp oid) (<= 0 oid #xffffffff))
    (error 'parameter-error
           :parameter oid
           :message "A PostgreSQL large-object OID must be an unsigned 32-bit integer."))
  oid)

(defun %large-object-validate-descriptor (descriptor)
  (unless (and (integerp descriptor) (<= 0 descriptor #x7fffffff))
    (error 'protocol-error
           :message "PostgreSQL returned an invalid large-object descriptor."
           :context :large-object
           :actual descriptor))
  descriptor)

(defun %large-object-validate-i64 (value message)
  (unless (and (integerp value)
               (<= (- (ash 1 63)) value (1- (ash 1 63))))
    (error 'parameter-error :parameter value :message message))
  value)

(defun %large-object-validate-nonnegative-i64 (value message)
  (%large-object-validate-i64 value message)
  (when (minusp value)
    (error 'parameter-error :parameter value :message message))
  value)

(defun %large-object-validate-server-path (path)
  (check-type path string)
  (when (position (code-char 0) path)
    (error 'parameter-error
           :parameter path
           :message "A server-side large-object path must not contain NUL."))
  path)

(defun %large-object-mode-value (mode)
  (let ((value (cond ((member mode '(:read :r) :test #'eq) #x40000)
                     ((member mode '(:write :w) :test #'eq) #x20000)
                     ((member mode '(:read-write :rw) :test #'eq) #x60000)
                     ((integerp mode) mode)
                     (t nil))))
    (unless (and value (<= 0 value #x7fffffff))
      (error 'parameter-error
             :parameter mode
             :message "Large-object mode must be :READ, :WRITE, :READ-WRITE, or a non-negative 31-bit integer."))
    value))

(defun %large-object-whence-value (whence)
  (let ((value (cond ((member whence '(:start :set) :test #'eq) 0)
                     ((eq whence :current) 1)
                     ((eq whence :end) 2)
                     ((integerp whence) whence)
                     (t nil))))
    (unless (and value (<= 0 value 2))
      (error 'parameter-error
             :parameter whence
             :message "Large-object seek origin must be :START, :CURRENT, :END, or 0, 1, or 2."))
    value))

(defun %require-large-object-transaction (connection)
  (%require-open-connection connection)
  (unless (eq (connection-transaction-status connection) :in-transaction)
    (error 'transaction-error
           :message "PostgreSQL large-object operations require an active transaction."))
  connection)

(defun %large-object-scalar-query (connection sql parameters parameter-type-oids)
  (let ((result (query connection sql
                        :parameters parameters
                        :parameter-type-oids parameter-type-oids
                        :result-formats '(1))))
    (unless (and (= 1 (length (result-columns result)))
                 (= 1 (result-row-count result)))
      (error 'protocol-error
             :message "PostgreSQL large-object function did not return one scalar row."
             :context :large-object
             :expected '(1 1)
             :actual (list (length (result-columns result))
                           (result-row-count result))))
    (row-value result 0 0)))

(defun %large-object-query-oid (connection sql parameters parameter-type-oids)
  (%large-object-validate-oid
   (%large-object-scalar-query connection sql parameters parameter-type-oids)))

(defun %large-object-query-octets (connection sql parameters parameter-type-oids
                                   protocol-error-message)
  (let ((value (%large-object-scalar-query connection sql parameters parameter-type-oids)))
    (unless (%large-object-octets-p value)
      (error 'protocol-error
             :message protocol-error-message
             :context :large-object
             :actual value))
    value))

(defun %large-object-run-effect (connection sql parameters parameter-type-oids)
  (%large-object-scalar-query connection sql parameters parameter-type-oids)
  t)

(defun %large-object-live (object)
  (check-type object large-object)
  (when (large-object-closed-p object)
    (error 'transaction-error
           :message "The PostgreSQL large-object descriptor is closed."))
  (%require-large-object-transaction (large-object-connection object))
  (%large-object-validate-descriptor (large-object-descriptor object))
  object)
