(in-package #:cl-postgresql-kit)

(defclass type-codec ()
  ((oid :reader type-codec-oid :initarg :oid)
   (name :reader type-codec-name :initarg :name)
   (text-decoder :reader type-codec-text-decoder :initarg :text-decoder :initform nil)
   (text-encoder :reader type-codec-text-encoder :initarg :text-encoder :initform nil)
   (binary-decoder :reader type-codec-binary-decoder :initarg :binary-decoder :initform nil)
   (binary-encoder :reader type-codec-binary-encoder :initarg :binary-encoder :initform nil)))

(defun make-type-codec (&rest arguments)
  (apply #'make-instance 'type-codec arguments))

(defstruct (type-registry (:constructor %make-type-registry))
  (by-oid (make-hash-table :test #'eql))
  (by-name (make-hash-table :test #'equalp))
  (lock (cl-concurrent-kit:make-lock :name "postgresql-type-registry")))

(defun make-type-registry (&key (include-defaults t))
  (let ((registry (%make-type-registry)))
    (when include-defaults
      (%register-built-in-types registry))
    registry))

(defstruct (typed-value (:constructor make-typed-value
                                          (value type-oid &optional (format 0))))
  value type-oid (format 0))

(defstruct json-value value)

(defstruct bytea-value value)

(defstruct date-value value)

(defstruct time-value value)

(defstruct timestamp-value value)

(defstruct interval-value value)

(defstruct timetz-value value)

(defstruct (postgres-interval
            (:constructor %make-postgres-interval
                (&key months days microseconds)))
  (months 0)
  (days 0)
  (microseconds 0))

(defstruct (postgres-time-with-time-zone
            (:constructor %make-postgres-time-with-time-zone
                (&key microseconds timezone-seconds)))
  (microseconds 0)
  (timezone-seconds 0))

(defstruct (postgres-tid
            (:constructor %make-postgres-tid
                (&key block-number offset-number)))
  (block-number 0)
  (offset-number 0))

(defconstant +postgres-microseconds-per-day+ (* 86400 1000000))

(defconstant +postgres-timetz-zone-limit+ (* 16 60 60))

(defstruct uuid-value value)

(defstruct (postgres-bit-string
            (:constructor make-postgres-bit-string
                (&key (bits "") (varying-p nil))))
  (bits "")
  (varying-p nil))

(defstruct (postgres-mac-address
            (:constructor make-postgres-mac-address
                (&key (octets #()))))
  (octets #()))

(defstruct (postgres-inet
            (:constructor %make-postgres-inet
                (&key octets family netmask cidr-p)))
  (octets #())
  family
  netmask
  (cidr-p nil))

(defstruct (postgres-array
            (:constructor make-postgres-array
                (&key elements dimensions lower-bounds element-oid)))
  (elements #())
  (dimensions #())
  (lower-bounds #())
  element-oid)

(defstruct (postgres-range
            (:constructor %make-postgres-range
                (&key lower upper lower-inclusive upper-inclusive empty-p)))
  lower
  upper
  (lower-inclusive nil)
  (upper-inclusive nil)
  (empty-p nil))

(defstruct (postgres-multirange
            (:constructor %make-postgres-multirange (&key ranges)))
  (ranges #()))

(defstruct (postgres-composite
            (:constructor make-postgres-composite
                (&key fields field-names field-oids)))
  (fields #())
  (field-names #())
  (field-oids #()))
