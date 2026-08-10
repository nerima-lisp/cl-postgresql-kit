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

(defun %large-object-live (object)
  (check-type object large-object)
  (when (large-object-closed-p object)
    (error 'transaction-error
           :message "The PostgreSQL large-object descriptor is closed."))
  (%require-large-object-transaction (large-object-connection object))
  (%large-object-validate-descriptor (large-object-descriptor object))
  object)

(defun large-object-create (connection &key (oid 0))
  "Create a PostgreSQL large object and return its OID.

The operation must run inside an active transaction.  OID zero asks the
server to allocate an OID."
  (check-type connection connection)
  (%require-large-object-transaction connection)
  (%large-object-validate-oid oid)
  (let ((result (%large-object-scalar-query connection
                                            "SELECT lo_create($1)"
                                            (list oid)
                                            '(26))))
    (%large-object-validate-oid result)))

(defun large-object-read-all (connection oid)
  "Read the complete contents of OID as an unsigned-byte vector.

The operation uses PostgreSQL's server-side LO_GET function and must run
inside an active transaction."
  (check-type connection connection)
  (%require-large-object-transaction connection)
  (%large-object-validate-oid oid)
  (let ((value (%large-object-scalar-query connection
                                            "SELECT lo_get($1)"
                                            (list oid)
                                            '(26))))
    (unless (%large-object-octets-p value)
      (error 'protocol-error
             :message "PostgreSQL lo_get returned a non-bytea value."
             :context :large-object
             :actual value))
    value))

(defun large-object-read-range (connection oid offset length)
  "Read LENGTH octets from OID starting at non-negative OFFSET.

The operation uses PostgreSQL's server-side LO_GET range form and must run
inside an active transaction."
  (check-type connection connection)
  (%require-large-object-transaction connection)
  (%large-object-validate-oid oid)
  (%large-object-validate-nonnegative-i64
   offset
   "Large-object range offset must be a non-negative signed 64-bit integer.")
  (unless (and (integerp length) (<= 0 length #x7fffffff))
    (error 'parameter-error :parameter length
           :message "Large-object range length must be a non-negative 31-bit integer."))
  (let ((value (%large-object-scalar-query connection
                                            "SELECT lo_get($1, $2, $3)"
                                            (list oid offset length)
                                            '(26 20 23))))
    (unless (%large-object-octets-p value)
      (error 'protocol-error
             :message "PostgreSQL lo_get returned a non-bytea value."
             :context :large-object
             :actual value))
    value))

(defun large-object-from-bytea (connection octets &key (oid 0))
  "Create a large object from OCTETS and return its OID.

OID zero asks PostgreSQL to allocate an OID.  The operation must run inside
an active transaction."
  (check-type connection connection)
  (%require-large-object-transaction connection)
  (%large-object-validate-oid oid)
  (unless (%large-object-octets-p octets)
    (error 'parameter-error
           :parameter octets
           :message "Large-object data must be an unsigned-byte vector."))
  (let ((result (%large-object-scalar-query connection
                                            "SELECT lo_from_bytea($1, $2)"
                                            (list oid (make-typed-value octets 17 1))
                                            '(26 17))))
    (%large-object-validate-oid result)))

(defun large-object-write-at (connection oid offset octets)
  "Write OCTETS to OID at non-negative OFFSET and return true.

The operation uses PostgreSQL's server-side LO_PUT function and must run
inside an active transaction."
  (check-type connection connection)
  (%require-large-object-transaction connection)
  (%large-object-validate-oid oid)
  (%large-object-validate-nonnegative-i64
   offset
   "Large-object write offset must be a non-negative signed 64-bit integer.")
  (unless (%large-object-octets-p octets)
    (error 'parameter-error
           :parameter octets
           :message "Large-object data must be an unsigned-byte vector."))
  (%large-object-scalar-query connection
                               "SELECT lo_put($1, $2, $3)"
                               (list oid offset (make-typed-value octets 17 1))
                               '(26 20 17))
  t)

(defun large-object-import-server-file (connection path &key (oid 0))
  "Import PATH from the PostgreSQL server's filesystem and return its OID.

OID zero asks PostgreSQL to allocate an OID.  The operation must run inside
an active transaction and requires the server-side LO_IMPORT privilege."
  (check-type connection connection)
  (%require-large-object-transaction connection)
  (%large-object-validate-server-path path)
  (%large-object-validate-oid oid)
  (let ((result (if (zerop oid)
                    (%large-object-scalar-query connection
                                                 "SELECT lo_import($1)"
                                                 (list path)
                                                 '(25))
                    (%large-object-scalar-query connection
                                                 "SELECT lo_import($1, $2)"
                                                 (list path oid)
                                                 '(25 26)))))
    (%large-object-validate-oid result)))

(defun large-object-export-server-file (connection oid path)
  "Export OID to PATH on the PostgreSQL server's filesystem.

The operation must run inside an active transaction and requires the
server-side LO_EXPORT privilege.  True is returned on success."
  (check-type connection connection)
  (%require-large-object-transaction connection)
  (%large-object-validate-oid oid)
  (%large-object-validate-server-path path)
  (%large-object-scalar-query connection
                               "SELECT lo_export($1, $2)"
                               (list oid path)
                               '(26 25))
  t)

(defun large-object-open (connection oid &key (mode :read))
  "Open OID and return a transaction-scoped LARGE-OBJECT handle."
  (check-type connection connection)
  (%require-large-object-transaction connection)
  (%large-object-validate-oid oid)
  (let* ((mode-value (%large-object-mode-value mode))
         (descriptor (%large-object-scalar-query connection
                                                  "SELECT lo_open($1, $2)"
                                                  (list oid mode-value)
                                                  '(26 23))))
    (make-instance 'large-object
                   :connection connection
                   :oid oid
                   :descriptor (%large-object-validate-descriptor descriptor)
                   :mode mode)))

(defun large-object-close (object)
  "Close OBJECT's descriptor and return true.

Closing is idempotent.  If the connection is already disconnected or its
transaction is no longer usable, the local handle is still invalidated."
  (check-type object large-object)
  (cl-concurrent-kit:with-lock-held ((%large-object-lock object))
    (unless (large-object-closed-p object)
      (unwind-protect
           (when (and (connection-open (large-object-connection object))
                      (eq (connection-transaction-status
                           (large-object-connection object))
                          :in-transaction))
             (%large-object-scalar-query (large-object-connection object)
                                          "SELECT lo_close($1)"
                                          (list (large-object-descriptor object))
                                          '(23)))
        (setf (large-object-closed-p object) t)))
    t))

(defun large-object-read (object length)
  "Read LENGTH octets from OBJECT and return an unsigned-byte vector."
  (unless (and (integerp length) (<= 0 length #x7fffffff))
    (error 'parameter-error :parameter length
           :message "Large-object read length must be a non-negative 31-bit integer."))
  (cl-concurrent-kit:with-lock-held ((%large-object-lock object))
    (%large-object-live object)
    (if (zerop length)
        (make-array 0 :element-type '(unsigned-byte 8))
        (let ((value (%large-object-scalar-query
                      (large-object-connection object)
                      "SELECT loread($1, $2)"
                      (list (large-object-descriptor object) length)
                      '(23 23))))
          (unless (%large-object-octets-p value)
            (error 'protocol-error
                   :message "PostgreSQL loread returned a non-bytea value."
                   :context :large-object
                   :actual value))
          value))))

(defun large-object-write (object octets)
  "Write OCTETS to OBJECT and return the number of octets written."
  (unless (%large-object-octets-p octets)
    (error 'parameter-error
           :parameter octets
           :message "Large-object write data must be an unsigned-byte vector."))
  (cl-concurrent-kit:with-lock-held ((%large-object-lock object))
    (%large-object-live object)
    (if (zerop (length octets))
        0
        (%large-object-scalar-query
         (large-object-connection object)
         "SELECT lowrite($1, $2)"
         (list (large-object-descriptor object)
               (make-typed-value octets 17 1))
         '(23 17)))))

(defun large-object-seek (object offset &key (whence :start))
  "Set OBJECT's position to OFFSET relative to WHENCE and return it."
  (%large-object-validate-i64
   offset
   "Large-object seek offset must be a signed 64-bit integer.")
  (cl-concurrent-kit:with-lock-held ((%large-object-lock object))
    (%large-object-live object)
    (%large-object-scalar-query
     (large-object-connection object)
     "SELECT lo_lseek64($1, $2, $3)"
     (list (large-object-descriptor object)
           offset
           (%large-object-whence-value whence))
     '(23 20 23))))

(defun large-object-tell (object)
  "Return OBJECT's current signed 64-bit position."
  (cl-concurrent-kit:with-lock-held ((%large-object-lock object))
    (%large-object-live object)
    (%large-object-scalar-query
     (large-object-connection object)
     "SELECT lo_tell64($1)"
     (list (large-object-descriptor object))
     '(23))))

(defun large-object-truncate (object length)
  "Truncate OBJECT to non-negative signed 64-bit LENGTH and return true."
  (%large-object-validate-nonnegative-i64
   length
   "Large-object truncate length must be a non-negative signed 64-bit integer.")
  (cl-concurrent-kit:with-lock-held ((%large-object-lock object))
    (%large-object-live object)
    (%large-object-scalar-query
     (large-object-connection object)
     "SELECT lo_truncate64($1, $2)"
     (list (large-object-descriptor object) length)
     '(23 20))
    t))

(defun large-object-unlink (connection oid)
  "Unlink OID from PostgreSQL and return true.

The operation must run inside an active transaction."
  (check-type connection connection)
  (%require-large-object-transaction connection)
  (%large-object-validate-oid oid)
  (%large-object-scalar-query connection
                               "SELECT lo_unlink($1)"
                               (list oid)
                               '(26))
  t)
