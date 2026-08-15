(in-package #:cl-postgresql-kit)

(defun large-object-create (connection &key (oid 0))
  "Create a PostgreSQL large object and return its OID.

The operation must run inside an active transaction.  OID zero asks the
  server to allocate an OID."
  (check-type connection connection)
  (%require-large-object-transaction connection)
  (%large-object-validate-oid oid)
  (%large-object-query-oid connection
                           "SELECT lo_create($1)"
                           (list oid)
                           '(26)))

(defun large-object-read-all (connection oid)
  "Read the complete contents of OID as an unsigned-byte vector.

The operation uses PostgreSQL's server-side LO_GET function and must run
  inside an active transaction."
  (check-type connection connection)
  (%require-large-object-transaction connection)
  (%large-object-validate-oid oid)
  (%large-object-query-octets connection
                              "SELECT lo_get($1)"
                              (list oid)
                              '(26)
                              "PostgreSQL lo_get returned a non-bytea value."))

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
  (%large-object-query-octets connection
                              "SELECT lo_get($1, $2, $3)"
                              (list oid offset length)
                              '(26 20 23)
                              "PostgreSQL lo_get returned a non-bytea value."))

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
  (%large-object-query-oid connection
                           "SELECT lo_from_bytea($1, $2)"
                           (list oid (make-typed-value octets 17 1))
                           '(26 17)))

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
  (%large-object-run-effect connection
                            "SELECT lo_put($1, $2, $3)"
                            (list oid offset (make-typed-value octets 17 1))
                            '(26 20 17)))

(defun large-object-import-server-file (connection path &key (oid 0))
  "Import PATH from the PostgreSQL server's filesystem and return its OID.

OID zero asks PostgreSQL to allocate an OID.  The operation must run inside
an active transaction and requires the server-side LO_IMPORT privilege."
  (check-type connection connection)
  (%require-large-object-transaction connection)
  (%large-object-validate-server-path path)
  (%large-object-validate-oid oid)
  (if (zerop oid)
      (%large-object-query-oid connection
                               "SELECT lo_import($1)"
                               (list path)
                               '(25))
      (%large-object-query-oid connection
                               "SELECT lo_import($1, $2)"
                               (list path oid)
                               '(25 26))))

(defun large-object-export-server-file (connection oid path)
  "Export OID to PATH on the PostgreSQL server's filesystem.

The operation must run inside an active transaction and requires the
  server-side LO_EXPORT privilege.  True is returned on success."
  (check-type connection connection)
  (%require-large-object-transaction connection)
  (%large-object-validate-oid oid)
  (%large-object-validate-server-path path)
  (%large-object-run-effect connection
                            "SELECT lo_export($1, $2)"
                            (list oid path)
                            '(26 25)))

(defun large-object-open (connection oid &key (mode :read))
  "Open OID and return a transaction-scoped LARGE-OBJECT handle."
  (check-type connection connection)
  (%require-large-object-transaction connection)
  (%large-object-validate-oid oid)
  (let* ((mode-value (%large-object-mode-value mode))
         (descriptor (%large-object-query-oid connection
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
             (%large-object-run-effect (large-object-connection object)
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
        (%large-object-query-octets
         (large-object-connection object)
         "SELECT loread($1, $2)"
         (list (large-object-descriptor object) length)
         '(23 23)
         "PostgreSQL loread returned a non-bytea value."))))

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
  (%large-object-run-effect connection
                            "SELECT lo_unlink($1)"
                            (list oid)
                            '(26)))
