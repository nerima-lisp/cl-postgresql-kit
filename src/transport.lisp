(in-package #:cl-postgresql-kit)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-bsd-sockets))

(defclass transport ()
  ((opened-p :accessor transport-opened-p :initform nil)
   (lock :initform (cl-concurrent-kit:make-lock :name "postgresql-transport"))))

(defgeneric transport-open (transport))
(defgeneric transport-close (transport))
(defgeneric transport-read-exactly (transport octet-count))
(defgeneric transport-write-all (transport octets))
(defgeneric transport-flush (transport))
(defgeneric transport-alive-p (transport))
(defgeneric transport-read-available (transport)
  (:documentation
   "Read bytes that are immediately available without waiting for more data."))
(defgeneric transport-wait-readable (transport &optional timeout)
  (:documentation
   "Wait until TRANSPORT has readable input, returning true on readiness."))
(defgeneric transport-start-tls (transport &key hostname verify))
(defgeneric transport-channel-binding-data (transport)
  (:documentation
   "Return TLS channel-binding data for TRANSPORT, or NIL when unavailable."))

(defparameter *socket-tls-option-keys*
  '(:alpn-protocols :certificate :key :password :cipher-list :method
    :verify-location))

(defun %transport-proper-list-p (value)
  (and (listp value)
       (let ((seen (make-hash-table :test #'eq))
             (tail value))
         (loop
           (cond ((null tail)
                  (return t))
                 ((not (consp tail))
                  (return nil))
                 ((gethash tail seen)
                  (return nil))
                 (t
                  (setf (gethash tail seen) t
                        tail (cdr tail))))))))

(defun %normalize-tls-verify-location (value)
  (labels ((normalize-location (location)
             (cond ((stringp location) location)
                   ((pathnamep location) (namestring location))
                   ((member location '(:default :default-file :default-dir)
                            :test #'eq)
                    location)
                   (t
                    (error 'parameter-error
                           :parameter location
                           :message
                           "TLS verify-location must be a string, pathname, or CL+SSL default keyword.")))))
    (cond ((null value) nil)
          ((%transport-proper-list-p value)
           (mapcar (lambda (location)
                     (unless (or (stringp location) (pathnamep location))
                       (error 'parameter-error
                              :parameter location
                              :message
                              "TLS verify-location lists must contain strings or pathnames."))
                     (normalize-location location))
                   value))
          (t
           (normalize-location value)))))

(defun %normalize-tls-option-value (key value)
  (case key
    (:alpn-protocols
    (unless (and (%transport-proper-list-p value) (every #'stringp value))
       (error 'parameter-error :parameter value
              :message "TLS ALPN protocols must be a list of strings."))
     (copy-list value))
    ((:certificate :key)
     (cond ((null value) nil)
           ((stringp value) value)
           ((pathnamep value) (namestring value))
           (t
            (error 'parameter-error :parameter value
                   :message "TLS certificate and key options must be strings or pathnames."))))
    ((:password :cipher-list)
     (unless (or (null value) (stringp value))
       (error 'parameter-error :parameter value
              :message "TLS password and cipher-list options must be strings."))
     value)
    (:verify-location
     (%normalize-tls-verify-location value))
    (:method value)))

(defun %normalize-tls-options (value)
  "Validate and copy TLS options accepted by the native CL+SSL transport.

Per-stream options are passed to CL+SSL's stream constructor.  VERIFY-LOCATION
is used by the optional TLS system to create a connection-local SSL context."
  (unless (%transport-proper-list-p value)
    (error 'parameter-error :parameter value
           :message "TLS options must be a property list."))
  (unless (evenp (length value))
    (error 'parameter-error :parameter value
           :message "TLS options must contain keyword/value pairs."))
  (let ((result nil)
        (seen nil))
    (loop for (key option-value) on value by #'cddr
          do (unless (and (keywordp key)
                          (member key *socket-tls-option-keys* :test #'eq))
               (error 'parameter-error :parameter key
                      :message "Unsupported TLS option."))
             (when (member key seen :test #'eq)
               (error 'parameter-error :parameter key
                      :message "TLS options must not contain duplicate keys."))
             (push key seen)
             (push key result)
             (push (%normalize-tls-option-value key option-value) result))
    (nreverse result)))

(defun %transport-octet-vector-p (value)
  (and (vectorp value)
       (every (lambda (octet)
                (and (integerp octet) (<= 0 octet 255)))
              value)))

(defun %transport-check-octets (value)
  (unless (%transport-octet-vector-p value)
    (error 'parameter-error :parameter value
           :message "Transport data must be a vector of octets"))
  value)

(defmethod transport-open ((transport transport))
  (setf (transport-opened-p transport) t)
  transport)

(defmethod transport-close ((transport transport))
  (setf (transport-opened-p transport) nil)
  transport)

(defmethod transport-flush ((transport transport))
  (declare (ignore transport))
  t)

(defmethod transport-alive-p ((transport transport))
  (transport-opened-p transport))

(defmethod transport-read-available ((transport transport))
  (declare (ignore transport))
  (make-array 0 :element-type '(unsigned-byte 8)))

(defmethod transport-wait-readable ((transport transport) &optional timeout)
  (declare (ignore transport timeout))
  nil)

(defmethod transport-start-tls ((transport transport) &key hostname verify)
  (declare (ignore transport hostname verify))
  (error 'unsupported-feature :feature :tls
         :message "TLS support is available through the optional cl-postgresql-kit/tls system"))

(defmethod transport-channel-binding-data ((transport transport))
  (declare (ignore transport))
  nil)
