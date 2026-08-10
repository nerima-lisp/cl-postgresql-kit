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
  "Validate and copy TLS options accepted by the native CL+SSL adapter.

Per-stream options are passed to CL+SSL's stream constructor.  VERIFY-LOCATION
is used by the optional adapter to create a connection-local SSL context."
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
             (setf result
                   (append result
                           (list key
                                 (%normalize-tls-option-value key option-value)))))
    result))

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

(defclass socket-transport (transport)
  ((host :reader socket-transport-host :initarg :host :initform "127.0.0.1")
   (port :reader socket-transport-port :initarg :port :initform 5432)
   (timeout :reader socket-transport-timeout :initarg :timeout :initform 30)
   (tls-options :reader socket-transport-tls-options
                :initarg :tls-options
                :initform nil)
   (socket :accessor socket-transport-socket :initform nil)
   (stream :accessor socket-transport-stream :initform nil)))

(defun make-socket-transport (&key (host "127.0.0.1") (port 5432) (timeout 30)
                                   tls-options)
  (make-instance 'socket-transport
                 :host host
                 :port port
                 :timeout timeout
                 :tls-options (%normalize-tls-options tls-options)))

(defun %clear-transport-tls-options (transport)
  (when (typep transport 'socket-transport)
    (setf (slot-value transport 'tls-options) nil))
  transport)

#+sbcl
(defmethod transport-open ((transport socket-transport))
  (when (transport-alive-p transport)
    (return-from transport-open transport))
  (handler-case
      (let ((socket nil)
            (stream nil)
            (completed-p nil))
        (unwind-protect
             (progn
               (require :sb-bsd-sockets)
               (multiple-value-bind (ipv4-host-ent ipv6-host-ent)
                   (sb-bsd-sockets:get-host-by-name
                    (socket-transport-host transport))
                 (let* ((ipv4-address
                          (and ipv4-host-ent
                               (sb-bsd-sockets:host-ent-address ipv4-host-ent)))
                        (ipv6-address
                          (and ipv6-host-ent
                               (sb-bsd-sockets:host-ent-address ipv6-host-ent)))
                        ;; SBCL returns the IPv6 HOST-ENT as a second value.
                        ;; Keep IPv4 as the first choice for existing hostnames,
                        ;; while allowing an IPv6 literal or IPv6-only hostname.
                        (address (or ipv4-address ipv6-address)))
                   (unless address
                     (error "The host resolved without a usable address."))
                   (setf socket
                         (make-instance
                          (if (= (length address) 16)
                              'sb-bsd-sockets:inet6-socket
                              'sb-bsd-sockets:inet-socket)
                          :type :stream :protocol :tcp))
                   (if (socket-transport-timeout transport)
                       (cl-concurrent-kit:with-timeout
                           (cl-date-kit:duration-of-nanos
                            (round (* (socket-transport-timeout transport)
                                      1000000000)))
                         (sb-bsd-sockets:socket-connect
                          socket address (socket-transport-port transport)))
                       (sb-bsd-sockets:socket-connect
                        socket address (socket-transport-port transport)))))
               (setf stream
                     (sb-bsd-sockets:socket-make-stream
                      socket :input t :output t
                      :element-type '(unsigned-byte 8)
                      :timeout (socket-transport-timeout transport)))
               (setf (socket-transport-socket transport) socket
                     (socket-transport-stream transport) stream)
               (prog1 (call-next-method)
                 (setf completed-p t)))
          (unless completed-p
            (setf (socket-transport-stream transport) nil
                  (socket-transport-socket transport) nil)
            (ignore-errors (when stream (close stream)))
            (ignore-errors (when socket (sb-bsd-sockets:socket-close socket))))))
    (error (condition)
      (ignore-errors (transport-close transport))
      (error 'transport-error :operation :open :cause condition
             :message "Unable to open PostgreSQL socket"))))

#-sbcl
(defmethod transport-open ((transport socket-transport))
  (declare (ignore transport))
  (error 'unsupported-feature :feature :socket
         :message "The native socket transport currently requires SBCL"))

(defmethod transport-close ((transport socket-transport))
  (let ((stream (socket-transport-stream transport))
        (socket (socket-transport-socket transport)))
    (setf (socket-transport-stream transport) nil
          (socket-transport-socket transport) nil)
    (ignore-errors (when stream (close stream)))
    #+sbcl (ignore-errors (when socket (sb-bsd-sockets:socket-close socket)))
    (call-next-method)))

(defmethod transport-read-exactly ((transport socket-transport) octet-count)
  (unless (and (integerp octet-count) (>= octet-count 0))
    (error 'parameter-error :parameter octet-count
           :message "Octet count must be a non-negative integer"))
  (let ((stream (socket-transport-stream transport))
        (position 0))
    (unless stream
      (error 'transport-error :operation :read
             :message "PostgreSQL socket is not open"))
    (let ((result (make-array octet-count :element-type '(unsigned-byte 8))))
      (handler-case
          (loop while (< position octet-count)
                do (let ((end (read-sequence result stream :start position)))
                     (if (= end position)
                         (error 'transport-error :operation :read
                                :message "PostgreSQL socket reached EOF")
                         (setf position end))))
        (postgresql-error (condition) (error condition))
        (error (condition)
          (error 'transport-error :operation :read :cause condition
                 :message "Unable to read PostgreSQL socket")))
      result)))

#+sbcl
(defmethod transport-read-available ((transport socket-transport))
  (let ((stream (socket-transport-stream transport))
        (result (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0)))
    (unless stream
      (error 'transport-error :operation :read
             :message "PostgreSQL socket is not open"))
    (handler-case
        (loop while (cl:listen stream)
              do (let ((octet (read-byte stream nil nil)))
                   (if octet
                       (vector-push-extend octet result)
                       (return))))
      (postgresql-error (condition) (error condition))
      (error (condition)
        (error 'transport-error :operation :read :cause condition
               :message "Unable to read available PostgreSQL socket data")))
    result))

#+sbcl
(defmethod transport-wait-readable ((transport socket-transport)
                                     &optional timeout)
  (let ((socket (socket-transport-socket transport))
        (stream (socket-transport-stream transport)))
    (unless (and socket stream)
      (error 'transport-error :operation :read
             :message "PostgreSQL socket is not open"))
    (handler-case
        (or (cl:listen stream)
            (sb-sys:wait-until-fd-usable
             (sb-bsd-sockets:socket-file-descriptor socket)
             :input
             timeout))
      (postgresql-error (condition) (error condition))
      (error (condition)
        (error 'transport-error :operation :read :cause condition
               :message "Unable to wait for PostgreSQL socket data")))))

#-sbcl
(defmethod transport-wait-readable ((transport socket-transport)
                                     &optional timeout)
  (declare (ignore transport timeout))
  (error 'unsupported-feature :feature :socket-wait
         :message "Socket readiness waiting currently requires SBCL"))

(defmethod transport-write-all ((transport socket-transport) octets)
  (%transport-check-octets octets)
  (let ((stream (socket-transport-stream transport))
        (position 0))
    (unless stream
      (error 'transport-error :operation :write
             :message "PostgreSQL socket is not open"))
    (handler-case
        (loop while (< position (length octets))
              do (setf position (write-sequence octets stream :start position)))
      (postgresql-error (condition) (error condition))
      (error (condition)
        (error 'transport-error :operation :write :cause condition
               :message "Unable to write PostgreSQL socket")))
    octets))

(defmethod transport-flush ((transport socket-transport))
  (let ((stream (socket-transport-stream transport)))
    (when stream (force-output stream)))
  t)

(defmethod transport-alive-p ((transport socket-transport))
  (and (transport-opened-p transport)
       (not (null (socket-transport-stream transport)))))
