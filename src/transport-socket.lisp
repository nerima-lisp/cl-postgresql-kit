(in-package #:cl-postgresql-kit)

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
