(in-package #:cl-postgresql-kit)

(defclass memory-transport (transport)
  ((input :accessor %memory-transport-input
          :initarg :input
          :initform (make-array 0 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer 0))
   (position :accessor %memory-transport-position :initform 0)
   (output :accessor %memory-transport-output
           :initform (make-array 0 :element-type '(unsigned-byte 8)
                                 :adjustable t :fill-pointer 0))
   (on-write :accessor memory-transport-on-write :initarg :on-write
             :initform nil)
   (channel-binding-data :accessor %memory-transport-channel-binding-data
                         :initarg :channel-binding-data
                         :initform nil)))

(defun make-memory-transport (&key input on-write channel-binding-data)
  (when channel-binding-data
    (%transport-check-octets channel-binding-data))
  (let ((transport (make-instance 'memory-transport
                                  :input (make-array 0 :element-type '(unsigned-byte 8)
                                                     :adjustable t :fill-pointer 0)
                                  :on-write on-write
                                  :channel-binding-data channel-binding-data)))
    (when input (memory-transport-append-input transport input))
    transport))

(defmethod transport-channel-binding-data ((transport memory-transport))
  (%memory-transport-channel-binding-data transport))

(defun memory-transport-input (transport)
  (subseq (%memory-transport-input transport)
          (%memory-transport-position transport)))

(defun memory-transport-output (transport)
  (copy-seq (%memory-transport-output transport)))

(defun memory-transport-append-input (transport octets)
  (%transport-check-octets octets)
  (loop for octet across octets
        do (vector-push-extend octet (%memory-transport-input transport)))
  transport)

(defmethod transport-open ((transport memory-transport))
  (call-next-method))

(defmethod transport-close ((transport memory-transport))
  (call-next-method))

(defun %memory-transport-require-open (transport operation)
  (unless (transport-opened-p transport)
    (error 'transport-error :operation operation
           :message "Memory transport is not open.")))

(defmethod transport-read-available ((transport memory-transport))
  (%memory-transport-require-open transport :read)
  (let* ((input (%memory-transport-input transport))
         (position (%memory-transport-position transport))
         (result (subseq input position)))
    (setf (%memory-transport-position transport) (length input))
    result))

(defmethod transport-wait-readable ((transport memory-transport)
                                    &optional timeout)
  (declare (ignore timeout))
  (%memory-transport-require-open transport :read)
  (< (%memory-transport-position transport)
     (length (%memory-transport-input transport))))

(defmethod transport-read-exactly ((transport memory-transport) octet-count)
  (unless (and (integerp octet-count) (>= octet-count 0))
    (error 'parameter-error :parameter octet-count
           :message "Octet count must be a non-negative integer"))
  (%memory-transport-require-open transport :read)
  (let* ((input (%memory-transport-input transport))
         (position (%memory-transport-position transport))
         (end (+ position octet-count)))
    (when (> end (length input))
      (error 'transport-error :operation :read
             :message "Memory transport has insufficient input"
             :cause (list :requested octet-count
                          :available (- (length input) position))))
    (prog1 (subseq input position end)
      (setf (%memory-transport-position transport) end))))

(defmethod transport-write-all ((transport memory-transport) octets)
  (%transport-check-octets octets)
  (%memory-transport-require-open transport :write)
  (loop for octet across octets
        do (vector-push-extend octet (%memory-transport-output transport)))
  (when (memory-transport-on-write transport)
    (funcall (memory-transport-on-write transport) transport octets))
  octets)

(defmethod transport-flush ((transport memory-transport))
  (%memory-transport-require-open transport :flush)
  t)

(defmethod transport-alive-p ((transport memory-transport))
  (transport-opened-p transport))
