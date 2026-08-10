(in-package #:cl-postgresql-kit/test)

(defmacro deftest (name &body body)
  (check-type name symbol)
  `(progn
     (cl-weave:it ,(string-capitalize
                    (substitute #\Space #\-
                                (string-downcase (symbol-name name))))
       ,@body)
     (quote ,name)))

(defun fail (format-control &rest arguments)
  (apply #'error format-control arguments))

(defmacro is (condition &optional (message nil message-p))
  (if message-p
      `(let ((actual ,condition))
         (if actual
             (cl-weave:expect actual :to-be-truthy)
             (error "~A" ,message)))
      `(cl-weave:expect ,condition)))

(defun assert-signals (condition-type thunk)
  (handler-case
      (progn
        (funcall thunk)
        (fail "Expected condition ~S was not signaled." condition-type))
    (condition (signaled-condition)
      (if (typep signaled-condition condition-type)
          t
          (fail "Expected condition ~S, got ~S."
                condition-type
                (type-of signaled-condition))))))

(defun octets (&rest values)
  (make-array (length values)
              :element-type '(unsigned-byte 8)
              :initial-contents values))

(defun octets-as-hex (value)
  (with-output-to-string (stream)
    (loop for octet across value
          do (format stream "~2,'0X" octet))))

(defun cstring (value)
  (let* ((payload (cl-codec-kit:string-to-octets value :encoding :utf-8))
         (result (make-array (1+ (length payload))
                             :element-type '(unsigned-byte 8))))
    (replace result payload)
    (setf (aref result (length payload)) 0)
    result))

(defun join-octets (&rest sequences)
  (apply #'concatenate '(vector (unsigned-byte 8)) sequences))

(defun ready-memory-connection (&key input on-write password
                                      oauth-token-provider
                                      gss-token-provider
                                      sspi-token-provider
                                      cancel-transport-factory
                                      (max-notifications 10000)
                                      query-timeout
                                      metric-registry
                                      (ssl-mode :disable))
  (let ((transport (make-memory-transport :input input :on-write on-write)))
    (transport-open transport)
    (let ((connection (make-connection :transport transport
                                       :ssl-mode ssl-mode
                                       :password password
                                       :oauth-token-provider
                                       oauth-token-provider
                                       :gss-token-provider
                                       gss-token-provider
                                       :sspi-token-provider
                                       sspi-token-provider
                                       :query-timeout query-timeout
                                       :metric-registry metric-registry
                                       :max-notifications max-notifications
                                       :cancel-transport-factory
                                       cancel-transport-factory)))
      (setf (connection-open connection) t
            (connection-state connection) :ready)
      connection)))

(defclass tls-probe-transport (memory-transport)
  ((tls-options :accessor tls-probe-options :initform nil)))

(defmethod transport-start-tls ((transport tls-probe-transport)
                                &key hostname verify)
  (setf (tls-probe-options transport)
        (list :hostname hostname :verify verify))
  transport)

(defun copy-response-payload ()
  (octets 0 0 1 0 0))

(defun append-ready-command (transport tag)
  (memory-transport-append-input
   transport
   (join-octets (make-frame #\C (cstring tag))
                (make-frame #\Z (octets (char-code #\I))))))

(defun append-startup-parameter (transport name value)
  (memory-transport-append-input
   transport
   (make-frame #\S (join-octets (cstring name) (cstring value))))
  (append-ready-command transport "STARTUP"))

(defun make-target-session-transport (read-only-p)
  (make-memory-transport
   :on-write (lambda (transport octets)
               (declare (ignore octets))
               (append-startup-parameter
                transport
                "transaction_read_only"
                (if read-only-p "on" "off")))))

(defun large-object-scalar-result-input (type-oid value tag)
  (let ((description (make-octet-builder))
        (row (make-octet-builder)))
    (append-u16 description 1)
    (append-cstring description "value")
    (append-u32 description 0)
    (append-i16 description 0)
    (append-u32 description type-oid)
    (append-i16 description (if (= type-oid 17) -1 4))
    (append-i32 description -1)
    (append-i16 description 1)
    (append-u16 row 1)
    (append-i32 row (length value))
    (append-octets row value)
    (join-octets
     (make-frame #\1 #())
     (make-frame #\2 #())
     (make-frame #\T (builder-octets description))
     (make-frame #\D (builder-octets row))
     (make-frame #\C (cstring tag))
     (make-frame #\Z (octets (char-code #\T))))))

(defun catalog-query-input (columns rows &optional (tag "SELECT 1"))
  (labels ((wire-value (value)
             (cond ((stringp value)
                    (cl-codec-kit:string-to-octets value :encoding :utf-8))
                   ((integerp value)
                    (cl-codec-kit:string-to-octets (format nil "~D" value)
                                                   :encoding :utf-8))
                   ((and (vectorp value)
                         (equalp (array-element-type value)
                                 '(unsigned-byte 8)))
                    value)
                   (t
                    (error "Unsupported catalog test value: ~S" value))))
           (row-frame (row)
             (let ((payload (make-octet-builder)))
               (append-u16 payload (length columns))
               (dolist (value row)
                 (if (null value)
                     (append-i32 payload -1)
                     (let ((wire (wire-value value)))
                       (append-i32 payload (length wire))
                       (append-octets payload wire))))
               (make-frame #\D (builder-octets payload)))))
    (let ((description (make-octet-builder)))
      (append-u16 description (length columns))
      (dolist (column columns)
        (destructuring-bind (name type-oid type-size) column
          (append-cstring description name)
          (append-u32 description 0)
          (append-i16 description 0)
          (append-u32 description type-oid)
          (append-i16 description type-size)
          (append-i32 description -1)
          (append-i16 description 0)))
      (apply #'join-octets
             (append (list (make-frame #\T (builder-octets description)))
                     (mapcar #'row-frame rows)
                     (list (make-frame #\C (cstring tag))
                           (make-frame #\Z (octets (char-code #\I)))))))))
