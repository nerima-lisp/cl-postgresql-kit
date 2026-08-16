(in-package #:cl-postgresql-kit)

(defconstant +protocol-version-3.0+ #x00030000
  "PostgreSQL protocol version 3.0.")

(defconstant +protocol-version-3.2+ #x00030002
  "PostgreSQL protocol version 3.2.")

(defun %supported-protocol-version-p (protocol-version)
  (member protocol-version
          (list +protocol-version-3.0+ +protocol-version-3.2+)
          :test #'=))

(defun %check-protocol-version (protocol-version)
  (unless (and (integerp protocol-version)
               (%supported-protocol-version-p protocol-version))
    (error 'parameter-error
           :parameter protocol-version
           :message "Only PostgreSQL protocol versions 3.0 and 3.2 are supported."))
  protocol-version)

(defun %protocol-builder (&optional (size 128))
  (make-octet-builder size))

(defun %protocol-frame (type builder)
  (make-frame type (builder-octets builder)))

(defun %protocol-string-octets (value)
  (cond ((null value) #())
        ((stringp value) (cl-codec-kit:string-to-octets value :encoding :utf-8))
        ((vectorp value)
         (%wire-check-octets value))
        ((listp value)
         (handler-case
             (%wire-check-octets
              (coerce value '(vector (unsigned-byte 8))))
           (type-error ()
             (error 'parameter-error
                    :parameter value
                    :message "PostgreSQL wire data must be a string or a sequence of octets"))))
        (t
         (error 'parameter-error
                :parameter value
                :message "PostgreSQL wire data must be a string or a sequence of octets"))))

(defun %ensure-payload-end (payload position context)
  (unless (= position (length payload))
    (error 'protocol-error
           :message "PostgreSQL payload contains trailing bytes"
           :context context
           :expected (length payload)
           :actual position))
  position)

(defun %ensure-count-capacity (payload position count minimum context)
  (let ((remaining (- (length payload) position)))
    (unless (and (integerp count)
                 (>= count 0)
                 (>= remaining 0)
                 (<= (* count minimum) remaining))
      (error 'protocol-error
             :message "PostgreSQL message count exceeds payload capacity"
             :context context
             :expected (floor (max 0 remaining) minimum)
             :actual count))))

(defun %protocol-format-code (value)
  (unless (and (integerp value) (member value '(0 1)))
    (error 'parameter-error :parameter value
           :message "PostgreSQL format codes must be 0 (text) or 1 (binary)"))
  value)

(defun %protocol-format-list (value count)
  (let ((values (cond ((null value) nil)
                      ((listp value) value)
                      ((vectorp value) (coerce value 'list))
                      (t (list value)))))
    (cond ((zerop count) nil)
          ((null values) nil)
          ((= (length values) 1)
           (make-list count :initial-element (%protocol-format-code (first values))))
          ((= (length values) count)
           (mapcar #'%protocol-format-code values))
          (t (error 'parameter-error :parameter values
                    :message "The PostgreSQL format-code count does not match")))))

(defun encode-startup-message (&key user database application-name parameters
                                    (protocol-version +protocol-version-3.0+))
  "Encode a PostgreSQL startup packet without a type byte."
  (%check-protocol-version protocol-version)
  (let ((builder (%protocol-builder)))
    (append-i32 builder 0)
    (append-i32 builder protocol-version)
    (dolist (pair (append (remove nil
                                  (list (and user (cons "user" user))
                                        (and database (cons "database" database))
                                        (and application-name
                                             (cons "application_name" application-name))))
                    parameters))
      (unless (and (consp pair) (stringp (car pair)))
        (error 'parameter-error :parameter pair
               :message "Startup parameters must be (name . value) pairs"))
      (append-cstring builder (car pair))
      (append-cstring builder (or (cdr pair) "")))
    (append-u8 builder 0)
    (let ((octets (builder-octets builder)))
      (unless (and (<= 4 (length octets) #xffffffff)
                   (integerp *maximum-frame-size*)
                   (<= (length octets) *maximum-frame-size*))
        (error 'parameter-error
               :parameter (length octets)
               :message "PostgreSQL startup packet exceeds the configured frame limit"))
      (setf (aref octets 0) (ldb (byte 8 24) (length octets))
            (aref octets 1) (ldb (byte 8 16) (length octets))
            (aref octets 2) (ldb (byte 8 8) (length octets))
            (aref octets 3) (ldb (byte 8 0) (length octets)))
      octets)))

(defun encode-ssl-request ()
  (let ((builder (%protocol-builder 8)))
    (append-i32 builder 8)
    (append-i32 builder 80877103)
    (builder-octets builder)))

(defun encode-gssenc-request ()
  "Encode the PostgreSQL GSS encryption negotiation request." 
  (let ((builder (%protocol-builder 8)))
    (append-i32 builder 8)
    (append-i32 builder 80877104)
    (builder-octets builder)))

(defun %backend-secret-octets (backend-secret)
  (let ((octets
          (cond ((vectorp backend-secret)
                 (%wire-check-octets backend-secret))
                ((listp backend-secret)
                 (handler-case
                     (%wire-check-octets
                      (coerce backend-secret '(vector (unsigned-byte 8))))
                   (type-error ()
                     (error 'parameter-error
                            :parameter backend-secret
                            :message "Backend secret keys must be sequences of octets"))))
                (t
                 (error 'parameter-error
                        :parameter backend-secret
                        :message "Backend secret keys must be sequences of octets")))))
    (unless (<= 4 (length octets) 256)
      (error 'parameter-error
             :parameter backend-secret
             :message "PostgreSQL variable-length backend secret keys must contain 4 to 256 octets"))
    octets))

(defun encode-cancel-request (backend-pid backend-secret)
  (if (integerp backend-secret)
      (let ((builder (%protocol-builder 16)))
        (append-i32 builder 16)
        (append-i32 builder 80877102)
        (append-i32 builder backend-pid)
        (append-i32 builder backend-secret)
        (builder-octets builder))
      (let* ((secret (%backend-secret-octets backend-secret))
             (builder (%protocol-builder (+ 12 (length secret)))))
        (append-i32 builder (+ 12 (length secret)))
        (append-i32 builder 80877102)
        (append-i32 builder backend-pid)
        (append-octets builder secret)
        (builder-octets builder))))

(defun encode-query-message (sql)
  (let ((builder (%protocol-builder (+ 1 (length sql) 1))))
    (append-cstring builder sql)
    (%protocol-frame #\Q builder)))

(defun encode-parse-message (sql &key (statement-name "") parameter-type-oids)
  (let ((builder (%protocol-builder)))
    (append-cstring builder statement-name)
    (append-cstring builder sql)
    (append-u16 builder (length parameter-type-oids))
    (dolist (oid parameter-type-oids)
      (append-u32 builder oid))
    (%protocol-frame #\P builder)))

(defun encode-bind-message (parameters &key (portal-name "") (statement-name "")
                                      parameter-formats result-formats)
  (let* ((parameter-count (length parameters))
         (formats (%protocol-format-list parameter-formats parameter-count))
         (result-formats
           (mapcar #'%protocol-format-code
                   (cond ((null result-formats) nil)
                         ((listp result-formats) result-formats)
                         ((vectorp result-formats)
                          (coerce result-formats 'list))
                         (t (list result-formats)))))
         (builder (%protocol-builder)))
    (append-cstring builder portal-name)
    (append-cstring builder statement-name)
    (append-u16 builder (length formats))
    (dolist (format formats) (append-i16 builder format))
    (append-u16 builder parameter-count)
    (dolist (parameter parameters)
      (if (sql-null-p parameter)
          (append-i32 builder -1)
          (let ((octets (%protocol-string-octets parameter)))
            (append-i32 builder (length octets))
            (append-octets builder octets))))
    (append-u16 builder (length result-formats))
    (dolist (format result-formats) (append-i16 builder format))
    (%protocol-frame #\B builder)))

(defun encode-describe-message (name &key (kind :portal))
  (let ((builder (%protocol-builder)))
    (append-u8 builder (ecase kind (:portal (char-code #\P)) (:statement (char-code #\S))))
    (append-cstring builder name)
    (%protocol-frame #\D builder)))

(defun encode-execute-message (&key (portal-name "") (max-rows 0))
  (let ((builder (%protocol-builder)))
    (append-cstring builder portal-name)
    (append-u32 builder max-rows)
    (%protocol-frame #\E builder)))

(defun encode-flush-message ()
  (make-frame #\H #()))

(defun encode-function-call-message (function-oid arguments
                                      &key argument-formats (result-format 0))
  (let* ((arguments (cond ((null arguments) nil)
                          ((listp arguments) (copy-list arguments))
                          ((and (vectorp arguments) (not (stringp arguments)))
                           (coerce arguments 'list))
                          (t (error 'parameter-error
                                    :parameter arguments
                                    :message "Function-call arguments must be a list or vector."))))
         (argument-count (length arguments))
         (formats (%protocol-format-list argument-formats argument-count))
         (result-format (%protocol-format-code result-format))
         (builder (%protocol-builder)))
    (unless (and (integerp function-oid) (<= 0 function-oid #xffffffff))
      (error 'parameter-error
             :parameter function-oid
             :message "Function OIDs must be unsigned 32-bit integers."))
    (unless (<= argument-count #xffff)
      (error 'parameter-error
             :parameter argument-count
             :message "Function-call argument count exceeds the PostgreSQL limit."))
    (append-u32 builder function-oid)
    (append-u16 builder (length formats))
    (dolist (format formats)
      (append-i16 builder format))
    (append-u16 builder argument-count)
    (dolist (argument arguments)
      (if (sql-null-p argument)
          (append-i32 builder -1)
          (let ((octets (%protocol-string-octets argument)))
            (append-i32 builder (length octets))
            (append-octets builder octets))))
    (append-i16 builder result-format)
    (%protocol-frame #\F builder)))

(defun encode-close-message (name &key (kind :portal))
  (let ((builder (%protocol-builder)))
    (append-u8 builder (ecase kind (:portal (char-code #\P)) (:statement (char-code #\S))))
    (append-cstring builder name)
    (%protocol-frame #\C builder)))

(defun encode-sync-message () (make-frame #\S #()))

(defun encode-terminate-message () (make-frame #\X #()))

(defun encode-password-message (password)
  (let ((builder (%protocol-builder)))
    (append-cstring builder password)
    (%protocol-frame #\p builder)))

(defun encode-sasl-initial-response (mechanism initial-response)
  (let* ((response (and initial-response
                        (%protocol-string-octets initial-response)))
         (builder (%protocol-builder)))
    (append-cstring builder mechanism)
    (append-i32 builder (if response (length response) -1))
    (when response
      (append-octets builder response))
    (%protocol-frame #\p builder)))

(defun encode-sasl-response (response)
  (let ((builder (%protocol-builder)))
    (append-octets builder (%protocol-string-octets response))
    (%protocol-frame #\p builder)))

(defun encode-gss-response (response)
  "Encode an opaque GSSAPI or SSPI token in a PasswordMessage." 
  (let ((builder (%protocol-builder)))
    (append-octets builder (%protocol-string-octets response))
    (%protocol-frame #\p builder)))

(defun encode-copy-data-message (data)
  (make-frame #\d (%protocol-string-octets data)))

(defun encode-copy-done-message () (make-frame #\c #()))

(defun encode-copy-fail-message (message)
  (let ((builder (%protocol-builder)))
    (append-cstring builder message)
    (%protocol-frame #\f builder)))
