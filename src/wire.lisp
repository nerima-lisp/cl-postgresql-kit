(in-package #:cl-postgresql-kit)

(deftype octet () '(unsigned-byte 8))

(defparameter *maximum-frame-size* (* 64 1024 1024)
  "Maximum accepted PostgreSQL message size, including its length field.")

(defstruct (octet-builder (:constructor %make-octet-builder (data)))
  (data (make-array 0 :element-type 'octet :adjustable t :fill-pointer 0)
        :type vector))

(defun make-octet-builder (&optional (size 64))
  (%make-octet-builder
   (make-array size :element-type 'octet :adjustable t :fill-pointer 0)))

(defun builder-octets (builder)
  (copy-seq (octet-builder-data builder)))

(defun %wire-octet-vector-p (value)
  (and (vectorp value)
       (every (lambda (octet)
                (and (integerp octet) (<= 0 octet 255)))
              value)))

(defun %wire-check-octets (value)
  (unless (%wire-octet-vector-p value)
    (error 'parameter-error :parameter value
           :message "PostgreSQL wire data must be a vector of octets"))
  value)

(defun %wire-check-integer (value minimum maximum message)
  (unless (and (integerp value) (<= minimum value maximum))
    (error 'parameter-error :parameter value :message message))
  value)

(defun append-octets (builder octets)
  (%wire-check-octets octets)
  (loop for octet across octets do (vector-push-extend octet (octet-builder-data builder)))
  builder)

(defun append-u8 (builder value)
  (%wire-check-integer value 0 #xff "Unsigned 8-bit value is out of range")
  (vector-push-extend value (octet-builder-data builder))
  builder)

(defun append-u16 (builder value)
  (%wire-check-integer value 0 #xffff "Unsigned 16-bit value is out of range")
  (append-u8 builder (ldb (byte 8 8) value))
  (append-u8 builder (ldb (byte 8 0) value)))

(defun append-i16 (builder value)
  (%wire-check-integer value -32768 32767 "Signed 16-bit value is out of range")
  (append-u16 builder (logand value #xffff)))

(defun append-u32 (builder value)
  (%wire-check-integer value 0 #xffffffff "Unsigned 32-bit value is out of range")
  (loop for shift from 24 downto 0 by 8
        do (append-u8 builder (ldb (byte 8 shift) value)))
  builder)

(defun append-u64 (builder value)
  (%wire-check-integer value 0 (1- (ash 1 64))
                       "Unsigned 64-bit value is out of range")
  (loop for shift from 56 downto 0 by 8
        do (append-u8 builder (ldb (byte 8 shift) value)))
  builder)

(defun append-i32 (builder value)
  (%wire-check-integer value -2147483648 2147483647
                       "Signed 32-bit value is out of range")
  (append-u32 builder (logand value #xffffffff)))

(defun append-i64 (builder value)
  (%wire-check-integer value (- (ash 1 63)) (1- (ash 1 63))
                       "Signed 64-bit value is out of range")
  (loop for shift from 56 downto 0 by 8
        do (append-u8 builder (ldb (byte 8 shift) value)))
  builder)

(defun append-cstring (builder string)
  (unless (stringp string)
    (error 'parameter-error :parameter string
           :message "PostgreSQL cstrings must be strings"))
  (when (find #\Null string)
    (error 'parameter-error :parameter string
           :message "PostgreSQL cstrings must not contain NUL"))
  (append-octets builder
                 (cl-codec-kit:string-to-octets string :encoding :utf-8))
  (append-u8 builder 0))

(defun %octet-at (octets position)
  (if (and (integerp position) (>= position 0) (< position (length octets)))
      (aref octets position)
      (error 'protocol-error :message "Unexpected end of PostgreSQL message"
             :context :wire :expected position :actual (length octets))))

(defun %read-u16 (octets position)
  (values (+ (ash (%octet-at octets position) 8)
             (%octet-at octets (1+ position)))
          (+ position 2)))

(defun %read-i16 (octets position)
  (multiple-value-bind (value next) (%read-u16 octets position)
    (values (if (logbitp 15 value) (- value #x10000) value) next)))

(defun %read-u32 (octets position)
  (values (loop with value = 0
                for offset from 0 below 4
                do (setf value (+ (ash value 8)
                                  (%octet-at octets (+ position offset))))
                finally (return value))
          (+ position 4)))

(defun %read-u64 (octets position)
  (values (loop with value = 0
                for offset from 0 below 8
                do (setf value (+ (ash value 8)
                                  (%octet-at octets (+ position offset))))
                finally (return value))
          (+ position 8)))

(defun %read-i32 (octets position)
  (multiple-value-bind (value next) (%read-u32 octets position)
    (values (if (logbitp 31 value) (- value #x100000000) value) next)))

(defun %read-i64 (octets position)
  (values (loop with value = 0
                for offset from 0 below 8
                do (setf value (+ (ash value 8)
                                  (%octet-at octets (+ position offset))))
                finally (return (if (logbitp 63 value)
                                    (- value #x10000000000000000)
                                    value)))
          (+ position 8)))

(defun %read-cstring (octets position &key (encoding :utf-8))
  (let ((end (position 0 octets :start position)))
    (unless end
      (error 'protocol-error :message "Unterminated PostgreSQL cstring"
             :context :wire :expected 0 :actual nil))
    (values (cl-codec-kit:octets-to-string (subseq octets position end)
                                           :encoding encoding)
            (1+ end))))

(defun make-frame (type payload)
  "Encode a typed PostgreSQL backend/frontend frame."
  (%wire-check-octets payload)
  (let ((type-code (if (characterp type) (char-code type) type))
        (frame-length (+ 4 (length payload))))
    (%wire-check-integer type-code 0 #xff "PostgreSQL message type is out of range")
    (%wire-check-integer frame-length 4 *maximum-frame-size*
                         "PostgreSQL frame exceeds configured limit")
    (let ((builder (make-octet-builder (+ 1 frame-length))))
      (append-u8 builder type-code)
      (append-i32 builder frame-length)
      (append-octets builder payload)
      (builder-octets builder))))

(defstruct (backend-message (:constructor %make-backend-message (type payload)))
  (type 0 :type octet)
  (payload #() :type vector))

(defun backend-message-kind (message-or-type)
  (let ((type (if (typep message-or-type 'backend-message)
                  (backend-message-type message-or-type)
                  message-or-type)))
    (case (if (characterp type) (char-code type) type)
      (#x52 :authentication)
      (#x53 :parameter-status)
      (#x4b :backend-key-data)
      (#x76 :negotiate-protocol-version)
      (#x5a :ready-for-query)
      (#x54 :row-description)
      (#x44 :data-row)
      (#x43 :command-complete)
      (#x45 :error-response)
      (#x4e :notice-response)
      (#x41 :notification-response)
      (#x47 :copy-in-response)
      (#x48 :copy-out-response)
      (#x57 :copy-both-response)
      (#x64 :copy-data)
      (#x63 :copy-done)
      (#x66 :copy-fail)
      (#x49 :empty-query-response)
      (#x31 :parse-complete)
      (#x32 :bind-complete)
      (#x33 :close-complete)
      (#x6e :no-data)
      (#x73 :portal-suspended)
      (#x74 :parameter-description)
      (#x56 :function-call-response)
      (otherwise :unknown))))

(defun parse-frame (octets)
  "Parse one complete typed PostgreSQL frame from OCTETS."
  (%wire-check-octets octets)
  (when (< (length octets) 5)
    (error 'protocol-error :message "PostgreSQL frame is too short"
           :context :wire :expected 5 :actual (length octets)))
  (multiple-value-bind (length next) (%read-u32 octets 1)
    (when (< length 4)
      (error 'protocol-error :message "Invalid PostgreSQL frame length"
             :context :wire :expected ">= 4" :actual length))
    (when (> length *maximum-frame-size*)
      (error 'protocol-error :message "PostgreSQL frame exceeds configured limit"
             :context :wire :expected *maximum-frame-size* :actual length))
    (let ((end (+ 1 length)))
      (unless (= end (length octets))
        (error 'protocol-error :message "PostgreSQL frame has trailing or missing bytes"
               :context :wire :expected end :actual (length octets)))
      (%make-backend-message (aref octets 0)
                              (subseq octets next end)))))

(defun read-backend-message (transport)
  "Read one typed backend message from TRANSPORT."
  (let ((type (aref (transport-read-exactly transport 1) 0))
        (header (transport-read-exactly transport 4)))
    (multiple-value-bind (length ignored) (%read-u32 header 0)
      (declare (ignore ignored))
      (when (< length 4)
        (error 'protocol-error :message "Invalid PostgreSQL backend message length"
               :context :wire :expected ">= 4" :actual length))
      (when (> length *maximum-frame-size*)
        (error 'protocol-error :message "PostgreSQL backend message exceeds configured limit"
               :context :wire :expected *maximum-frame-size* :actual length))
      (%make-backend-message type (transport-read-exactly transport (- length 4))))))
