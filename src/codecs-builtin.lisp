(in-package #:cl-postgresql-kit)

(defun %decode-boolean (value)
  (let ((string (%decode-utf8 value)))
    (cond ((string= string "t") t)
          ((string= string "f") nil)
          (t (error 'protocol-error :message "Invalid PostgreSQL boolean payload"
                    :context :type-decoder)))))

(defun %encode-boolean (value)
  (%encode-utf8 (if value "t" "f")))

(defun %decode-binary-signed (octets expected-size)
  (unless (and (vectorp octets)
               (every (lambda (octet)
                        (and (integerp octet) (<= 0 octet 255)))
                      octets)
               (= (length octets) expected-size))
    (error 'protocol-error :message "Invalid PostgreSQL integer binary length"
           :context :type-decoder :expected expected-size
           :actual (and (vectorp octets) (length octets))))
  (let ((value 0))
    (loop for octet across octets
          do (setf value (+ (ash value 8) octet)))
    (if (logbitp (1- (* 8 (length octets))) value)
        (- value (ash 1 (* 8 (length octets))))
        value)))

(defun %encode-binary-signed (value size)
  (unless (and (integerp value) (integerp size) (<= 1 size 8))
    (error 'parameter-error :parameter value
           :message "Binary integer value and size are invalid"))
  (unless (<= (- (ash 1 (1- (* 8 size))))
              value
              (1- (ash 1 (1- (* 8 size)))))
    (error 'parameter-error :parameter value
           :message "Binary integer value is out of range"))
  (let ((builder (make-octet-builder size)))
    (append-i64 builder value)
    (subseq (builder-octets builder) (- 8 size))))

(defun %decode-binary-unsigned (octets expected-size)
  (unless (and (vectorp octets)
               (every (lambda (octet)
                        (and (integerp octet) (<= 0 octet 255)))
                      octets)
               (= (length octets) expected-size))
    (error 'protocol-error :message "Invalid PostgreSQL unsigned integer binary length"
           :context :type-decoder :expected expected-size
           :actual (and (vectorp octets) (length octets))))
  (let ((value 0))
    (loop for octet across octets
          do (setf value (+ (ash value 8) octet)))
    value))

(defun %encode-binary-unsigned (value size)
  (unless (and (integerp value) (integerp size) (<= 1 size 8))
    (error 'parameter-error :parameter value
           :message "Binary unsigned integer value and size are invalid"))
  (unless (<= 0 value (1- (ash 1 (* 8 size))))
    (error 'parameter-error :parameter value
           :message "Binary unsigned integer value is out of range"))
  (let ((builder (make-octet-builder size)))
    (loop for shift from (* 8 (1- size)) downto 0 by 8
          do (append-u8 builder (ldb (byte 8 shift) value)))
    (coerce (builder-octets builder) '(vector (unsigned-byte 8)))))

(defun %decode-char-binary (octets)
  (string (code-char (%decode-binary-unsigned octets 1))))

(defun %encode-char-binary (value)
  (unless (and (stringp value) (= (length value) 1)
               (<= (char-code (aref value 0)) 255))
    (error 'parameter-error :parameter value
           :message "PostgreSQL char binary values must be one byte strings"))
  (%encode-binary-unsigned (char-code (aref value 0)) 1))

(defun %decode-name-binary (octets)
  (unless (%wire-octet-vector-p octets)
    (error 'protocol-error
           :message "PostgreSQL name binary value must be an octet vector"
           :context :type-decoder))
  (unless (< (length octets) 64)
    (error 'protocol-error
           :message "PostgreSQL name binary value exceeds NAMEDATALEN"
           :context :type-decoder :expected 63 :actual (length octets)))
  (when (find 0 octets)
    (error 'protocol-error
           :message "PostgreSQL name binary value contains NUL"
           :context :type-decoder))
  (%decode-utf8 octets))

(defun %encode-name-binary (value)
  (unless (stringp value)
    (error 'parameter-error :parameter value
           :message "PostgreSQL name binary values must be strings"))
  (when (find #\Null value)
    (error 'parameter-error :parameter value
           :message "PostgreSQL name binary values cannot contain NUL"))
  (let ((octets (%encode-utf8 value)))
    (unless (< (length octets) 64)
      (error 'parameter-error :parameter value
             :message "PostgreSQL name binary value exceeds NAMEDATALEN"))
    octets))

(defun %decode-tid-binary (octets)
  (unless (and (%wire-octet-vector-p octets) (= (length octets) 6))
    (error 'protocol-error
           :message "PostgreSQL TID binary value must be six bytes"
           :context :type-decoder :expected 6
           :actual (and (vectorp octets) (length octets))))
  (make-postgres-tid
   :block-number (%decode-binary-unsigned (subseq octets 0 4) 4)
   :offset-number (%decode-binary-unsigned (subseq octets 4 6) 2)))

(defun %encode-tid-binary (value)
  (unless (postgres-tid-p value)
    (error 'parameter-error :parameter value
           :message "PostgreSQL TID binary values must be POSTGRES-TID instances"))
  (let ((builder (make-octet-builder 6)))
    (append-u32 builder (postgres-tid-block-number value))
    (append-u16 builder (postgres-tid-offset-number value))
    (coerce (builder-octets builder) '(vector (unsigned-byte 8)))))
