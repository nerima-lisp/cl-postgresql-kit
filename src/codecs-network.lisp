(in-package #:cl-postgresql-kit)

(defun parse-replication-lsn (value)
  "Parse a PostgreSQL replication LSN into its unsigned 64-bit integer form."
  (cond
    ((integerp value)
     (if (<= 0 value #xffffffffffffffff)
         value
         (error 'parameter-error :parameter value
                :message "PostgreSQL replication LSN integer is out of range")))
    ((stringp value)
     (let ((slash (position #\/ value)))
       (unless (and slash
                    (> slash 0)
                    (< (1+ slash) (length value))
                    (null (position #\/ value :start (1+ slash))))
         (error 'parameter-error :parameter value
                :message "PostgreSQL replication LSN must have the form X/Y"))
       (labels ((parse-hex-range (start end)
                  (let ((result 0))
                    (when (= start end)
                      (error 'parameter-error :parameter value
                             :message "PostgreSQL replication LSN contains an empty hexadecimal component"))
                    (loop for index from start below end
                          for digit = (digit-char-p (char value index) 16)
                          do (unless digit
                               (error 'parameter-error :parameter value
                                      :message "PostgreSQL replication LSN contains a non-hexadecimal digit"))
                             (setf result (+ (ash result 4) digit)))
                    (unless (<= result #xffffffff)
                      (error 'parameter-error :parameter value
                             :message "PostgreSQL replication LSN component is out of range"))
                    result)))
         (+ (ash (parse-hex-range 0 slash) 32)
            (parse-hex-range (1+ slash) (length value))))))
    (t
     (error 'parameter-error :parameter value
            :message "PostgreSQL replication LSN must be an integer or X/Y string"))))

(defun format-replication-lsn (value)
  "Format a PostgreSQL replication LSN integer or X/Y string canonically."
  (let ((number (parse-replication-lsn value)))
    (format nil "~X/~X"
            (ldb (byte 32 32) number)
            (ldb (byte 32 0) number))))

(defun %decode-pg-lsn-text (octets)
  (format-replication-lsn (%decode-utf8 octets)))

(defun %encode-pg-lsn-text (value)
  (%encode-utf8 (format-replication-lsn value)))

(defun %decode-pg-lsn-binary (octets)
  (format-replication-lsn (%decode-binary-unsigned octets 8)))

(defun %encode-pg-lsn-binary (value)
  (%encode-binary-unsigned (parse-replication-lsn value) 8))

(defun %bit-string-error (protocol-p value message &key expected actual)
  (if protocol-p
      (error 'protocol-error :message message :context :type-decoder
             :expected expected :actual actual)
      (error 'parameter-error :parameter value :message message)))

(defun %bit-string-value (value protocol-p)
  (let ((bits (if (postgres-bit-string-p value)
                  (postgres-bit-string-bits value)
                  value)))
    (unless (stringp bits)
      (%bit-string-error protocol-p value
                         "PostgreSQL bit values must contain a string of 0 and 1 characters"))
    (loop for character across bits
          do (unless (member character '(#\0 #\1) :test #'char=)
               (%bit-string-error
                protocol-p value
                "PostgreSQL bit values may contain only 0 and 1 characters")))
    bits))

(defun %decode-bit-text (octets varying-p)
  (let ((bits (%decode-utf8 octets)))
    (%bit-string-value bits t)
    (make-postgres-bit-string :bits bits :varying-p varying-p)))

(defun %encode-bit-text (value)
  (%encode-utf8 (%bit-string-value value nil)))

(defun %decode-bit-binary (octets varying-p)
  (unless (and (%wire-octet-vector-p octets)
               (>= (length octets) 4))
    (error 'protocol-error :message "Invalid PostgreSQL bit binary payload"
           :context :type-decoder :expected 4
           :actual (and (vectorp octets) (length octets))))
  (let ((bit-count (%decode-binary-signed (subseq octets 0 4) 4)))
    (unless (>= bit-count 0)
      (error 'protocol-error :message "PostgreSQL bit length cannot be negative"
             :context :type-decoder :actual bit-count))
    (let ((byte-count (ceiling bit-count 8)))
      (unless (= (length octets) (+ 4 byte-count))
        (error 'protocol-error :message "Invalid PostgreSQL bit binary payload length"
               :context :type-decoder :expected (+ 4 byte-count)
               :actual (length octets)))
      (let ((bits (make-string bit-count :initial-element #\0)))
        (loop for bit-index from 0 below bit-count
              for octet = (aref octets (+ 4 (floor bit-index 8)))
              for bit-position = (- 7 (mod bit-index 8))
              do (when (logbitp bit-position octet)
                   (setf (char bits bit-index) #\1)))
        (make-postgres-bit-string :bits bits :varying-p varying-p)))))

(defun %encode-bit-binary (value)
  (let* ((bits (%bit-string-value value nil))
         (bit-count (length bits))
         (byte-count (ceiling bit-count 8))
         (builder (make-octet-builder (+ 4 byte-count))))
    (append-i32 builder bit-count)
    (loop for byte-index below byte-count
          for octet = 0
          do (loop for bit-position from 0 below 8
                   for bit-index = (+ (* byte-index 8) bit-position)
                   when (and (< bit-index bit-count)
                             (char= (char bits bit-index) #\1))
                     do (setf octet
                              (logior octet (ash 1 (- 7 bit-position)))))
             (append-u8 builder octet))
    (builder-octets builder)))

(defun %mac-address-error (protocol-p value message &key expected actual)
  (if protocol-p
      (error 'protocol-error :message message :context :type-decoder
             :expected expected :actual actual)
      (error 'parameter-error :parameter value :message message)))

(defun %mac-address-separator-p (character)
  (member character '(#\: #\- #\.) :test #'char=))

(defun %mac-address-text-separator (value)
  (cond ((position #\: value) #\:)
        ((position #\- value) #\-)
        ((position #\. value) #\.)
        (t nil)))

(defun %mac-address-components (value separator)
  (let ((components '())
        (start 0))
    (loop for index from 0 below (length value)
          do (when (char= (char value index) separator)
               (push (subseq value start index) components)
               (setf start (1+ index))))
    (push (subseq value start) components)
    (nreverse components)))

(defun %parse-mac-address-component (component width protocol-p original)
  (unless (= (length component) width)
    (%mac-address-error
     protocol-p original
     "PostgreSQL MAC address components have an invalid width"
     :expected width :actual (length component)))
  (let ((value 0))
    (loop for character across component
          for digit = (digit-char-p character 16)
          do (unless (integerp digit)
               (%mac-address-error
                protocol-p original
                "PostgreSQL MAC addresses contain only hexadecimal digits"))
             (setf value (+ (ash value 4) digit)))
    value))

(defun %parse-mac-address-text (value expected-size protocol-p)
  (let ((separator (%mac-address-text-separator value)))
    (unless separator
      (%mac-address-error
       protocol-p value
       "PostgreSQL MAC addresses must use colon, hyphen, or dotted separators"))
    (loop for character across value
          do (when (and (%mac-address-separator-p character)
                        (char/= character separator))
               (%mac-address-error
                protocol-p value
                "PostgreSQL MAC addresses must use one separator style")))
    (let* ((dotted-p (char= separator #\.))
           (component-width (if dotted-p 4 2))
           (component-count (if dotted-p (/ expected-size 2) expected-size))
           (components (%mac-address-components value separator)))
      (unless (= (length components) component-count)
        (%mac-address-error
         protocol-p value
         "PostgreSQL MAC addresses have an invalid component count"
         :expected component-count :actual (length components)))
      (let ((octets (make-array expected-size :element-type '(unsigned-byte 8)))
            (position 0))
        (dolist (component components)
          (let ((component-value
                  (%parse-mac-address-component
                   component component-width protocol-p value)))
            (if dotted-p
                (progn
                  (setf (aref octets position) (ldb (byte 8 8) component-value)
                        (aref octets (1+ position)) (ldb (byte 8 0) component-value))
                  (incf position 2))
                (setf (aref octets position) component-value
                      position (1+ position)))))
        octets))))

(defun %mac-address-octets (value expected-size protocol-p)
  (let ((raw (if (postgres-mac-address-p value)
                 (postgres-mac-address-octets value)
                 value)))
    (cond
      ((stringp raw)
       (%parse-mac-address-text raw expected-size protocol-p))
      ((vectorp raw)
       (unless (and (= (length raw) expected-size)
                    (%wire-octet-vector-p raw))
         (%mac-address-error
          protocol-p value
          "PostgreSQL MAC address octets have an invalid length or value"
          :expected expected-size :actual (and (vectorp raw) (length raw))))
       (make-array expected-size :element-type '(unsigned-byte 8)
                   :initial-contents raw))
      ((listp raw)
       (handler-case
           (%mac-address-octets (coerce raw 'vector) expected-size protocol-p)
         (type-error ()
           (%mac-address-error
            protocol-p value
            "PostgreSQL MAC address octets must be a proper sequence"))))
      (t
       (%mac-address-error
        protocol-p value
        "PostgreSQL MAC addresses must be strings, octet vectors, or MAC address values")))))

(defun %format-mac-address (octets)
  (string-downcase
   (format nil "~{~2,'0X~^:~}" (coerce octets 'list))))

(defun %decode-mac-address-text (octets expected-size)
  (make-postgres-mac-address
   :octets (%parse-mac-address-text (%decode-utf8 octets) expected-size t)))

(defun %encode-mac-address-text (value expected-size)
  (%encode-utf8
   (if (stringp value)
       (progn
         (%parse-mac-address-text value expected-size nil)
         value)
       (%format-mac-address (%mac-address-octets value expected-size nil)))))

(defun %decode-mac-address-binary (octets expected-size)
  (unless (and (%wire-octet-vector-p octets)
               (= (length octets) expected-size))
    (error 'protocol-error :message "Invalid PostgreSQL MAC address binary payload"
           :context :type-decoder :expected expected-size
           :actual (and (vectorp octets) (length octets))))
  (make-postgres-mac-address
   :octets (make-array expected-size :element-type '(unsigned-byte 8)
                       :initial-contents octets)))

(defun %encode-mac-address-binary (value expected-size)
  (%mac-address-octets value expected-size nil))
