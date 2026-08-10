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

(defun %network-error (protocol-p value message &key expected actual)
  (if protocol-p
      (error 'protocol-error :message message :context :type-decoder
             :expected expected :actual actual)
      (error 'parameter-error :parameter value :message message)))

(defun %network-components (value separator protocol-p original)
  (let ((components '())
        (start 0))
    (loop for index from 0 below (length value)
          do (when (char= (char value index) separator)
               (push (subseq value start index) components)
               (setf start (1+ index))))
    (push (subseq value start) components)
    (nreverse components)))

(defun %network-parse-decimal (component maximum protocol-p original)
  (unless (plusp (length component))
    (%network-error protocol-p original
                    "PostgreSQL network address components must not be empty"))
  (let ((result 0))
    (loop for character across component
          for digit = (digit-char-p character 10)
          do (unless (integerp digit)
               (%network-error
                protocol-p original
                "PostgreSQL network address components contain only decimal digits"))
             (setf result (+ (* result 10) digit))
             (when (> result maximum)
               (%network-error
                protocol-p original
                "PostgreSQL network address component is outside its range"
                :expected maximum :actual result)))
    result))

(defun %network-parse-ipv4 (value protocol-p original)
  (let ((components (%network-components value #\. protocol-p original)))
    (unless (<= 1 (length components) 4)
      (%network-error protocol-p original
                      "PostgreSQL IPv4 addresses have one to four components"
                      :expected '(1 4) :actual (length components)))
    (let ((octets (make-array 4 :element-type '(unsigned-byte 8)))
          (component-count (length components)))
      (loop for component in components
            for index from 0
            do (setf (aref octets index)
                     (%network-parse-decimal
                      component 255 protocol-p original)))
      (values octets component-count))))

(defun %network-parse-hex-group (component protocol-p original)
  (unless (and (plusp (length component)) (<= (length component) 4))
    (%network-error protocol-p original
                    "PostgreSQL IPv6 groups must contain one to four digits"))
  (let ((result 0))
    (loop for character across component
          for digit = (digit-char-p character 16)
          do (unless (integerp digit)
               (%network-error
                protocol-p original
                "PostgreSQL IPv6 groups contain only hexadecimal digits"))
             (setf result (+ (ash result 4) digit)))
    result))

(defun %network-parse-ipv6-parts (value protocol-p original)
  (if (zerop (length value))
      nil
      (let ((parts (%network-components value #\: protocol-p original)))
        (loop for part in parts
              for index from 0
              for last-p = (= index (1- (length parts)))
              append
              (if (find #\. part)
                  (progn
                    (unless last-p
                      (%network-error
                       protocol-p original
                       "Embedded IPv4 must be the final PostgreSQL IPv6 component"))
                    (multiple-value-bind (octets component-count)
                        (%network-parse-ipv4 part protocol-p original)
                      (unless (= component-count 4)
                        (%network-error
                         protocol-p original
                         "Embedded IPv4 must contain four components"))
                      (list (+ (ash (aref octets 0) 8)
                               (aref octets 1))
                            (+ (ash (aref octets 2) 8)
                               (aref octets 3)))))
                  (list (%network-parse-hex-group
                         part protocol-p original)))))))

(defun %network-parse-ipv6 (value protocol-p original)
  (let* ((double-colon (search "::" value))
         (second-double-colon
           (and double-colon
                (search "::" value :start2 (+ double-colon 2))))
         (groups
           (cond
             (second-double-colon
              (%network-error
               protocol-p original
               "PostgreSQL IPv6 addresses contain more than one ::"))
             (double-colon
              (let* ((left (subseq value 0 double-colon))
                     (right (subseq value (+ double-colon 2)))
                     (left-groups (%network-parse-ipv6-parts
                                   left protocol-p original))
                     (right-groups (%network-parse-ipv6-parts
                                    right protocol-p original))
                     (present (+ (length left-groups) (length right-groups))))
                (unless (< present 8)
                  (%network-error
                   protocol-p original
                   "PostgreSQL IPv6 :: must replace at least one group"
                   :expected 7 :actual present))
                (append left-groups
                        (make-list (- 8 present) :initial-element 0)
                        right-groups)))
             (t
              (%network-parse-ipv6-parts value protocol-p original)))))
    (unless (= (length groups) 8)
      (%network-error protocol-p original
                      "PostgreSQL IPv6 addresses must contain eight groups"
                      :expected 8 :actual (length groups)))
    (let ((octets (make-array 16 :element-type '(unsigned-byte 8))))
      (loop for group in groups
            for index from 0
            do (setf (aref octets (* index 2)) (ldb (byte 8 8) group)
                     (aref octets (1+ (* index 2))) (ldb (byte 8 0) group)))
      octets)))

(defun %network-split-prefix (value protocol-p)
  (let ((slash (position #\/ value)))
    (if slash
        (progn
          (when (position #\/ value :start (1+ slash))
            (%network-error protocol-p value
                            "PostgreSQL network addresses contain one prefix only"))
          (values (subseq value 0 slash)
                  (subseq value (1+ slash))))
        (values value nil))))

(defun %parse-network-text (value cidr-p protocol-p)
  (unless (stringp value)
    (%network-error protocol-p value
                    "PostgreSQL network values must be strings"))
  (when (find #\Null value)
    (%network-error protocol-p value
                    "PostgreSQL network values must not contain NUL"))
  (multiple-value-bind (address prefix)
      (%network-split-prefix value protocol-p)
    (when (zerop (length address))
      (%network-error protocol-p value
                      "PostgreSQL network addresses must not be empty"))
    (let ((ipv6-p (find #\: address)))
      (multiple-value-bind (octets component-count)
          (if ipv6-p
              (values (%network-parse-ipv6 address protocol-p value) nil)
              (%network-parse-ipv4 address protocol-p value))
        (let* ((family (if ipv6-p 6 4))
               (maximum-netmask (if ipv6-p 128 32))
               (netmask
                 (if prefix
                     (%network-parse-decimal
                      prefix maximum-netmask protocol-p value)
                     (if (and cidr-p (= family 4))
                         (* 8 component-count)
                         maximum-netmask))))
          (when (and cidr-p
                     (not (%postgres-inet-host-bits-zero-p octets netmask)))
            (%network-error
             protocol-p value
             "PostgreSQL cidr values must have zero host bits"))
          (%make-postgres-inet :octets octets
                               :family family
                               :netmask netmask
                               :cidr-p cidr-p))))))

(defun %network-value (value cidr-p protocol-p)
  (cond
    ((postgres-inet-p value)
     (make-postgres-inet
      :octets (postgres-inet-octets value)
      :family (postgres-inet-family value)
      :netmask (postgres-inet-netmask value)
      :cidr-p cidr-p))
    ((stringp value)
     (%parse-network-text value cidr-p protocol-p))
    ((or (vectorp value) (listp value))
     (make-postgres-inet :octets value :cidr-p cidr-p))
    (t
     (%network-error protocol-p value
                     "PostgreSQL network values must be strings, octets, or inet values"))))

(defun %network-format-ipv4 (octets)
  (format nil "~{~D~^.~}" (coerce octets 'list)))

(defun %network-format-ipv6 (octets)
  (let ((groups
          (loop for index from 0 below 8
                collect (+ (ash (aref octets (* index 2)) 8)
                           (aref octets (1+ (* index 2)))))))
    (let ((best-start nil)
          (best-length 0))
      (loop for start from 0 below 8
            when (zerop (nth start groups))
              do (let ((end start))
                   (loop while (and (< end 8) (zerop (nth end groups)))
                         do (incf end))
                   (let ((length (- end start)))
                     (when (and (>= length 2) (> length best-length))
                       (setf best-start start
                             best-length length)))))
      (labels ((format-groups (values)
                 (format nil "~{~A~^:~}"
                         (mapcar (lambda (group)
                                   (string-downcase (format nil "~X" group)))
                                 values))))
        (if (null best-start)
            (format-groups groups)
            (let* ((best-end (+ best-start best-length))
                   (prefix (format-groups (subseq groups 0 best-start)))
                   (suffix (format-groups (subseq groups best-end))))
              (cond
                ((zerop best-start)
                 (concatenate 'string "::" suffix))
                ((= best-end 8)
                 (concatenate 'string prefix "::"))
                (t
                 (concatenate 'string prefix "::" suffix)))))))))

(defun %format-network-value (value cidr-p)
  (let* ((octets (postgres-inet-octets value))
         (family (postgres-inet-family value))
         (netmask (postgres-inet-netmask value))
         (maximum-netmask (if (= family 4) 32 128))
         (address (if (= family 4)
                      (%network-format-ipv4 octets)
                      (%network-format-ipv6 octets))))
    (if (or cidr-p (/= netmask maximum-netmask))
        (format nil "~A/~D" address netmask)
        address)))

(defun %decode-network-text (octets cidr-p)
  (%parse-network-text (%decode-utf8 octets) cidr-p t))

(defun %encode-network-text (value cidr-p)
  (%encode-utf8
   (%format-network-value (%network-value value cidr-p nil) cidr-p)))

(defun %decode-network-binary (octets cidr-p)
  (unless (%wire-octet-vector-p octets)
    (error 'protocol-error :message "Invalid PostgreSQL network binary payload"
           :context :type-decoder))
  (unless (>= (length octets) 4)
    (error 'protocol-error :message "PostgreSQL network binary payload is truncated"
           :context :type-decoder :expected 4 :actual (length octets)))
  (let* ((family-code (aref octets 0))
         (netmask (aref octets 1))
         (is-cidr (aref octets 2))
         (address-length (aref octets 3))
         (family (case family-code
                   (2 4)
                   (3 6)
                   (otherwise
                    (error 'protocol-error
                           :message "Unknown PostgreSQL network address family"
                           :context :type-decoder
                           :expected '(2 3) :actual family-code))))
         (expected-length (if (= family 4) 4 16))
         (maximum-netmask (if (= family 4) 32 128)))
    (unless (member is-cidr '(0 1))
      (error 'protocol-error
             :message "Invalid PostgreSQL network CIDR marker"
             :context :type-decoder :expected '(0 1) :actual is-cidr))
    (unless (= is-cidr (if cidr-p 1 0))
      (error 'protocol-error
             :message "PostgreSQL network CIDR marker does not match its type"
             :context :type-decoder
             :expected (if cidr-p 1 0)
             :actual is-cidr))
    (unless (<= 0 netmask maximum-netmask)
      (error 'protocol-error
             :message "PostgreSQL network netmask is outside its family range"
             :context :type-decoder :expected maximum-netmask :actual netmask))
    (unless (= address-length expected-length)
      (error 'protocol-error
             :message "PostgreSQL network address has an invalid length"
             :context :type-decoder :expected expected-length
             :actual address-length))
    (unless (= (length octets) (+ 4 address-length))
      (error 'protocol-error
             :message "PostgreSQL network binary payload has trailing bytes"
             :context :type-decoder :expected (+ 4 address-length)
             :actual (length octets)))
    (let ((address (make-array address-length
                               :element-type '(unsigned-byte 8)
                               :initial-contents (subseq octets 4))))
      (when (and cidr-p
                 (not (%postgres-inet-host-bits-zero-p address netmask)))
        (error 'protocol-error
               :message "PostgreSQL cidr binary value has non-zero host bits"
               :context :type-decoder))
      (make-postgres-inet :octets address
                          :family family
                          :netmask netmask
                          :cidr-p cidr-p))))

(defun %encode-network-binary (value cidr-p)
  (let* ((network (%network-value value cidr-p nil))
         (octets (postgres-inet-octets network))
         (family (postgres-inet-family network))
         (builder (make-octet-builder)))
    (append-u8 builder (if (= family 4) 2 3))
    (append-u8 builder (postgres-inet-netmask network))
    (append-u8 builder (if cidr-p 1 0))
    (append-u8 builder (length octets))
    (append-octets builder octets)
    (builder-octets builder)))
