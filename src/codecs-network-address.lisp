(in-package #:cl-postgresql-kit)

(defun %network-error (protocol-p value message &key expected actual)
  (if protocol-p
      (error 'protocol-error :message message :context :type-decoder
             :expected expected :actual actual)
      (error 'parameter-error :parameter value :message message)))

(defun %network-components (value separator protocol-p original)
  (declare (ignore protocol-p original))
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
