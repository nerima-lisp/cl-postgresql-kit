(in-package #:cl-postgresql-kit)

(defun %copy-row-sequence (value parameter)
  (cond ((null value) nil)
        ((listp value) (copy-list value))
        ((and (vectorp value) (not (stringp value)))
         (coerce value 'list))
        (t (error 'parameter-error
                  :parameter value
                  :message (format nil "~A must be a list or vector."
                                   parameter)))))

(defun %copy-row-type-oids (value)
  (let ((oids (%copy-row-sequence value :column-type-oids)))
    (dolist (oid oids)
      (unless (and (integerp oid) (<= 0 oid #xffffffff))
        (error 'parameter-error
               :parameter oid
               :message "COPY column type OIDs must be unsigned 32-bit integers.")))
    oids))

(defun %copy-row-format-code (format)
  (cond ((or (eq format :text) (eql format 0)) 0)
        ((or (eq format :binary) (eql format 1)) 1)
        (t (error 'parameter-error
                  :parameter format
                  :message "COPY row format must be :TEXT, :BINARY, 0, or 1."))))

(defun %copy-row-delimiter-octet (delimiter)
  (unless (and (characterp delimiter)
               (<= 1 (char-code delimiter) #xff)
               (not (member delimiter '(#\\ #\Null #\Newline #\Return))))
    (error 'parameter-error
           :parameter delimiter
           :message "COPY text delimiters must be a non-NUL, non-newline, non-backslash octet."))
  (char-code delimiter))

(defun %copy-row-append-escape (builder code)
  (append-u8 builder (char-code #\\))
  (append-u8 builder code))

(defun %copy-row-append-text-field (builder octets delimiter)
  (loop for octet across octets
        do (cond ((= octet (char-code #\\))
                  (%copy-row-append-escape builder (char-code #\\)))
                 ((= octet 0)
                  (%copy-row-append-escape builder (char-code #\0)))
                 ((= octet 8)
                  (%copy-row-append-escape builder (char-code #\b)))
                 ((= octet 9)
                  (%copy-row-append-escape builder (char-code #\t)))
                 ((= octet 10)
                  (%copy-row-append-escape builder (char-code #\n)))
                 ((= octet 11)
                  (%copy-row-append-escape builder (char-code #\v)))
                 ((= octet 12)
                  (%copy-row-append-escape builder (char-code #\f)))
                 ((= octet 13)
                  (%copy-row-append-escape builder (char-code #\r)))
                 ((= octet delimiter)
                  (%copy-row-append-escape builder delimiter))
                 (t (append-u8 builder octet))))
  builder)

(defun %copy-row-null-marker (null-string)
  (%protocol-string-octets null-string))

(defun %encode-copy-row-text (values oids registry delimiter null-marker)
  (let ((builder (make-octet-builder)))
    (loop for value in values
          for oid in oids
          for index from 0
          do (when (plusp index)
               (append-u8 builder delimiter))
             (if (sql-null-p value)
                 (append-octets builder null-marker)
                 (%copy-row-append-text-field
                  builder
                  (%protocol-string-octets
                   (encode-value registry oid value :format 0))
                  delimiter)))
    (append-u8 builder (char-code #\Newline))
    (builder-octets builder)))

(defun encode-copy-row (values column-type-oids
                         &key (type-registry (default-type-registry))
                           (format :text)
                           (delimiter #\Tab)
                           (null-string "\\N"))
  "Encode one logical PostgreSQL COPY row using the registered column codecs.

TEXT rows include their terminating newline.  BINARY rows contain only the
row payload (the field count and field values), not the COPY binary header or
trailer.  VALUES and COLUMN-TYPE-OIDS may be lists or vectors."
  (check-type type-registry type-registry)
  (let* ((values (%copy-row-sequence values :values))
         (oids (%copy-row-type-oids column-type-oids))
         (format-code (%copy-row-format-code format)))
    (unless (= (length values) (length oids))
      (error 'parameter-error
             :parameter column-type-oids
             :message "COPY values and column type OIDs must have the same length."))
    (if (zerop format-code)
        (%encode-copy-row-text values oids type-registry
                               (%copy-row-delimiter-octet delimiter)
                               (%copy-row-null-marker null-string))
        (%encode-copy-row-binary values oids type-registry))))

(defun %copy-row-text-without-terminator (octets)
  (let ((end (length octets)))
    (when (and (plusp end) (= (aref octets (1- end)) 10))
      (decf end)
      (when (and (plusp end) (= (aref octets (1- end)) 13))
        (decf end)))
    (subseq octets 0 end)))

(defun %copy-row-split-text (octets delimiter)
  (let ((fields nil)
        (builder (make-octet-builder))
        (position 0)
        (length (length octets)))
    (loop while (< position length)
          do (let ((octet (aref octets position)))
               (cond ((= octet (char-code #\\))
                      (append-u8 builder octet)
                      (incf position)
                      (when (< position length)
                        (append-u8 builder (aref octets position))))
                     ((= octet delimiter)
                      (push (builder-octets builder) fields)
                      (setf builder (make-octet-builder)))
                     (t (append-u8 builder octet))))
             (incf position))
    (push (builder-octets builder) fields)
    (nreverse fields)))

(defun %copy-row-hex-digit (octet)
  (and (< octet 128)
       (digit-char-p (code-char octet) 16)))

(defun %copy-row-unescape-text (octets)
  (let ((builder (make-octet-builder))
        (position 0)
        (length (length octets)))
    (loop while (< position length)
          do (let ((octet (aref octets position)))
               (if (/= octet (char-code #\\))
                   (append-u8 builder octet)
                   (progn
                     (incf position)
                     (when (>= position length)
                       (error 'protocol-error
                              :message "COPY text field ends with an incomplete escape."
                              :context :copy-text))
                     (let ((escaped (aref octets position)))
                       (case escaped
                         (110 (append-u8 builder 10))
                         (114 (append-u8 builder 13))
                         (116 (append-u8 builder 9))
                         (98 (append-u8 builder 8))
                         (102 (append-u8 builder 12))
                         (118 (append-u8 builder 11))
                         (92 (append-u8 builder 92))
                         (120
                          (when (>= (+ position 2) length)
                            (error 'protocol-error
                                   :message "COPY hex escape is truncated."
                                   :context :copy-text))
                          (let ((high (%copy-row-hex-digit
                                       (aref octets (1+ position))))
                                (low (%copy-row-hex-digit
                                      (aref octets (+ position 2)))))
                            (unless (and high low)
                              (error 'protocol-error
                                     :message "COPY hex escape contains a non-hex digit."
                                     :context :copy-text))
                            (append-u8 builder (+ (* 16 high) low)))
                          (incf position 2))
                         (otherwise
                          (if (and (<= 48 escaped 55))
                              (let ((value 0)
                                    (digits 0)
                                    (cursor position))
                                (loop while (and (< cursor length)
                                                 (< digits 3)
                                                 (<= 48 (aref octets cursor) 55))
                                      do (setf value (+ (* value 8)
                                                         (- (aref octets cursor) 48)))
                                         (incf cursor)
                                         (incf digits))
                                (append-u8 builder value)
                                (setf position (1- cursor)))
                              ;; PostgreSQL COPY treats an unknown escape as
                              ;; the escaped byte itself.
                              (append-u8 builder escaped))))))))
             (incf position))
    (builder-octets builder)))

(defun %decode-copy-row-text (octets oids registry delimiter null-marker)
  (let ((fields (%copy-row-split-text
                 (%copy-row-text-without-terminator octets)
                 delimiter)))
    (unless (= (length fields) (length oids))
      (error 'protocol-error
             :message "COPY text row field count does not match the column type OIDs."
             :context :copy-text
             :expected (length oids)
             :actual (length fields)))
    (loop for field in fields
          for oid in oids
          collect (if (equalp field null-marker)
                      +sql-null+
                      (decode-value registry oid
                                    (%copy-row-unescape-text field)
                                    :format 0)))))

(defun decode-copy-row (data column-type-oids
                        &key (type-registry (default-type-registry))
                          (format :text)
                          (delimiter #\Tab)
                          (null-string "\\N"))
  "Decode one logical PostgreSQL COPY row with the registered column codecs.

TEXT input may include its terminating newline.  BINARY input is one row
payload without the COPY binary header or trailer."
  (check-type type-registry type-registry)
  (let* ((octets (%protocol-string-octets data))
         (oids (%copy-row-type-oids column-type-oids))
         (format-code (%copy-row-format-code format)))
    (if (zerop format-code)
        (%decode-copy-row-text octets oids type-registry
                               (%copy-row-delimiter-octet delimiter)
                               (%copy-row-null-marker null-string))
        (%decode-copy-row-binary octets oids type-registry))))
