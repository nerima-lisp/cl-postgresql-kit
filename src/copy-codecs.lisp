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

(defun %encode-copy-row-binary (values oids registry)
  ;; 0xffff is reserved for the binary COPY stream trailer.  Keep the
  ;; standalone row codec consistent with the stream representation.
  (unless (<= (length values) #xfffe)
    (error 'parameter-error
           :parameter values
           :message "A PostgreSQL binary COPY row cannot contain more than 65534 fields."))
  (let ((builder (make-octet-builder)))
    (append-u16 builder (length values))
    (loop for value in values
          for oid in oids
          do (if (sql-null-p value)
                 (append-i32 builder -1)
                 (let ((octets
                         (%protocol-string-octets
                          (encode-value registry oid value :format 1))))
                   (append-i32 builder (length octets))
                   (append-octets builder octets))))
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

(defun %decode-copy-row-binary (octets oids registry)
  (multiple-value-bind (count position) (%read-u16 octets 0)
    (%ensure-count-capacity octets position count 4 :copy-binary-row)
    (unless (= count (length oids))
      (error 'protocol-error
             :message "COPY binary row field count does not match the column type OIDs."
             :context :copy-binary-row
             :expected (length oids)
             :actual count))
    (let ((values nil))
      (dolist (oid oids)
        (multiple-value-bind (length next) (%read-i32 octets position)
          (setf position next)
          (cond ((= length -1)
                 (push +sql-null+ values))
                ((or (< length 0)
                     (> length (- (length octets) position)))
                 (error 'protocol-error
                        :message "COPY binary field length exceeds the row payload."
                        :context :copy-binary-row
                        :expected (- (length octets) position)
                        :actual length))
                (t
                 (let ((end (+ position length)))
                   (push (decode-value registry oid (subseq octets position end)
                                       :format 1)
                         values)
                   (setf position end))))))
      (%ensure-payload-end octets position :copy-binary-row)
      (nreverse values))))

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

(defun encode-copy-text-stream (rows column-type-oids
                                 &key (type-registry (default-type-registry))
                                   (delimiter #\Tab)
                                   (null-string "\\N"))
  "Encode ROWS as a complete PostgreSQL text COPY stream.

Each row is encoded with ENCODE-COPY-ROW using TEXT format, including its
terminating newline.  The result is an empty octet vector when ROWS is
empty.  VALUES and COLUMN-TYPE-OIDS may be lists or vectors."
  (check-type type-registry type-registry)
  (let ((rows (%copy-row-sequence rows :rows))
        (oids (%copy-row-type-oids column-type-oids))
        (builder (make-octet-builder)))
    (dolist (row rows)
      (append-octets builder
                     (encode-copy-row row oids
                                      :type-registry type-registry
                                      :format :text
                                      :delimiter delimiter
                                      :null-string null-string)))
    (builder-octets builder)))

(defun %copy-text-stream-rows (octets)
  (let ((rows nil)
        (builder (make-octet-builder))
        (escaped-p nil))
    (loop for octet across octets
          do (cond
               (escaped-p
                (when (= octet (char-code #\Newline))
                  (error 'protocol-error
                         :message "COPY text stream contains a raw newline in an escape."
                         :context :copy-text-stream))
                (append-u8 builder octet)
                (setf escaped-p nil))
               ((= octet (char-code #\\))
                (append-u8 builder octet)
                (setf escaped-p t))
               ((= octet (char-code #\Newline))
                (push (builder-octets builder) rows)
                (setf builder (make-octet-builder)))
               (t
                (append-u8 builder octet))))
    (when escaped-p
      (error 'protocol-error
             :message "COPY text stream ends with an incomplete escape."
             :context :copy-text-stream))
    (unless (zerop (length (builder-octets builder)))
      (error 'protocol-error
             :message "COPY text stream does not end with a row terminator."
             :context :copy-text-stream))
    (nreverse rows)))

(defun decode-copy-text-stream (data column-type-oids
                                 &key (type-registry (default-type-registry))
                                   (delimiter #\Tab)
                                   (null-string "\\N"))
  "Decode a complete PostgreSQL text COPY stream.

The stream must terminate every row with a newline.  Backslash escapes are
kept intact while locating row boundaries and are then decoded by
DECODE-COPY-ROW, so escaped newlines and delimiters do not split a row."
  (check-type type-registry type-registry)
  (let* ((octets (%protocol-string-octets data))
         (oids (%copy-row-type-oids column-type-oids))
         (delimiter-octet (%copy-row-delimiter-octet delimiter))
         (null-marker (%copy-row-null-marker null-string)))
    (loop for row in (%copy-text-stream-rows octets)
          collect (%decode-copy-row-text row oids type-registry
                                         delimiter-octet null-marker))))

(defparameter +copy-binary-signature+
  (make-array 11 :element-type '(unsigned-byte 8)
              :initial-contents '(80 71 67 79 80 89 10 255 13 10 0)))

(defun %copy-binary-signature-p (octets)
  (and (>= (length octets) (length +copy-binary-signature+))
       (loop for expected across +copy-binary-signature+
             for actual across octets
             repeat (length +copy-binary-signature+)
             always (= expected actual))))

(defun encode-copy-binary-header (&key (flags 0) (extension #()))
  "Encode the header of a PostgreSQL binary COPY stream.

FLAGS is the unsigned 32-bit header flags word.  EXTENSION is the raw
extension area, whose length is encoded immediately before its octets."
  (%wire-check-integer
   flags 0 #xffffffff
   "PostgreSQL binary COPY header flags must be an unsigned 32-bit integer.")
  (%wire-check-octets extension)
  (let ((builder (make-octet-builder (+ 19 (length extension)))))
    (append-octets builder +copy-binary-signature+)
    (append-u32 builder flags)
    (append-u32 builder (length extension))
    (append-octets builder extension)
    (builder-octets builder)))

(defun encode-copy-binary-trailer ()
  "Encode the -1 field-count trailer of a PostgreSQL binary COPY stream."
  (let ((builder (make-octet-builder 2)))
    (append-u16 builder #xffff)
    (builder-octets builder)))

(defun encode-copy-binary-stream (rows column-type-oids
                                   &key (type-registry (default-type-registry))
                                     (flags 0)
                                     (extension #()))
  "Encode ROWS as a complete PostgreSQL binary COPY stream.

Each row is encoded with ENCODE-COPY-ROW using BINARY format.  The result
contains the PostgreSQL binary signature, header extension area, all rows,
and the required trailer."
  (check-type type-registry type-registry)
  (let ((rows (%copy-row-sequence rows :rows))
        (oids (%copy-row-type-oids column-type-oids))
        (builder (make-octet-builder)))
    (append-octets builder
                   (encode-copy-binary-header :flags flags
                                              :extension extension))
    (dolist (row rows)
      (append-octets builder
                     (encode-copy-row row oids
                                      :type-registry type-registry
                                      :format :binary)))
    (append-octets builder (encode-copy-binary-trailer))
    (builder-octets builder)))

(defun %copy-binary-header (octets)
  (unless (>= (length octets) (+ (length +copy-binary-signature+) 8))
    (error 'protocol-error
           :message "PostgreSQL binary COPY header is truncated."
           :context :copy-binary-stream))
  (unless (%copy-binary-signature-p octets)
    (error 'protocol-error
           :message "PostgreSQL binary COPY signature is invalid."
           :context :copy-binary-stream))
  (multiple-value-bind (flags after-flags)
      (%read-u32 octets (length +copy-binary-signature+))
    (multiple-value-bind (extension-length after-length)
        (%read-u32 octets after-flags)
      (when (> extension-length (- (length octets) after-length))
        (error 'protocol-error
               :message "PostgreSQL binary COPY header extension is truncated."
               :context :copy-binary-stream
               :expected (- (length octets) after-length)
               :actual extension-length))
      (values flags
              (subseq octets after-length (+ after-length extension-length))
              (+ after-length extension-length)))))

(defun %copy-binary-row-end (octets position count)
  (%ensure-count-capacity octets position count 4 :copy-binary-stream)
  (loop repeat count
        do (multiple-value-bind (length next) (%read-i32 octets position)
             (setf position next)
             (cond ((= length -1) nil)
                   ((or (< length 0)
                        (> length (- (length octets) position)))
                    (error 'protocol-error
                           :message "PostgreSQL binary COPY field exceeds its row payload."
                           :context :copy-binary-stream
                           :expected (- (length octets) position)
                           :actual length))
                   (t (incf position length)))))
  position)

(defun decode-copy-binary-stream (data column-type-oids
                                   &key (type-registry (default-type-registry)))
  "Decode a complete PostgreSQL binary COPY stream.

Returns three values: the decoded rows, the header flags, and a copy of the
raw header extension area.  A missing trailer, invalid signature, truncated
field, or bytes following the trailer signals PROTOCOL-ERROR."
  (check-type type-registry type-registry)
  (let* ((octets (%protocol-string-octets data))
         (oids (%copy-row-type-oids column-type-oids)))
    (multiple-value-bind (flags extension position)
        (%copy-binary-header octets)
      (let ((rows nil)
            (trailer-seen-p nil))
        (loop while (< position (length octets))
              do (let ((row-start position))
                   (multiple-value-bind (count next) (%read-u16 octets position)
                     (setf position next)
                     (if (= count #xffff)
                         (progn
                           (setf trailer-seen-p t)
                           (return))
                         (progn
                           (setf position (%copy-binary-row-end octets position count))
                           (push (decode-copy-row
                                  (subseq octets row-start position)
                                  oids
                                  :type-registry type-registry
                                  :format :binary)
                                 rows))))))
        (unless trailer-seen-p
          (error 'protocol-error
                 :message "PostgreSQL binary COPY stream is missing its trailer."
                 :context :copy-binary-stream))
        (%ensure-payload-end octets position :copy-binary-stream)
        (values (nreverse rows) flags extension)))))
