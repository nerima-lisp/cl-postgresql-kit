(in-package #:cl-postgresql-kit)

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
