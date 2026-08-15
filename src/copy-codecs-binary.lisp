(in-package #:cl-postgresql-kit)

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
