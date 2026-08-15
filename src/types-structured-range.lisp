(in-package #:cl-postgresql-kit)

(defconstant +range-empty-flag+ #x01)

(defconstant +range-lower-inclusive-flag+ #x02)

(defconstant +range-upper-inclusive-flag+ #x04)

(defconstant +range-lower-infinite-flag+ #x08)

(defconstant +range-upper-infinite-flag+ #x10)

(defun %parse-range-bound-token (token)
  (when (string/= (%type-trim token) "")
    (multiple-value-bind (value quoted-p)
        (%unquote-type-token token)
      (declare (ignore quoted-p))
      (values value t))))

(defun %parse-range-text (string)
  (let ((string (%type-trim string)))
    (when (string= string "empty")
      (return-from %parse-range-text
        (values nil nil nil nil nil nil t)))
    (when (< (length string) 2)
      (error 'protocol-error :message "PostgreSQL range text is too short"
             :context :type-decoder))
    (let ((opening (char string 0))
          (closing (char string (1- (length string)))))
      (unless (member opening '(#\[ #\())
        (error 'protocol-error :message "PostgreSQL range text has an invalid opening delimiter"
               :context :type-decoder))
      (unless (member closing '(#\] #\)))
        (error 'protocol-error :message "PostgreSQL range text has an invalid closing delimiter"
               :context :type-decoder))
      (let ((tokens (%split-type-delimited
                     (subseq string 1 (1- (length string))) #\,
                     :type-decoder)))
        (unless (= (length tokens) 2)
          (error 'protocol-error :message "PostgreSQL range text must contain one comma"
                 :context :type-decoder :expected 2 :actual (length tokens)))
        (multiple-value-bind (lower lower-present-p)
            (%parse-range-bound-token (first tokens))
          (multiple-value-bind (upper upper-present-p)
              (%parse-range-bound-token (second tokens))
            (values lower upper lower-present-p upper-present-p
                    (char= opening #\[) (char= closing #\]) nil)))))))

(defun %range-bound-text (registry subtype-oid value)
  (let ((payload (encode-value registry subtype-oid value :format 0)))
    (when (or (null payload) (sql-null-p payload))
      (error 'parameter-error :parameter value
             :message "PostgreSQL range bounds must not be SQL NULL"))
    (%type-encoded-text payload :range)))

(defun %decode-range-bound-text (registry subtype-oid value present-p)
  (when present-p
    (decode-value registry subtype-oid (%encode-utf8 value) :format 0)))

(defun %decode-range-text (registry subtype-oid octets)
  (multiple-value-bind (lower upper lower-present-p upper-present-p
                               lower-inclusive upper-inclusive empty-p)
      (%parse-range-text (%type-encoded-text octets :range))
    (make-postgres-range
     :lower (%decode-range-bound-text registry subtype-oid lower lower-present-p)
     :upper (%decode-range-bound-text registry subtype-oid upper upper-present-p)
     :lower-inclusive lower-inclusive
     :upper-inclusive upper-inclusive
     :empty-p empty-p)))

(defun %encode-range-text (registry subtype-oid value)
  (unless (postgres-range-p value)
    (error 'parameter-error :parameter value
           :message "PostgreSQL range values must be postgres-range instances"))
  (if (postgres-range-empty-p value)
      (%encode-utf8 "empty")
      (%encode-utf8
       (with-output-to-string (stream)
         (write-char (if (postgres-range-lower-inclusive value) #\[ #\() stream)
         (when (postgres-range-lower value)
           (%write-type-token
            stream
            (%range-bound-text registry subtype-oid
                               (postgres-range-lower value))))
         (write-char #\, stream)
         (when (postgres-range-upper value)
           (%write-type-token
            stream
            (%range-bound-text registry subtype-oid
                               (postgres-range-upper value))))
         (write-char (if (postgres-range-upper-inclusive value) #\] #\)) stream)))))

(defun %split-multirange-text (string)
  (let* ((string (%type-trim string))
         (length (length string))
         (limit (1- length))
         (position 1)
         (items nil)
         (after-comma-p nil))
    (unless (and (>= length 2)
                 (char= (char string 0) #\{)
                 (char= (char string limit) #\}))
      (error 'protocol-error
             :message "PostgreSQL multirange text has invalid delimiters"
             :context :type-decoder))
    (labels ((skip-space ()
               (loop while (and (< position limit)
                                (%space-character-p (char string position)))
                     do (incf position)))
             (invalid (message)
               (error 'protocol-error :message message :context :type-decoder)))
      (loop
        (skip-space)
        (when (= position limit)
          (when after-comma-p
            (invalid "PostgreSQL multirange text has a trailing comma"))
          (return (nreverse items)))
        (let ((start position)
              (character (char string position)))
          (if (or (char= character #\[)
                  (char= character #\())
              (progn
                (let ((quoted-p nil)
                      (escaped-p nil)
                      (closed-p nil))
                  (loop while (< position limit)
                        for item-character = (char string position)
                        do (cond (escaped-p
                                  (setf escaped-p nil)
                                  (incf position))
                                 ((char= item-character #\\)
                                  (setf escaped-p t)
                                  (incf position))
                                 ((char= item-character #\")
                                  (setf quoted-p (not quoted-p))
                                  (incf position))
                                 ((and (not quoted-p)
                                       (or (char= item-character #\])
                                           (char= item-character #\))))
                                  (incf position)
                                  (setf closed-p t)
                                  (return))
                                 (t
                                  (incf position))))
                  (when (or quoted-p escaped-p)
                    (invalid "PostgreSQL multirange range item is unterminated"))
                  (unless closed-p
                    (invalid "PostgreSQL multirange range item has no closing delimiter")))
                (push (subseq string start position) items))
              (let ((token-start position))
                (loop while (and (< position limit)
                                 (not (char= (char string position) #\,)))
                      do (incf position))
                (unless (string= (%type-trim (subseq string token-start position))
                                 "empty")
                  (invalid "PostgreSQL multirange item is neither a range nor empty"))))
          (setf after-comma-p nil)
          (skip-space)
          (cond ((= position limit)
                 (return (nreverse items)))
                ((char= (char string position) #\,)
                 (incf position)
                 (setf after-comma-p t))
                (t
                 (invalid "PostgreSQL multirange items must be comma-separated"))))))))

(defun %decode-multirange-text (registry subtype-oid octets)
  (make-postgres-multirange
   :ranges
   (loop for item in (%split-multirange-text
                      (%type-encoded-text octets :multirange))
         collect (%decode-range-text registry subtype-oid (%encode-utf8 item)))))

(defun %encode-multirange-text (registry subtype-oid value)
  (unless (postgres-multirange-p value)
    (error 'parameter-error :parameter value
           :message "PostgreSQL multirange values must be postgres-multirange instances"))
  (%encode-utf8
   (with-output-to-string (stream)
     (write-char #\{ stream)
     (loop for range across (postgres-multirange-ranges value)
           for first-p = t then nil
           do (unless first-p
                (write-char #\, stream))
              (write-string (%type-encoded-text
                             (%encode-range-text registry subtype-oid range)
                             :multirange)
                            stream))
     (write-char #\} stream))))

(defun %range-binary-payload (registry subtype-oid value)
  (let ((payload (encode-value registry subtype-oid value :format 1)))
    (when (or (null payload) (sql-null-p payload))
      (error 'parameter-error :parameter value
             :message "PostgreSQL range bounds must not be SQL NULL"))
    (%type-encoded-octets payload value)))

(defun %decode-range-binary (registry subtype-oid octets)
  (let ((position 0)
        (length (length octets)))
    (let ((flags (%octet-at octets position)))
      (incf position)
      (unless (zerop (logand flags #xe0))
        (error 'protocol-error :message "PostgreSQL range flags are invalid"
               :context :range :actual flags))
      (if (logbitp 0 flags)
          (progn
            (unless (= flags +range-empty-flag+)
              (error 'protocol-error :message "PostgreSQL empty range has invalid flags"
                     :context :range :actual flags))
            (unless (= position length)
              (error 'protocol-error :message "PostgreSQL empty range has trailing bytes"
                     :context :range :expected position :actual length))
            (make-postgres-range :empty-p t))
          (let ((lower-infinite-p (logbitp 3 flags))
                (upper-infinite-p (logbitp 4 flags))
                (lower nil)
                (upper nil))
            (when (and lower-infinite-p (logbitp 1 flags))
              (error 'protocol-error :message "PostgreSQL lower range bound is both infinite and inclusive"
                     :context :range))
            (when (and upper-infinite-p (logbitp 2 flags))
              (error 'protocol-error :message "PostgreSQL upper range bound is both infinite and inclusive"
                     :context :range))
            (unless lower-infinite-p
              (multiple-value-bind (payload-length next)
                  (%read-i32 octets position)
                (setf position next)
                (when (minusp payload-length)
                  (error 'protocol-error :message "PostgreSQL range lower bound length is invalid"
                         :context :range :actual payload-length))
                (when (> payload-length (- length position))
                  (error 'protocol-error :message "PostgreSQL range lower bound exceeds its payload"
                         :context :range :expected (- length position)
                         :actual payload-length))
                (setf lower
                      (decode-value registry subtype-oid
                                    (subseq octets position (+ position payload-length))
                                    :format 1))
                (incf position payload-length)))
            (unless upper-infinite-p
              (multiple-value-bind (payload-length next)
                  (%read-i32 octets position)
                (setf position next)
                (when (minusp payload-length)
                  (error 'protocol-error :message "PostgreSQL range upper bound length is invalid"
                         :context :range :actual payload-length))
                (when (> payload-length (- length position))
                  (error 'protocol-error :message "PostgreSQL range upper bound exceeds its payload"
                         :context :range :expected (- length position)
                         :actual payload-length))
                (setf upper
                      (decode-value registry subtype-oid
                                    (subseq octets position (+ position payload-length))
                                    :format 1))
                (incf position payload-length)))
            (unless (= position length)
              (error 'protocol-error :message "PostgreSQL range has trailing bytes"
                     :context :range :expected position :actual length))
            (make-postgres-range
             :lower lower
             :upper upper
             :lower-inclusive (logbitp 1 flags)
             :upper-inclusive (logbitp 2 flags)))))))

(defun %encode-range-binary (registry subtype-oid value)
  (unless (postgres-range-p value)
    (error 'parameter-error :parameter value
           :message "PostgreSQL range values must be postgres-range instances"))
  (let ((builder (make-octet-builder)))
    (if (postgres-range-empty-p value)
        (append-u8 builder +range-empty-flag+)
        (let ((flags 0))
          (when (postgres-range-lower-inclusive value)
            (setf flags (logior flags +range-lower-inclusive-flag+)))
          (when (postgres-range-upper-inclusive value)
            (setf flags (logior flags +range-upper-inclusive-flag+)))
          (unless (postgres-range-lower value)
            (setf flags (logior flags +range-lower-infinite-flag+)))
          (unless (postgres-range-upper value)
            (setf flags (logior flags +range-upper-infinite-flag+)))
          (append-u8 builder flags)
          (unless (logbitp 3 flags)
            (let ((payload (%range-binary-payload
                            registry subtype-oid
                            (postgres-range-lower value))))
              (append-i32 builder (length payload))
              (append-octets builder payload)))
          (unless (logbitp 4 flags)
            (let ((payload (%range-binary-payload
                            registry subtype-oid
                            (postgres-range-upper value))))
              (append-i32 builder (length payload))
              (append-octets builder payload)))))
    (builder-octets builder)))

(defun %decode-multirange-binary (registry subtype-oid octets)
  (multiple-value-bind (range-count position)
      (%read-i32 octets 0)
    (when (minusp range-count)
      (error 'protocol-error
             :message "PostgreSQL multirange range count is negative"
             :context :multirange :actual range-count))
    (let ((ranges nil)
          (length (length octets)))
      (loop repeat range-count
            do (multiple-value-bind (range-length next)
                   (%read-i32 octets position)
                 (setf position next)
                 (when (minusp range-length)
                   (error 'protocol-error
                          :message "PostgreSQL multirange range length is negative"
                          :context :multirange :actual range-length))
                 (when (> range-length (- length position))
                   (error 'protocol-error
                          :message "PostgreSQL multirange range exceeds its payload"
                          :context :multirange
                          :expected (- length position)
                          :actual range-length))
                 (let ((range (%decode-range-binary
                               registry subtype-oid
                               (subseq octets position (+ position range-length)))))
                   ;; PostgreSQL discards empty ranges while constructing a
                   ;; multirange.  Keep the public value canonical as well.
                   (unless (postgres-range-empty-p range)
                     (push range ranges)))
                 (incf position range-length)))
      (unless (= position length)
        (error 'protocol-error
               :message "PostgreSQL multirange has trailing bytes"
               :context :multirange :expected position :actual length))
      (make-postgres-multirange :ranges (nreverse ranges)))))

(defun %encode-multirange-binary (registry subtype-oid value)
  (unless (postgres-multirange-p value)
    (error 'parameter-error :parameter value
           :message "PostgreSQL multirange values must be postgres-multirange instances"))
  (let ((ranges (postgres-multirange-ranges value))
        (builder (make-octet-builder)))
    (append-i32 builder (length ranges))
    (loop for range across ranges
          do (let ((payload (%encode-range-binary registry subtype-oid range)))
               (append-i32 builder (length payload))
               (append-octets builder payload)))
    (builder-octets builder)))
