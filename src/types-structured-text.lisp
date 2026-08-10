(in-package #:cl-postgresql-kit)

(defun %type-octet-vector-p (value)
  (and (vectorp value)
       (every (lambda (octet)
                (and (integerp octet) (<= 0 octet 255)))
              value)))

(defun %type-encoded-octets (value parameter)
  (cond ((stringp value) (%encode-utf8 value))
        ((%type-octet-vector-p value) (coerce value 'vector))
        (t (error 'parameter-error :parameter parameter
                  :message "PostgreSQL type codec must return text or octets"))))

(defun %type-encoded-text (value context)
  (cond ((stringp value) value)
        ((%type-octet-vector-p value)
         (handler-case
             (%decode-utf8 value)
           (error (condition)
             (error 'protocol-error
                    :message "PostgreSQL type text is not valid UTF-8"
                    :context context
                    :actual condition))))
        (t (error 'protocol-error
                  :message "PostgreSQL type codec returned invalid text"
                  :context context
                  :actual value))))

(defun %type-trim (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return #\Page) string))

(defun %unquote-type-token (token &key (null-as-sql-null nil))
  (let* ((string (%type-trim token))
         (length (length string)))
    (if (zerop length)
        (values (if null-as-sql-null +sql-null+ "") nil)
        (if (char= (char string 0) #\")
            (let ((position 1)
                  (closed-p nil))
              (values
               (with-output-to-string (stream)
                 (loop while (< position length)
                       do (let ((character (char string position)))
                            (cond ((char= character #\\)
                                   (incf position)
                                   (when (>= position length)
                                     (error 'protocol-error
                                            :message "PostgreSQL composite/range quote is unterminated"
                                            :context :type-decoder))
                                   (write-char (char string position) stream)
                                   (incf position))
                                  ((char= character #\")
                                   (incf position)
                                   (setf closed-p t)
                                   (return))
                                  (t
                                   (write-char character stream)
                                   (incf position)))))
                 (unless closed-p
                   (error 'protocol-error
                          :message "PostgreSQL composite/range quote is unterminated"
                          :context :type-decoder))
                 (loop while (and (< position length)
                                  (%space-character-p (char string position)))
                       do (incf position))
                 (unless (= position length)
                   (error 'protocol-error
                          :message "PostgreSQL composite/range quoted token has trailing data"
                          :context :type-decoder)))
               t))
            (progn
              (when (find #\" string)
                (error 'protocol-error
                       :message "PostgreSQL composite/range token has an invalid quote"
                       :context :type-decoder))
              (values
               (with-output-to-string (stream)
                 (let ((position 0))
                   (loop while (< position length)
                         do (let ((character (char string position)))
                              (if (char= character #\\)
                                  (progn
                                    (incf position)
                                    (when (>= position length)
                                      (error 'protocol-error
                                             :message "PostgreSQL composite/range escape is unterminated"
                                             :context :type-decoder))
                                    (write-char (char string position) stream))
                                  (write-char character stream))
                              (incf position))))
               (and null-as-sql-null (string= string "NULL")))))))))

(defun %split-type-delimited (string delimiter context)
  (let ((start 0)
        (quoted-p nil)
        (escaped-p nil)
        (result '()))
    (loop for position below (length string)
          for character = (char string position)
          do (cond (escaped-p
                    (setf escaped-p nil))
                   ((char= character #\\)
                    (setf escaped-p t))
                   ((char= character #\")
                    (setf quoted-p (not quoted-p)))
                   ((and (not quoted-p) (char= character delimiter))
                    (push (subseq string start position) result)
                    (setf start (1+ position)))))
    (when (or quoted-p escaped-p)
      (error 'protocol-error
             :message "PostgreSQL composite/range token is unterminated"
             :context context))
    (nreverse (cons (subseq string start) result))))

(defun %parse-range-bound-token (token)
  (if (zerop (length (%type-trim token)))
      (values nil nil)
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

(defun %parse-composite-text (string)
  (let ((string (%type-trim string)))
    (when (or (< (length string) 2)
              (not (char= (char string 0) #\())
              (not (char= (char string (1- (length string))) #\))))
      (error 'protocol-error :message "PostgreSQL composite text has invalid delimiters"
             :context :type-decoder))
    (let ((body (subseq string 1 (1- (length string)))))
      (if (zerop (length body))
          #()
          (coerce
           (loop for token in (%split-type-delimited body #\, :type-decoder)
                 collect (multiple-value-bind (value quoted-p)
                             (%unquote-type-token token :null-as-sql-null t)
                           (declare (ignore quoted-p))
                           value))
           'vector)))))

(defun %type-text-needs-quote-p (string)
  (or (zerop (length string))
      (string= string "NULL")
      (string= string "empty")
      (some (lambda (character)
              (or (%space-character-p character)
                  (member character '(#\" #\\ #\, #\[ #\] #\( #\)))))
            string)))

(defun %write-type-quoted-string (stream string)
  (write-char #\" stream)
  (loop for character across string
        do (when (member character '(#\" #\\))
             (write-char #\\ stream))
           (write-char character stream))
  (write-char #\" stream))

(defun %write-type-token (stream string)
  (if (%type-text-needs-quote-p string)
      (%write-type-quoted-string stream string)
      (write-string string stream)))

(defun %range-bound-text (registry subtype-oid value)
  (let ((payload (encode-value registry subtype-oid value :format 0)))
    (when (or (null payload) (sql-null-p payload))
      (error 'parameter-error :parameter value
             :message "PostgreSQL range bounds must not be SQL NULL"))
    (%type-encoded-text payload :range)))

(defun %decode-range-bound-text (registry subtype-oid value present-p)
  (if present-p
      (decode-value registry subtype-oid (%encode-utf8 value) :format 0)
      nil))

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
          (if after-comma-p
              (invalid "PostgreSQL multirange text has a trailing comma")
              (return (nreverse items))))
        (let ((start position)
              (character (char string position)))
          (if (member character '(#\[ #\())
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
                                       (member item-character '(#\] #\))))
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
