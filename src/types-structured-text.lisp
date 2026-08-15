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

(defun %type-text-needs-quote-p (string)
  (or (string= string "")
      (string= string "NULL")
      (string= string "empty")
      (some (lambda (character)
              (or (%space-character-p character)
                  (char= character #\")
                  (char= character #\\)
                  (char= character #\,)
                  (char= character #\[)
                  (char= character #\])
                  (char= character (char "(" 0))))
            string)))

(defun %write-type-quoted-string (stream string)
  (write-char #\" stream)
  (loop for character across string
        do (when (or (char= character #\")
                     (char= character #\\))
             (write-char #\\ stream))
           (write-char character stream))
  (write-char #\" stream))

(defun %write-type-token (stream string)
  (if (%type-text-needs-quote-p string)
      (%write-type-quoted-string stream string)
      (write-string string stream)))
