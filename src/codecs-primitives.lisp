(in-package #:cl-postgresql-kit)

(defun %decode-utf8 (octets)
  (cl-codec-kit:octets-to-string octets :encoding :utf-8))

(defun %encode-utf8 (value)
  (cl-codec-kit:string-to-octets (if (stringp value) value (princ-to-string value))
                                 :encoding :utf-8))

(defun %decode-integer (value &optional minimum maximum)
  (let ((number
          (handler-case
              (parse-integer (%decode-utf8 value) :junk-allowed nil)
            (error (condition)
              (declare (ignore condition))
              (error 'protocol-error :message "Invalid PostgreSQL integer payload"
                     :context :type-decoder)))))
    (when (and minimum maximum
               (not (<= minimum number maximum)))
      (error 'protocol-error :message "PostgreSQL integer payload is out of range"
             :context :type-decoder :expected (list minimum maximum)
             :actual number))
    number))

(defun %encode-integer (value &optional minimum maximum)
  (unless (and (integerp value)
               (or (null minimum) (null maximum)
                   (<= minimum value maximum)))
    (error 'parameter-error :parameter value
           :message "PostgreSQL integer value is out of range"))
  (%encode-utf8 (princ-to-string value)))

(defun %space-character-p (character)
  (find character '(#\Space #\Tab #\Newline #\Return #\Page)
        :test #'char=))

(defun %decode-space-separated-integers (octets minimum maximum)
  (let* ((string (%decode-utf8 octets))
         (length (length string))
         (position 0)
         (result (make-array 0 :element-type 'integer
                               :adjustable t :fill-pointer 0)))
    (loop
      (loop while (and (< position length)
                       (%space-character-p (char string position)))
            do (incf position))
      (when (>= position length)
        (return))
      (let ((start position))
        (loop while (and (< position length)
                         (not (%space-character-p (char string position))))
              do (incf position))
        (handler-case
            (let ((number (parse-integer string :start start :end position
                                         :junk-allowed nil)))
              (unless (<= minimum number maximum)
                (error 'protocol-error
                       :message "PostgreSQL integer vector element is out of range"
                       :context :type-decoder
                       :expected (list minimum maximum)
                       :actual number))
              (vector-push-extend number result))
          (protocol-error (condition)
            (error condition))
          (error (condition)
            (error 'protocol-error
                   :message "Invalid PostgreSQL integer vector payload"
                   :context :type-decoder
                   :cause condition)))))
    result))

(defun %encode-space-separated-integers (value minimum maximum)
  (let ((sequence
          (cond ((and (vectorp value) (not (stringp value))) value)
                ((listp value) (coerce value 'vector))
                (t (error 'parameter-error
                          :parameter value
                          :message "PostgreSQL integer vectors must be lists or vectors")))))
    (loop for number across sequence
          unless (and (integerp number) (<= minimum number maximum))
            do (error 'parameter-error
                      :parameter number
                      :message "PostgreSQL integer vector element is out of range"))
    (%encode-utf8
     (with-output-to-string (stream)
       (loop for index below (length sequence)
             do (when (plusp index)
                  (write-char #\Space stream))
                (princ (aref sequence index) stream))))))

(defun %float-special-value (value)
  (when (stringp value)
    (let ((string (string-trim '(#\Space #\Tab #\Newline #\Return #\Page)
                               value)))
      (cond ((string-equal string "Infinity") "Infinity")
            ((string-equal string "+Infinity") "Infinity")
            ((string-equal string "-Infinity") "-Infinity")
            ((string-equal string "NaN") "NaN")))))

(defun %decode-float (value)
  (let ((string (%decode-utf8 value)))
    (handler-case
        (or (%float-special-value string)
            (let ((string (string-trim
                           '(#\Space #\Tab #\Newline #\Return #\Page)
                           string)))
              (when (zerop (length string))
                (error 'protocol-error
                       :message "Invalid PostgreSQL floating-point value."
                       :context :type-decoder))
              (let ((*read-eval* nil))
                (multiple-value-bind (number position)
                    (read-from-string string nil nil)
                  (if (and (realp number)
                           position
                           (= position (length string)))
                      number
                      (error 'protocol-error
                             :message "Invalid PostgreSQL floating-point value."
                             :context :type-decoder))))))
      (protocol-error (condition)
        (error condition))
      (error (condition)
        (error 'protocol-error
               :message "Invalid PostgreSQL floating-point value."
               :context :type-decoder
               :cause condition)))))

(defun %encode-float (value)
  (let ((special (%float-special-value value)))
    (%encode-utf8
     (cond (special)
           ((realp value) (format nil "~,17G" value))
           (t (error 'parameter-error :parameter value
                     :message "PostgreSQL floating-point values must be real numbers or Infinity/NaN."))))))
