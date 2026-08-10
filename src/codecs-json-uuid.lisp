(in-package #:cl-postgresql-kit)

(defun %uuid-string (value)
  (let ((string (cond ((typep value 'uuid-value) (uuid-value-value value))
                      ((stringp value) value)
                      (t (error 'parameter-error :parameter value
                                :message "PostgreSQL UUID values must be strings or UUID values")))))
    (unless (= (length string) 36)
      (error 'parameter-error :parameter value
             :message "PostgreSQL UUID values must contain 36 characters"))
    (loop for position from 0 below (length string)
          for character = (char string position)
          do (if (member position '(8 13 18 23))
                 (unless (char= character #\-)
                   (error 'parameter-error :parameter value
                          :message "PostgreSQL UUID values have invalid separators"))
                 (unless (digit-char-p character 16)
                   (error 'parameter-error :parameter value
                          :message "PostgreSQL UUID values contain a non-hex digit"))))
    (string-downcase string)))

(defun %uuid-octets (value)
  (let* ((string (remove #\- (%uuid-string value)))
         (result (make-array 16 :element-type '(unsigned-byte 8)))
         (position 0))
    (loop for index below 16
          do (setf (aref result index)
                   (+ (ash (digit-char-p (char string position) 16) 4)
                      (digit-char-p (char string (1+ position)) 16)))
             (incf position 2))
    result))

(defun %uuid-octets-string (octets)
  (unless (and (vectorp octets)
               (= (length octets) 16)
               (every (lambda (octet) (and (integerp octet) (<= 0 octet 255)))
                      octets))
    (error 'protocol-error :message "Invalid PostgreSQL UUID binary length or octet"
           :context :type-decoder :expected 16
           :actual (and (vectorp octets) (length octets))))
  (string-downcase
   (with-output-to-string (stream)
     (loop for octet across octets
           for position from 0
           do (when (member position '(4 6 8 10))
                (write-char #\- stream))
              (format stream "~2,'0X" octet)))))

(defun %decode-uuid-text (octets)
  (handler-case
      (make-uuid-value :value (%uuid-string (%decode-utf8 octets)))
    (parameter-error ()
      (error 'protocol-error :message "Invalid PostgreSQL UUID text payload"
             :context :type-decoder))))

(defun %encode-uuid-text (value)
  (%encode-utf8 (%uuid-string value)))

(defun %decode-uuid-binary (octets)
  (make-uuid-value :value (%uuid-octets-string octets)))

(defun %encode-uuid-binary (value)
  (%uuid-octets value))

(defun %decode-bytea-legacy (string)
  (let ((result (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0))
        (position 0))
    (labels ((append-character (character)
               (loop for octet across
                         (cl-codec-kit:string-to-octets (string character)
                                                         :encoding :utf-8)
                     do (vector-push-extend octet result))))
      (loop while (< position (length string))
            do (if (char/= (char string position) #\\)
                   (progn
                     (append-character (char string position))
                     (incf position))
                   (progn
                     (incf position)
                     (when (>= position (length string))
                       (error 'protocol-error
                              :message "Invalid PostgreSQL bytea escape"
                              :context :type-decoder))
                     (if (char= (char string position) #\\)
                         (progn
                           (vector-push-extend 92 result)
                           (incf position))
                         (let ((start position)
                               (end position))
                           (loop while (and (< end (length string))
                                            (< (- end start) 3)
                                            (digit-char-p (char string end) 8))
                                 do (incf end))
                           (when (= start end)
                             (error 'protocol-error
                                    :message "Invalid PostgreSQL bytea escape"
                                    :context :type-decoder))
                           (let ((value 0))
                             (loop for index from start below end
                                   do (setf value (+ (* value 8)
                                                     (digit-char-p
                                                      (char string index) 8))))
                             (when (> value 255)
                               (error 'protocol-error
                                      :message "PostgreSQL bytea escape is out of range"
                                      :context :type-decoder))
                             (vector-push-extend value result))
                           (setf position end)))))))
    result))

(defun %decode-bytea-text (octets)
  (let ((string (%decode-utf8 octets)))
    (if (and (>= (length string) 2) (string= "\\x" string :end2 2))
        (let* ((hex (subseq string 2))
               (hex-length (length hex)))
          (unless (evenp hex-length)
            (error 'protocol-error
                   :message "The PostgreSQL bytea hex payload has odd length."
                   :context :type-decoder))
          (let ((result (make-array (/ hex-length 2)
                                    :element-type '(unsigned-byte 8))))
            (loop for index from 0 below hex-length by 2
                  for output from 0
                  for high = (digit-char-p (char hex index) 16)
                  for low = (digit-char-p (char hex (1+ index)) 16)
                  do (unless (and high low)
                       (error 'protocol-error
                              :message "The PostgreSQL bytea hex payload contains a non-hex digit."
                              :context :type-decoder))
                     (setf (aref result output) (+ (* 16 high) low)))
            result))
        (%decode-bytea-legacy string))))

(defun %encode-bytea-text (value)
  (let ((octets (if (typep value 'bytea-value)
                    (bytea-value-value value)
                    (%crypto-octets value))))
    (with-output-to-string (stream)
      (write-string "\\x" stream)
      (loop for octet across octets
            do (format stream "~2,'0X" octet)))))

(defun %decode-json (octets)
  (make-json-value :value (json-kit:parse (%decode-utf8 octets))))

(defun %encode-json (value)
  (let ((value (if (typep value 'json-value) (json-value-value value) value)))
    (%encode-utf8 (json-kit:stringify value))))

(defun %decode-jsonb (octets)
  (when (< (length octets) 1)
    (error 'protocol-error :message "JSONB payload is missing its version byte"
           :context :jsonb))
  (unless (= (aref octets 0) 1)
    (error 'protocol-error :message "Unsupported JSONB binary version"
           :context :jsonb :expected 1 :actual (aref octets 0)))
  (%decode-json (subseq octets 1)))

(defun %encode-jsonb (value)
  (let* ((payload (%encode-json value))
         (result (make-array (1+ (length payload))
                             :element-type '(unsigned-byte 8))))
    (setf (aref result 0) 1)
    (replace result payload :start1 1)
    result))
