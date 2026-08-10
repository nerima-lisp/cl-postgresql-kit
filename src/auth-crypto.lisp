(in-package #:cl-postgresql-kit)

(defun %secure-random-octets (length)
  #+sbcl
  (let ((result (make-array length :element-type '(unsigned-byte 8))))
    (with-open-file (stream "/dev/urandom"
                            :direction :input
                            :element-type '(unsigned-byte 8))
      (unless (= (read-sequence result stream) length)
        (error 'authentication-error
               :message "The operating system did not provide enough secure randomness.")))
    result)
  #-sbcl
  (error 'unsupported-feature
         :feature :secure-random
         :message "Secure SCRAM randomness is unavailable on this implementation."))

(defun %constant-time-string= (left right)
  (and (= (length left) (length right))
       (zerop (loop for left-char across left
                    for right-char across right
                    sum (logxor (char-code left-char) (char-code right-char))))))

(defun %constant-time-octets= (left right)
  (and (= (length left) (length right))
       (zerop (loop for left-octet across left
                    for right-octet across right
                    sum (logxor left-octet right-octet)))))

(defun %md5-password-response (password user salt)
  (let* ((first (md5-digest (%string-to-octets (format nil "~A~A" password user))))
         (first-hex (%octets-to-lower-hex first))
         (second (md5-digest
                  (concatenate '(vector (unsigned-byte 8))
                               (%string-to-octets first-hex)
                               salt))))
    (concatenate 'string "md5" (%octets-to-lower-hex second))))

(defun %string-replace-characters (string replacements)
  (with-output-to-string (stream)
    (loop for character across string
          do (let ((replacement (assoc character replacements)))
               (if replacement
                   (write-string (cdr replacement) stream)
                   (write-char character stream))))))

(defun %scram-username (user)
  (%string-replace-characters user '((#\= . "=3D") (#\, . "=2C"))))

(defun %split-comma-fields (string)
  (loop with start = 0
        for comma = (position #\, string :start start)
        collect (subseq string start comma)
        while comma
        do (setf start (1+ comma))))

(defun %scram-attributes (string &key allowed-names)
  (unless (and (stringp string)
               (<= (length string) *maximum-scram-message-length*))
    (error 'authentication-error :message "SCRAM message exceeds the configured limit."))
  (let ((fields (%split-comma-fields string))
        (seen (make-hash-table :test #'equal))
        (attributes nil))
    (when (> (length fields) *maximum-scram-attribute-count*)
      (error 'authentication-error :message "SCRAM message has too many attributes."))
    (dolist (field fields (nreverse attributes))
      (when (> (length field) *maximum-scram-attribute-length*)
        (error 'authentication-error :message "SCRAM attribute exceeds the configured limit."))
      (let ((separator (position #\= field)))
        (unless (and separator
                     (plusp separator)
                     (< (1+ separator) (length field)))
          (error 'authentication-error :message "Malformed SCRAM attribute."))
        (let ((name (subseq field 0 separator))
              (value (subseq field (1+ separator))))
          (when (gethash name seen)
            (error 'authentication-error
                   :message "A SCRAM attribute was specified more than once."))
          (when (and allowed-names
                     (not (member name allowed-names :test #'string=)))
            (error 'authentication-error
                   :message "The server sent an unknown SCRAM attribute."))
          (setf (gethash name seen) t)
          (push (cons name value) attributes))))))

(defun %scram-attribute (attributes name)
  (cdr (assoc name attributes :test #'string=)))

(defun %random-scram-nonce ()
  (string-right-trim "="
                     (octets-to-base64 (%secure-random-octets 18))))

(defun %xor-octet-vectors (left right)
  (unless (= (length left) (length right))
    (error 'protocol-error :message "SCRAM operands have different lengths."))
  (let ((result (make-array (length left) :element-type '(unsigned-byte 8))))
    (loop for index below (length result)
          do (setf (aref result index)
                   (logxor (aref left index) (aref right index))))
    result))
