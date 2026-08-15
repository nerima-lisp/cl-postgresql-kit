(in-package #:cl-postgresql-kit)

(defun decode-value (registry oid octets &key (format 0))
  (unless (member format '(0 1))
    (error 'parameter-error :parameter format
           :message "PostgreSQL format must be 0 (text) or 1 (binary)"))
  (let ((codec (find-type-codec registry oid)))
    (if (or (null octets) (sql-null-p octets))
        +sql-null+
        (let ((decoder (if (zerop format)
                           (and codec (type-codec-text-decoder codec))
                           (and codec (type-codec-binary-decoder codec)))))
          (if decoder
              (funcall decoder octets)
              octets)))))

(defun encode-value (registry oid value &key (format 0))
  (unless (member format '(0 1))
    (error 'parameter-error :parameter format
           :message "PostgreSQL format must be 0 (text) or 1 (binary)"))
  (when (sql-null-p value)
    (return-from encode-value nil))
  (let* ((codec (find-type-codec registry oid))
         (encoder (if (zerop format)
                      (and codec (type-codec-text-encoder codec))
                      (and codec (type-codec-binary-encoder codec)))))
    (cond (encoder (funcall encoder value))
          ((typep value '(vector (unsigned-byte 8))) value)
          (t (%encode-utf8 value)))))
