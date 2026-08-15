(in-package #:cl-postgresql-kit)

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
