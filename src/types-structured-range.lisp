(in-package #:cl-postgresql-kit)

(defconstant +range-empty-flag+ #x01)

(defconstant +range-lower-inclusive-flag+ #x02)

(defconstant +range-upper-inclusive-flag+ #x04)

(defconstant +range-lower-infinite-flag+ #x08)

(defconstant +range-upper-infinite-flag+ #x10)

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
                (when (< payload-length 0)
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
                (when (< payload-length 0)
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
    (when (< range-count 0)
      (error 'protocol-error
             :message "PostgreSQL multirange range count is negative"
             :context :multirange :actual range-count))
    (let ((ranges nil)
          (length (length octets)))
      (loop repeat range-count
            do (multiple-value-bind (range-length next)
                   (%read-i32 octets position)
                 (setf position next)
                 (when (< range-length 0)
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
