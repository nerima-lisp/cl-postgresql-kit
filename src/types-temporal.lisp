(in-package #:cl-postgresql-kit)

(defparameter *postgres-epoch-day*
  (cl-date-kit:local-date-to-epoch-day
   (cl-date-kit:local-date-of 2000 1 1)))

(defparameter *postgres-microseconds-per-day* (* 86400 1000000))

(defparameter *postgres-date-negative-infinity* (- (ash 1 31)))

(defparameter *postgres-date-infinity* (1- (ash 1 31)))

(defparameter *postgres-timestamp-negative-infinity* (- (ash 1 63)))

(defparameter *postgres-timestamp-infinity* (1- (ash 1 63)))

(defparameter *postgres-epoch-unix-microseconds*
  (* (- *postgres-epoch-day*
        (cl-date-kit:local-date-to-epoch-day
         (cl-date-kit:local-date-of 1970 1 1)))
     *postgres-microseconds-per-day*))

(defun %parse-temporal-parameter (parser value message)
  (handler-case
      (funcall parser value)
    (error (condition)
      (error 'parameter-error :parameter value
             :message message
             :cause condition))))

(defun %date-binary-value (value)
  (let ((value (if (date-value-p value) (date-value-value value) value)))
    (cond ((and (stringp value) (string-equal value "infinity")) value)
          ((and (stringp value) (string-equal value "-infinity")) value)
          ((cl-date-kit:local-date-p value) value)
          ((stringp value)
           (%parse-temporal-parameter #'cl-date-kit:parse-local-date value
                                      "Invalid PostgreSQL date value"))
          (t (error 'parameter-error :parameter value
                    :message "PostgreSQL date values must be dates or strings")))))

(defun %time-binary-value (value)
  (let ((value (if (time-value-p value) (time-value-value value) value)))
    (cond ((cl-date-kit:local-time-p value) value)
          ((stringp value)
           (%parse-temporal-parameter #'cl-date-kit:parse-local-time value
                                      "Invalid PostgreSQL time value"))
          (t (error 'parameter-error :parameter value
                    :message "PostgreSQL time values must be times or strings")))))

(defun %timestamp-binary-value (value)
  (let ((value (if (timestamp-value-p value)
                   (timestamp-value-value value)
                   value)))
    (cond ((and (stringp value) (string-equal value "infinity")) value)
          ((and (stringp value) (string-equal value "-infinity")) value)
          ((cl-date-kit:local-date-time-p value) value)
          ((stringp value)
           (%parse-temporal-parameter #'cl-date-kit:parse-local-date-time value
                                      "Invalid PostgreSQL timestamp value"))
          (t (error 'parameter-error :parameter value
                    :message "PostgreSQL timestamp values must be date-times or strings")))))

(defun %timestamptz-binary-value (value)
  (let ((value (if (timestamptz-value-p value)
                   (timestamptz-value-value value)
                   value)))
    (cond ((and (stringp value) (string-equal value "infinity")) value)
          ((and (stringp value) (string-equal value "-infinity")) value)
          ((cl-date-kit:instant-p value) value)
          ((stringp value)
           (%parse-temporal-parameter #'cl-date-kit:parse-instant value
                                      "Invalid PostgreSQL timestamptz value"))
          (t (error 'parameter-error :parameter value
                    :message "PostgreSQL timestamptz values must be instants or strings")))))

(defun %decode-date-binary (octets)
  (let ((days (%decode-binary-signed octets 4)))
    (cond ((= days *postgres-date-negative-infinity*)
           (make-date-value :value "-infinity"))
          ((= days *postgres-date-infinity*)
           (make-date-value :value "infinity"))
          (t
           (handler-case
               (make-date-value
                :value (cl-date-kit:local-date-from-epoch-day
                        (+ *postgres-epoch-day* days)))
             (error (condition)
               (error 'protocol-error
                      :message "Invalid PostgreSQL date binary payload"
                      :context :type-decoder
                      :cause condition)))))))

(defun %encode-date-binary (value)
  (let ((value (%date-binary-value value)))
    (%encode-binary-signed
     (cond ((and (stringp value) (string-equal value "-infinity"))
            *postgres-date-negative-infinity*)
           ((and (stringp value) (string-equal value "infinity"))
            *postgres-date-infinity*)
           (t (- (cl-date-kit:local-date-to-epoch-day value)
                 *postgres-epoch-day*)))
     4)))

(defun %decode-time-binary (octets)
  (let ((microseconds (%decode-binary-signed octets 8)))
    (unless (<= 0 microseconds (1- *postgres-microseconds-per-day*))
      (error 'protocol-error
             :message "PostgreSQL time binary payload is outside one day"
             :context :type-decoder
             :expected (list 0 (1- *postgres-microseconds-per-day*))
             :actual microseconds))
    (handler-case
        (make-time-value
         :value (cl-date-kit:local-time-of-nano-of-day (* microseconds 1000)))
      (error (condition)
        (error 'protocol-error
               :message "Invalid PostgreSQL time binary payload"
               :context :type-decoder
               :cause condition)))))

(defun %encode-time-binary (value)
  (let* ((value (%time-binary-value value))
         (microseconds
           (floor (cl-date-kit:local-time-to-nano-of-day value) 1000)))
    (%encode-binary-signed microseconds 8)))

(defun %decode-timetz-binary (octets)
  (unless (%wire-octet-vector-p octets)
    (error 'protocol-error
           :message "Invalid PostgreSQL timetz binary payload"
           :context :type-decoder))
  (unless (= (length octets) 12)
    (error 'protocol-error
           :message "PostgreSQL timetz binary payload has an invalid length"
           :context :type-decoder
           :expected 12
           :actual (length octets)))
  (let ((microseconds (%decode-binary-signed (subseq octets 0 8) 8))
        (timezone-seconds (%decode-binary-signed (subseq octets 8 12) 4)))
    (unless (<= 0 microseconds +postgres-microseconds-per-day+)
      (error 'protocol-error
             :message "PostgreSQL timetz time is outside one day"
             :context :type-decoder
             :expected (list 0 +postgres-microseconds-per-day+)
             :actual microseconds))
    (unless (<= (- (1- +postgres-timetz-zone-limit+))
                timezone-seconds
                (1- +postgres-timetz-zone-limit+))
      (error 'protocol-error
             :message "PostgreSQL timetz zone is outside its wire range"
             :context :type-decoder
             :expected (list (- (1- +postgres-timetz-zone-limit+))
                             (1- +postgres-timetz-zone-limit+))
             :actual timezone-seconds))
    (make-timetz-value
     :value (make-postgres-time-with-time-zone
             :microseconds microseconds
             :timezone-seconds timezone-seconds))))

(defun %timetz-binary-value (value)
  (let ((value (if (timetz-value-p value)
                   (timetz-value-value value)
                   value)))
    (unless (postgres-time-with-time-zone-p value)
      (error 'parameter-error
             :parameter value
             :message
             "PostgreSQL binary timetz values must be postgres-time-with-time-zone instances"))
    value))

(defun %encode-timetz-binary (value)
  (let* ((value (%timetz-binary-value value))
         (builder (make-octet-builder 12)))
    (append-i64 builder (postgres-time-with-time-zone-microseconds value))
    (append-i32 builder (postgres-time-with-time-zone-timezone-seconds value))
    (builder-octets builder)))

(defun %decode-timestamp-binary (octets)
  (let ((microseconds (%decode-binary-signed octets 8)))
    (cond ((= microseconds *postgres-timestamp-negative-infinity*)
           (make-timestamp-value :value "-infinity"))
          ((= microseconds *postgres-timestamp-infinity*)
           (make-timestamp-value :value "infinity"))
          (t
           (handler-case
               (multiple-value-bind (days remainder)
                   (floor microseconds *postgres-microseconds-per-day*)
                 (make-timestamp-value
                  :value
                  (cl-date-kit:make-local-date-time
                   (cl-date-kit:local-date-from-epoch-day
                    (+ *postgres-epoch-day* days))
                   (cl-date-kit:local-time-of-nano-of-day
                    (* remainder 1000)))))
             (error (condition)
               (error 'protocol-error
                      :message "Invalid PostgreSQL timestamp binary payload"
                      :context :type-decoder
                      :cause condition)))))))

(defun %encode-timestamp-binary (value)
  (let ((value (%timestamp-binary-value value)))
    (%encode-binary-signed
     (cond ((and (stringp value) (string-equal value "-infinity"))
            *postgres-timestamp-negative-infinity*)
           ((and (stringp value) (string-equal value "infinity"))
            *postgres-timestamp-infinity*)
           (t
            (let ((date (cl-date-kit:local-date-time-date value))
                  (time (cl-date-kit:local-date-time-time value)))
              (+ (* (- (cl-date-kit:local-date-to-epoch-day date)
                       *postgres-epoch-day*)
                    *postgres-microseconds-per-day*)
                 (floor (cl-date-kit:local-time-to-nano-of-day time) 1000)))))
     8)))

(defun %decode-timestamptz-binary (octets)
  (let ((microseconds (%decode-binary-signed octets 8)))
    (cond ((= microseconds *postgres-timestamp-negative-infinity*)
           (make-timestamptz-value :value "-infinity"))
          ((= microseconds *postgres-timestamp-infinity*)
           (make-timestamptz-value :value "infinity"))
          (t
           (handler-case
               (make-timestamptz-value
                :value
                (cl-date-kit:instant-of-epoch-micros
                 (+ *postgres-epoch-unix-microseconds* microseconds)))
             (error (condition)
               (error 'protocol-error
                      :message "Invalid PostgreSQL timestamptz binary payload"
                      :context :type-decoder
                      :cause condition)))))))

(defun %encode-timestamptz-binary (value)
  (let ((value (%timestamptz-binary-value value)))
    (%encode-binary-signed
     (cond ((and (stringp value) (string-equal value "-infinity"))
            *postgres-timestamp-negative-infinity*)
           ((and (stringp value) (string-equal value "infinity"))
            *postgres-timestamp-infinity*)
           (t (- (cl-date-kit:instant-to-epoch-micros value)
                 *postgres-epoch-unix-microseconds*)))
     8)))

(defun %decode-interval-binary (octets)
  (unless (%wire-octet-vector-p octets)
    (error 'protocol-error
           :message "Invalid PostgreSQL interval binary payload"
           :context :type-decoder))
  (unless (= (length octets) 16)
    (error 'protocol-error
           :message "PostgreSQL interval binary payload has an invalid length"
           :context :type-decoder
           :expected 16
           :actual (length octets)))
  (make-interval-value
   :value (make-postgres-interval
           :microseconds (%decode-binary-signed (subseq octets 0 8) 8)
           :days (%decode-binary-signed (subseq octets 8 12) 4)
           :months (%decode-binary-signed (subseq octets 12 16) 4))))

(defun %interval-binary-value (value)
  (let ((value (if (interval-value-p value)
                   (interval-value-value value)
                   value)))
    (unless (postgres-interval-p value)
      (error 'parameter-error
             :parameter value
             :message "PostgreSQL binary interval values must be postgres-interval instances"))
    value))

(defun %encode-interval-binary (value)
  (let* ((value (%interval-binary-value value))
         (builder (make-octet-builder 16)))
    (append-i64 builder (postgres-interval-microseconds value))
    (append-i32 builder (postgres-interval-days value))
    (append-i32 builder (postgres-interval-months value))
    (builder-octets builder)))

(defun %decode-date (octets)
  (make-date-value :value
                   (cl-date-kit:parse-local-date (%decode-utf8 octets))))

(defun %encode-date (value)
  (%encode-utf8
   (cl-date-kit:format-local-date
    (if (typep value 'date-value) (date-value-value value) value))))

(defun %decode-time (octets)
  (make-time-value :value
                   (cl-date-kit:parse-local-time (%decode-utf8 octets))))

(defun %encode-time (value)
  (%encode-utf8
   (cl-date-kit:format-local-time
    (if (typep value 'time-value) (time-value-value value) value))))

(defun %decode-timestamp (octets)
  (make-timestamp-value :value
   (cl-date-kit:parse-local-date-time (%decode-utf8 octets))))

(defun %encode-timestamp (value)
  (%encode-utf8
   (cl-date-kit:format-local-date-time
    (if (typep value 'timestamp-value) (timestamp-value-value value) value))))

(defun %decode-timestamptz (octets)
  (make-timestamptz-value :value
   (cl-date-kit:parse-instant (%decode-utf8 octets))))

(defun %encode-timestamptz (value)
  (%encode-utf8
   (cl-date-kit:format-instant
    (if (typep value 'timestamptz-value)
        (timestamptz-value-value value)
        value))))

(defun %decode-interval (octets)
  (make-interval-value :value (%decode-utf8 octets)))

(defun %encode-interval (value)
  (%encode-utf8
   (if (typep value 'interval-value) (interval-value-value value) value)))
