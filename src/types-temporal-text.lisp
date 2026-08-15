(in-package #:cl-postgresql-kit)

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
