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
