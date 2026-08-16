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

(defun %timetz-decimal-value (string start end)
  (when (>= start end)
    (error "PostgreSQL timetz has an empty numeric component."))
  (loop with value = 0
        for index from start below end
        for digit = (- (char-code (char string index)) (char-code #\0))
        do (unless (<= 0 digit 9)
             (error "PostgreSQL timetz has a non-decimal numeric component."))
           (setf value (+ (* value 10) digit))
        finally (return value)))

(defun %timetz-fraction-value (string start end)
  (let ((digits (- end start)))
    (unless (<= 1 digits 6)
      (error "PostgreSQL timetz fractional seconds must have one to six digits."))
    (* (%timetz-decimal-value string start end)
       (expt 10 (- 6 digits)))))

(defun %timetz-clock-value (string start end)
  (let ((colon-count
          (loop for index from start below end
                count (char= (char string index) #\:) into count
                finally (return count))))
    (labels ((component (component-start component-end expected-digits)
               (unless (= (- component-end component-start) expected-digits)
                 (error "PostgreSQL timetz has an invalid clock component."))
               (%timetz-decimal-value string component-start component-end))
             (seconds-component (component-start)
               (let ((dot (position #\. string :start component-start :end end)))
                 (if dot
                     (values (component component-start dot 2)
                             (%timetz-fraction-value string (1+ dot) end))
                     (values (component component-start end 2) 0)))))
      (multiple-value-bind (hour minute second microseconds)
          (cond
            ((zerop colon-count)
             (let ((dot (position #\. string :start start :end end)))
               (when (and dot (< dot (+ start 6)))
                 (error "PostgreSQL timetz has an invalid compact clock value."))
               (let* ((clock-end (or dot end))
                      (digits (- clock-end start)))
                 (cond
                   ((= digits 4)
                    (values (component start (+ start 2) 2)
                            (component (+ start 2) clock-end 2)
                            0
                            (if dot
                                (%timetz-fraction-value string (1+ dot) end)
                                0)))
                   ((= digits 6)
                    (values (component start (+ start 2) 2)
                            (component (+ start 2) (+ start 4) 2)
                            (component (+ start 4) clock-end 2)
                            (if dot
                                (%timetz-fraction-value string (1+ dot) end)
                                0)))
                   (t
                    (error "PostgreSQL timetz has an invalid compact clock value."))))))
            ((= colon-count 1)
             (let ((colon (position #\: string :start start :end end)))
               (values (component start colon 2)
                       (component (1+ colon) end 2)
                       0
                       0)))
            ((= colon-count 2)
             (let* ((first-colon (position #\: string :start start :end end))
                    (second-colon (position #\: string :start (1+ first-colon)
                                             :end end)))
               (multiple-value-bind (second fraction)
                   (seconds-component (1+ second-colon))
                 (values (component start first-colon 2)
                         (component (1+ first-colon) second-colon 2)
                         second
                         fraction))))
            (t
             (error "PostgreSQL timetz has too many clock separators.")))
        (unless (and (<= 0 hour 24)
                     (<= 0 minute 59)
                     (<= 0 second 59))
          (error "PostgreSQL timetz has an out-of-range clock value."))
        (when (and (= hour 24)
                   (or (plusp minute) (plusp second) (plusp microseconds)))
          (error "PostgreSQL timetz only permits 24:00:00 as a 24-hour value."))
        (values (+ (* hour 60 60 1000000)
                   (* minute 60 1000000)
                   (* second 1000000)
                   microseconds))))))

(defun %timetz-zone-value (string start end)
  (when (and (= (- end start) 1)
             (char-equal (char string start) #\z))
    (return-from %timetz-zone-value 0))
  (unless (and (> (- end start) 1)
               (member (char string start) '(#\+ #\-)))
    (error "PostgreSQL timetz requires a UTC offset."))
  (let* ((sign (char string start))
         (zone-start (1+ start))
         (colon-count
           (loop for index from zone-start below end
                 count (char= (char string index) #\:) into count
                 finally (return count)))
         (hour 0)
         (minute 0)
         (second 0))
    (cond
      ((zerop colon-count)
       (let ((digits (- end zone-start)))
         (unless (member digits '(2 4))
           (error "PostgreSQL timetz has an invalid compact UTC offset."))
         (setf hour (%timetz-decimal-value string zone-start (+ zone-start 2)))
         (when (= digits 4)
           (setf minute (%timetz-decimal-value string (+ zone-start 2) end)))))
      ((= colon-count 1)
       (let ((colon (position #\: string :start zone-start :end end)))
         (setf hour (%timetz-decimal-value string zone-start colon)
               minute (%timetz-decimal-value string (1+ colon) end))))
      ((= colon-count 2)
       (let* ((first-colon (position #\: string :start zone-start :end end))
              (second-colon (position #\: string :start (1+ first-colon)
                                       :end end)))
         (setf hour (%timetz-decimal-value string zone-start first-colon)
               minute (%timetz-decimal-value string (1+ first-colon) second-colon)
               second (%timetz-decimal-value string (1+ second-colon) end))))
      (t
       (error "PostgreSQL timetz has too many UTC offset separators.")))
    (unless (and (<= 0 hour 15)
                 (<= 0 minute 59)
                 (<= 0 second 59))
      (error "PostgreSQL timetz has an out-of-range UTC offset."))
    (let ((offset-seconds (+ (* hour 60 60) (* minute 60) second)))
      (unless (< offset-seconds +postgres-timetz-zone-limit+)
        (error "PostgreSQL timetz UTC offset exceeds sixteen hours."))
      (if (char= sign #\+)
          (- offset-seconds)
          offset-seconds))))

(defun %parse-timetz-text (string)
  (unless (stringp string)
    (error "PostgreSQL timetz value must be a string."))
  (let* ((length (length string))
         (zone-start
           (loop for index from 1 below length
                 for character = (char string index)
                 when (member character '(#\+ #\-))
                   return index))
         (zone-suffix-p
           (and (> length 0)
                (char-equal (char string (1- length)) #\z)))
         (clock-end (cond (zone-start zone-start)
                          (zone-suffix-p (1- length))
                          (t (error "PostgreSQL timetz requires a UTC offset."))))
         (zone (if zone-start
                   (%timetz-zone-value string zone-start length)
                   0)))
    (multiple-value-bind (microseconds)
        (%timetz-clock-value string 0 clock-end)
      (values microseconds zone))))

(defun %decode-timetz (octets)
  (handler-case
      (multiple-value-bind (microseconds zone)
          (%parse-timetz-text (%decode-utf8 octets))
        (make-timetz-value
         :value (make-postgres-time-with-time-zone
                 :microseconds microseconds
                 :timezone-seconds zone)))
    (error ()
      (error 'protocol-error
             :message "Invalid PostgreSQL timetz text payload."
             :context :timetz))))

(defun %timetz-parameter-value (value)
  (let ((value (if (typep value 'timetz-value)
                   (timetz-value-value value)
                   value)))
    (cond
      ((typep value 'postgres-time-with-time-zone)
       value)
      ((stringp value)
       (handler-case
           (multiple-value-bind (microseconds zone)
               (%parse-timetz-text value)
             (make-postgres-time-with-time-zone
              :microseconds microseconds
              :timezone-seconds zone))
         (error ()
           (error 'parameter-error
                  :parameter value
                  :message "Invalid PostgreSQL timetz parameter."))))
      (t
       (error 'parameter-error
              :parameter value
              :message "PostgreSQL timetz requires a string or time-with-zone value.")))))

(defun %format-timetz-text (value)
  (let* ((time (if (typep value 'timetz-value)
                   (timetz-value-value value)
                   value))
         (microseconds (postgres-time-with-time-zone-microseconds time))
         (zone-seconds (postgres-time-with-time-zone-timezone-seconds time)))
    (multiple-value-bind (seconds fraction) (floor microseconds 1000000)
      (multiple-value-bind (hours remainder) (floor seconds 3600)
        (multiple-value-bind (minutes clock-seconds) (floor remainder 60)
          (let* ((fraction-text
                   (if (zerop fraction)
                       ""
                       (format nil ".~A"
                               (string-right-trim
                                "0"
                                (format nil "~6,'0D" fraction)))))
                 (east-zone-seconds (- zone-seconds))
                 (zone-sign (if (minusp east-zone-seconds) #\- #\+))
                 (absolute-zone-seconds (abs east-zone-seconds)))
            (multiple-value-bind (zone-hours zone-remainder)
                (floor absolute-zone-seconds 3600)
              (multiple-value-bind (zone-minutes zone-seconds)
                  (floor zone-remainder 60)
                (concatenate
                 'string
                 (format nil "~2,'0D:~2,'0D:~2,'0D~A"
                         hours minutes clock-seconds fraction-text)
                 (cond
                   ((zerop (mod absolute-zone-seconds 3600))
                    (format nil "~C~2,'0D" zone-sign zone-hours))
                   ((zerop zone-seconds)
                    (format nil "~C~2,'0D:~2,'0D"
                            zone-sign zone-hours zone-minutes))
                   (t
                    (format nil "~C~2,'0D:~2,'0D:~2,'0D"
                            zone-sign zone-hours zone-minutes zone-seconds))))))))))))

(defun %encode-timetz (value)
  (%encode-utf8
   (%format-timetz-text (%timetz-parameter-value value))))

(defun %decode-interval (octets)
  (make-interval-value :value (%decode-utf8 octets)))

(defun %encode-interval (value)
  (%encode-utf8
   (if (typep value 'interval-value) (interval-value-value value) value)))
