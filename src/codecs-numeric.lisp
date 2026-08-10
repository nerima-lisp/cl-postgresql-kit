(in-package #:cl-postgresql-kit)

(defun %numeric-string-components (value)
  (let* ((string (string-trim '(#\Space #\Tab #\Newline #\Return #\Page)
                              value))
         (special (%float-special-value string)))
    (when special
      (return-from %numeric-string-components
        (values :special special nil nil nil)))
    (let ((length (length string))
          (position 0)
          (negative nil)
          (fraction-digits 0)
          (seen-digit nil)
          (seen-dot nil)
          (digits (make-array 0 :element-type 'character
                                :adjustable t :fill-pointer 0)))
      (when (zerop length)
        (error 'protocol-error :message "Invalid PostgreSQL numeric value."
               :context :type-decoder))
      (when (member (char string position) '(#\+ #\-))
        (setf negative (char= (char string position) #\-))
        (incf position))
      (loop while (< position length)
            for character = (char string position)
            do (cond
                 ((digit-char-p character 10)
                  (vector-push-extend character digits)
                  (setf seen-digit t)
                  (when seen-dot
                    (incf fraction-digits))
                  (incf position))
                 ((char= character #\.)
                  (when seen-dot
                    (error 'protocol-error
                           :message "Invalid PostgreSQL numeric value."
                           :context :type-decoder))
                  (setf seen-dot t)
                  (incf position))
                 (t (return))))
      (unless seen-digit
        (error 'protocol-error :message "Invalid PostgreSQL numeric value."
               :context :type-decoder))
      (let ((exponent 0)
            (exponent-negative nil))
        (when (< position length)
          (unless (member (char string position) '(#\e #\E))
            (error 'protocol-error :message "Invalid PostgreSQL numeric value."
                   :context :type-decoder))
          (incf position)
          (when (and (< position length)
                     (member (char string position) '(#\+ #\-)))
            (setf exponent-negative (char= (char string position) #\-))
            (incf position))
          (let ((exponent-digit-seen nil))
            (loop while (< position length)
                  for character = (char string position)
                  for digit = (digit-char-p character 10)
                  do (unless digit (return))
                     (setf exponent-digit-seen t)
                     (setf exponent (+ (* exponent 10) digit))
                     (when (> exponent *maximum-numeric-digits*)
                       (error 'protocol-error
                              :message "PostgreSQL numeric exponent is too large."
                              :context :type-decoder))
                     (incf position))
            (unless exponent-digit-seen
              (error 'protocol-error :message "Invalid PostgreSQL numeric exponent."
                     :context :type-decoder))))
        (unless (= position length)
          (error 'protocol-error :message "Invalid PostgreSQL numeric value."
                 :context :type-decoder))
        (when exponent-negative
          (setf exponent (- exponent)))
        (let* ((digits (coerce digits 'string))
               (scale (- fraction-digits exponent)))
          (when (> (abs scale) *maximum-numeric-digits*)
            (error 'protocol-error
                   :message "PostgreSQL numeric scale is too large."
                   :context :type-decoder))
          (when (every (lambda (character) (char= character #\0)) digits)
            (setf negative nil))
          (values :finite nil (if negative -1 1) digits scale))))))

(defun %numeric-value-from-components (sign digits scale)
  (let ((coefficient (parse-integer digits :radix 10)))
    (if (zerop coefficient)
        0
        (let ((value (if (plusp scale)
                         (/ coefficient (expt 10 scale))
                         (* coefficient (expt 10 (- scale))))))
          (if (minusp sign) (- value) value)))))

(defun %decode-numeric (value)
  (let ((string (%decode-utf8 value)))
    (handler-case
        (multiple-value-bind (kind special sign digits scale)
            (%numeric-string-components string)
          (if (eq kind :special)
              special
              (%numeric-value-from-components sign digits scale)))
      (protocol-error (condition)
        (error condition))
      (error (condition)
        (error 'protocol-error :message "Invalid PostgreSQL numeric value."
               :context :type-decoder :cause condition)))))

(defun %numeric-decimal-string (coefficient scale)
  (if (zerop coefficient)
      "0"
      (let* ((prefix (if (minusp coefficient) "-" ""))
             (digits (princ-to-string (abs coefficient)))
             (length (length digits)))
        (if (zerop scale)
            (concatenate 'string prefix digits)
            (if (<= length scale)
                (concatenate 'string prefix "0."
                             (make-string (- scale length)
                                           :initial-element #\0)
                             digits)
                (let ((split (- length scale)))
                  (concatenate 'string prefix
                               (subseq digits 0 split)
                               "."
                               (subseq digits split))))))))

(defun %numeric-rational-string (value)
  (let ((numerator (numerator value))
        (denominator (denominator value)))
    (if (= denominator 1)
        (princ-to-string numerator)
        (let ((remaining denominator)
              (twos 0)
              (fives 0))
          (loop while (evenp remaining)
                do (incf twos)
                   (setf remaining (/ remaining 2)))
          (loop while (zerop (mod remaining 5))
                do (incf fives)
                   (setf remaining (/ remaining 5)))
          (unless (= remaining 1)
            (error 'parameter-error :parameter value
                   :message "PostgreSQL numeric rationals must have a terminating decimal representation."))
          (let* ((scale (max twos fives))
                 (scaled (* numerator
                            (ash 1 (- scale twos))
                            (expt 5 (- scale fives)))))
            (%numeric-decimal-string scaled scale))))))

(defun %numeric-input-string (value)
  (cond ((stringp value)
         (string-trim '(#\Space #\Tab #\Newline #\Return #\Page) value))
        ((rationalp value) (%numeric-rational-string value))
        ((realp value) (format nil "~,17G" value))
        (t (error 'parameter-error :parameter value
                  :message "PostgreSQL numeric values must be numbers or numeric strings."))))

(defun %encode-numeric (value)
  (let ((string (%numeric-input-string value)))
    (handler-case
        (multiple-value-bind (kind special sign digits scale)
            (%numeric-string-components string)
          (declare (ignore sign digits scale))
          (%encode-utf8 (if (eq kind :special) special string)))
      (protocol-error (condition)
        (error 'parameter-error :parameter value
               :message "Invalid PostgreSQL numeric value."
               :cause condition)))))

(defun %numeric-binary-groups (digits scale)
  (let* ((decimal-position (- (length digits) scale))
         (leading (or (position-if-not (lambda (character)
                                         (char= character #\0))
                                       digits)
                      (length digits))))
    (if (= leading (length digits))
        (values #() 0)
        (progn
          (setf decimal-position (- decimal-position leading)
                digits (subseq digits leading))
          (let* ((left-padding (mod (- decimal-position) 4))
                 (left (concatenate 'string
                                    (make-string left-padding
                                                 :initial-element #\0)
                                    digits))
                 (right-padding (mod (- (length left)) 4))
                 (padded (concatenate 'string left
                                      (make-string right-padding
                                                   :initial-element #\0)))
                 (group-position
                   (/ (+ decimal-position left-padding) 4))
                 (weight (1- group-position))
                 (groups (make-array (/ (length padded) 4))))
            (loop for index below (length groups)
                  do (setf (aref groups index)
                           (parse-integer padded
                                          :start (* index 4)
                                          :end (* (1+ index) 4)
                                          :radix 10)))
            (let ((first (position-if-not #'zerop groups))
                  (last (position-if-not #'zerop groups :from-end t)))
              (values (subseq groups first (1+ last))
                      (- weight first))))))))

(defun %encode-numeric-binary (value)
  (let ((string (%numeric-input-string value)))
    (handler-case
        (multiple-value-bind (kind special sign digits scale)
            (%numeric-string-components string)
          (if (eq kind :special)
              (if (string= special "NaN")
                  (let ((builder (make-octet-builder 8)))
                    (append-i16 builder 0)
                    (append-i16 builder 0)
                    (append-i16 builder -16384)
                    (append-i16 builder 0)
                    (coerce (builder-octets builder)
                            '(vector (unsigned-byte 8))))
                  (error 'parameter-error :parameter value
                         :message "PostgreSQL binary numeric values support NaN, not Infinity."))
              (multiple-value-bind (groups weight)
                  (%numeric-binary-groups digits scale)
                (let ((display-scale (max scale 0)))
                  (when (or (> (length groups) 32767)
                            (> display-scale 32767)
                            (not (<= -32768 weight 32767)))
                    (error 'parameter-error :parameter value
                           :message "PostgreSQL binary numeric value is out of range."))
                  (let ((builder (make-octet-builder (+ 8 (* 2 (length groups))))))
                    (append-i16 builder (length groups))
                    (append-i16 builder weight)
                    (append-i16 builder (if (minusp sign) 16384 0))
                    (append-i16 builder display-scale)
                    (loop for digit across groups
                          do (append-u16 builder digit))
                    (coerce (builder-octets builder)
                            '(vector (unsigned-byte 8))))))))
      (protocol-error (condition)
        (error 'parameter-error :parameter value
               :message "Invalid PostgreSQL numeric value."
               :cause condition)))))

(defun %decode-numeric-binary (octets)
  (unless (and (vectorp octets)
               (every (lambda (octet) (and (integerp octet) (<= 0 octet 255)))
                      octets)
               (>= (length octets) 8))
    (error 'protocol-error :message "Invalid PostgreSQL numeric binary payload."
           :context :type-decoder))
  (let ((position 0)
        (length (length octets)))
    (flet ((read-i16 ()
             (prog1 (%read-i16 octets position)
               (incf position 2)))
           (read-u16 ()
             (prog1 (%read-u16 octets position)
               (incf position 2))))
      (let ((ndigits (read-i16))
            (weight (read-i16))
            (sign (read-i16))
            (scale (read-i16)))
        (unless (and (<= 0 ndigits *maximum-numeric-digits*)
                     (<= 0 scale 32767)
                     (= length (+ 8 (* 2 ndigits)))
                     (member sign '(0 16384 -16384)))
          (error 'protocol-error :message "Invalid PostgreSQL numeric binary header."
                 :context :type-decoder))
        (when (= sign -16384)
          (unless (zerop ndigits)
            (error 'protocol-error :message "Invalid PostgreSQL numeric NaN payload."
                   :context :type-decoder))
          (return-from %decode-numeric-binary "NaN"))
        (let ((coefficient 0))
          (loop repeat ndigits
                for digit = (read-u16)
                do (unless (<= digit 9999)
                     (error 'protocol-error
                            :message "Invalid PostgreSQL numeric base-10000 digit."
                            :context :type-decoder))
                   (setf coefficient (+ (* coefficient 10000) digit)))
          (let* ((exponent (- weight (1- ndigits)))
                 (value (if (plusp exponent)
                            (* coefficient (expt 10000 exponent))
                            (if (minusp exponent)
                                (/ coefficient (expt 10000 (- exponent)))
                                coefficient))))
            (if (and (= sign 16384) (not (zerop value)))
                (- value)
                value)))))))
