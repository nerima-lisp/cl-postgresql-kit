(in-package #:cl-postgresql-kit)

(defun %round-positive-ratio-to-even (numerator denominator)
  (multiple-value-bind (quotient remainder)
      (floor numerator denominator)
    (let ((twice-remainder (* 2 remainder)))
      (cond ((< twice-remainder denominator) quotient)
            ((> twice-remainder denominator) (1+ quotient))
            ((evenp quotient) quotient)
            (t (1+ quotient))))))

(defun %scale-binary-float-significand (significand shift)
  (if (>= shift 0)
      (ash significand shift)
      (%round-positive-ratio-to-even significand
                                     (ash 1 (- shift)))))

(defun %binary-float-octets (bits total-bits)
  (let ((builder (make-octet-builder (/ total-bits 8))))
    (loop for shift from (- total-bits 8) downto 0 by 8
          do (append-u8 builder (ldb (byte 8 shift) bits)))
    (coerce (builder-octets builder) '(vector (unsigned-byte 8)))))

(defun %decode-binary-float (octets total-bits exponent-bits fraction-bits
                              target-type)
  (let ((expected-size (/ total-bits 8)))
    (unless (and (vectorp octets)
                 (every (lambda (octet)
                          (and (integerp octet) (<= 0 octet 255)))
                        octets)
                 (= (length octets) expected-size))
      (error 'protocol-error
             :message "Invalid PostgreSQL floating-point binary length."
             :context :type-decoder
             :expected expected-size
             :actual (and (vectorp octets) (length octets))))
    (let* ((bits (%decode-binary-unsigned octets expected-size))
           (sign-bit (ldb (byte 1 (1- total-bits)) bits))
           (exponent-mask (1- (ash 1 exponent-bits)))
           (fraction-mask (1- (ash 1 fraction-bits)))
           (exponent (ldb (byte exponent-bits fraction-bits) bits))
           (fraction (logand bits fraction-mask))
           (bias (1- (ash 1 (1- exponent-bits))))
           (maximum-exponent exponent-mask))
      (cond ((= exponent maximum-exponent)
             (if (zerop fraction)
                 (if (zerop sign-bit) "Infinity" "-Infinity")
                 "NaN"))
            ((and (zerop exponent) (zerop fraction))
             (let ((zero (coerce 0 target-type)))
               (if (zerop sign-bit) zero (- zero))))
            (t
             (let* ((unbiased-exponent
                    (if (zerop exponent)
                          (- 1 bias)
                          (- exponent bias)))
                    (significand
                      (if (zerop exponent)
                          fraction
                          (+ (ash 1 fraction-bits) fraction)))
                    (value
                      (scale-float
                       (coerce significand target-type)
                       (- unbiased-exponent fraction-bits))))
               (if (zerop sign-bit) value (- value))))))))

(defun %encode-binary-float (value total-bits exponent-bits fraction-bits
                              target-type)
  (let* ((special (%float-special-value value))
         (bias (1- (ash 1 (1- exponent-bits))))
         (maximum-exponent (1- (ash 1 exponent-bits)))
         (minimum-normal-exponent (- 1 bias))
         (sign-bit 0)
         (exponent-field 0)
         (fraction 0))
    (cond
      (special
       (setf sign-bit (if (string= special "-Infinity") 1 0)
             exponent-field maximum-exponent
             fraction (if (string= special "NaN") 1 0)))
      ((not (realp value))
       (error 'parameter-error :parameter value
              :message "PostgreSQL floating-point values must be real numbers or Infinity/NaN."))
      (t
       (handler-case
           (let ((float (coerce value target-type)))
             (multiple-value-bind (significand exponent sign)
                 (integer-decode-float float)
               (unless (and (= (float-radix float) 2)
                            (= (float-digits float) (1+ fraction-bits)))
                 (error 'parameter-error :parameter value
                        :message "The implementation float format is not IEEE-754 binary."))
               (setf sign-bit (if (minusp sign) 1 0))
               (unless (zerop significand)
                 (let* ((significand-bits (integer-length significand))
                        (unbiased-exponent
                          (+ exponent (1- significand-bits))))
                   (unless (<= significand-bits (1+ fraction-bits))
                     (error 'parameter-error :parameter value
                            :message "The implementation float significand is too wide for IEEE-754."))
                   (when (> unbiased-exponent bias)
                     (error 'parameter-error :parameter value
                            :message "PostgreSQL floating-point value overflows the target format."))
                   (if (>= unbiased-exponent minimum-normal-exponent)
                       (let ((normalized-significand
                               (ash significand
                                    (- (1+ fraction-bits)
                                       significand-bits))))
                         (setf exponent-field (+ unbiased-exponent bias)
                               fraction (- normalized-significand
                                           (ash 1 fraction-bits))))
                       (setf fraction
                             (%scale-binary-float-significand
                              significand
                              (+ exponent fraction-bits
                                 (- minimum-normal-exponent)))))))))
         (parameter-error (condition)
           (error condition))
         (error (condition)
           (error 'parameter-error :parameter value
                  :message "Unable to encode PostgreSQL floating-point value."
                  :cause condition)))))
    (%binary-float-octets
     (logior (ash sign-bit (1- total-bits))
             (ash exponent-field fraction-bits)
             fraction)
     total-bits)))

(defun %decode-binary-boolean (octets)
  (unless (and (vectorp octets) (= (length octets) 1))
    (error 'protocol-error :message "Invalid PostgreSQL boolean binary length"
           :context :type-decoder :expected 1
           :actual (and (vectorp octets) (length octets))))
  (case (aref octets 0)
    (0 nil)
    (1 t)
    (otherwise
     (error 'protocol-error :message "Invalid PostgreSQL boolean binary value"
            :context :type-decoder))))

(defun %encode-binary-boolean (value)
  (vector (if value 1 0)))
