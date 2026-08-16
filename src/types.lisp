(in-package #:cl-postgresql-kit)

(defparameter *maximum-array-dimensions* 6)
(defparameter *maximum-array-elements* 1000000)
(defparameter *maximum-numeric-digits* 100000)

(defun register-type (registry &key codec oid name text-decoder text-encoder
                               binary-decoder binary-encoder)
  (check-type registry type-registry)
  (let ((codec (or codec
                   (make-type-codec :oid oid :name name
                                    :text-decoder text-decoder
                                    :text-encoder text-encoder
                                    :binary-decoder binary-decoder
                                    :binary-encoder binary-encoder))))
    (unless (typep codec 'type-codec)
      (error 'parameter-error :parameter codec
             :message "TYPE-CODEC must be a type-codec instance"))
    (let ((codec-oid (type-codec-oid codec))
          (codec-name (type-codec-name codec)))
      (unless (and (integerp codec-oid) (<= 0 codec-oid))
        (error 'parameter-error :parameter codec-oid
               :message "TYPE-CODEC OID must be a non-negative integer."))
      (unless (and (stringp codec-name) (plusp (length codec-name)))
        (error 'parameter-error :parameter codec-name
               :message "TYPE-CODEC name must be a non-empty string."))
      (let ((name-key (string-downcase codec-name)))
        (cl-concurrent-kit:with-lock-held ((type-registry-lock registry))
          (let ((old-by-oid (gethash codec-oid
                                     (type-registry-by-oid registry)))
                (old-by-name (gethash name-key
                                      (type-registry-by-name registry))))
            (when old-by-oid
              (let ((old-name-key
                      (string-downcase (type-codec-name old-by-oid))))
                (when (eq (gethash old-name-key
                                   (type-registry-by-name registry))
                          old-by-oid)
                  (remhash old-name-key (type-registry-by-name registry)))))
            (when old-by-name
              (let ((old-oid (type-codec-oid old-by-name)))
                (when (eq (gethash old-oid (type-registry-by-oid registry))
                          old-by-name)
                  (remhash old-oid (type-registry-by-oid registry)))))
            (setf (gethash codec-oid (type-registry-by-oid registry)) codec
                  (gethash name-key (type-registry-by-name registry)) codec))))
    codec)))

(defun find-type-codec (registry type)
  (check-type registry type-registry)
  (cl-concurrent-kit:with-lock-held ((type-registry-lock registry))
    (etypecase type
      (integer (gethash type (type-registry-by-oid registry)))
      (string (gethash (string-downcase type) (type-registry-by-name registry)))
      (symbol (gethash (string-downcase (symbol-name type))
                       (type-registry-by-name registry))))))
(defstruct timestamptz-value value)

(defun make-postgres-range (&key lower upper lower-inclusive upper-inclusive
                                  empty-p)
  (%make-postgres-range :lower lower
                        :upper upper
                        :lower-inclusive lower-inclusive
                        :upper-inclusive upper-inclusive
                        :empty-p empty-p))

(defun make-postgres-interval (&key (months 0) (days 0) (microseconds 0))
  (labels ((ensure-component (value parameter minimum maximum)
             (unless (and (integerp value) (<= minimum value maximum))
               (error 'parameter-error
                      :parameter parameter
                      :message "PostgreSQL interval component is outside its wire range"))))
    (ensure-component months :months (- (ash 1 31)) (1- (ash 1 31)))
    (ensure-component days :days (- (ash 1 31)) (1- (ash 1 31)))
    (ensure-component microseconds :microseconds
                       (- (ash 1 63))
                       (1- (ash 1 63)))
    (%make-postgres-interval :months months
                             :days days
                             :microseconds microseconds)))

(defun make-postgres-time-with-time-zone (&key (microseconds 0)
                                               (timezone-seconds 0))
  (labels ((ensure-component (value parameter minimum maximum)
             (unless (and (integerp value) (<= minimum value maximum))
               (error 'parameter-error
                      :parameter parameter
                      :message "PostgreSQL timetz component is outside its wire range"))))
    (ensure-component microseconds :microseconds
                      0
                      +postgres-microseconds-per-day+)
    (ensure-component timezone-seconds :timezone-seconds
                      (- (1- +postgres-timetz-zone-limit+))
                      (1- +postgres-timetz-zone-limit+))
    (%make-postgres-time-with-time-zone
     :microseconds microseconds
     :timezone-seconds timezone-seconds)))

(defun make-postgres-tid (&key (block-number 0) (offset-number 0))
  (unless (and (integerp block-number)
               (<= 0 block-number (1- (ash 1 32))))
    (error 'parameter-error :parameter block-number
           :message "PostgreSQL TID block number is outside its wire range"))
  (unless (and (integerp offset-number)
               (<= 0 offset-number (1- (ash 1 16))))
    (error 'parameter-error :parameter offset-number
           :message "PostgreSQL TID offset number is outside its wire range"))
  (%make-postgres-tid :block-number block-number
                      :offset-number offset-number))

(defun %postgres-inet-octets-vector (value)
  (let ((raw (%copy-type-definition-vector
              value :octets
              "PostgreSQL inet octets must be a list or vector")))
    (let ((octets (make-array (length raw)
                              :element-type '(unsigned-byte 8))))
      (loop for octet across raw
            for index from 0
            do (unless (and (integerp octet) (<= 0 octet 255))
                 (error 'parameter-error :parameter value
                        :message
                        "PostgreSQL inet octets must be unsigned bytes"))
               (setf (aref octets index) octet))
      octets)))

(defun %postgres-inet-host-bits-zero-p (octets netmask)
  (loop for bit-index from netmask below (* 8 (length octets))
        always (zerop
                (ldb (byte 1 (- 7 (mod bit-index 8)))
                     (aref octets (floor bit-index 8))))))

(defun make-postgres-inet (&key octets family netmask (cidr-p nil))
  (let* ((octets (%postgres-inet-octets-vector octets))
         (octet-count (length octets))
         (inferred-family
           (case octet-count
             (4 4)
             (16 6)
             (otherwise
              (error 'parameter-error :parameter octets
                     :message
                     "PostgreSQL inet values must contain 4 or 16 octets"))))
         (family (or family inferred-family))
         (maximum-netmask
           (case family
             (4 32)
             (6 128)
             (otherwise
              (error 'parameter-error :parameter family
                     :message "PostgreSQL inet family must be 4 or 6"))))
         (netmask (if (null netmask) maximum-netmask netmask))
         (cidr-p (not (null cidr-p))))
    (unless (= octet-count (if (= family 4) 4 16))
      (error 'parameter-error :parameter octets
             :message "PostgreSQL inet octet count does not match its family"))
    (unless (and (integerp netmask) (<= 0 netmask maximum-netmask))
      (error 'parameter-error :parameter netmask
             :message "PostgreSQL inet netmask is outside its family range"))
    (when (and cidr-p (not (%postgres-inet-host-bits-zero-p octets netmask)))
      (error 'parameter-error :parameter octets
             :message "PostgreSQL cidr values must have zero host bits"))
    (%make-postgres-inet :octets octets
                         :family family
                         :netmask netmask
                         :cidr-p cidr-p)))

(defun make-postgres-multirange (&key (ranges #()))
  (let ((ranges (%copy-type-definition-vector
                 ranges :ranges
                 "PostgreSQL multirange ranges must be a list or vector")))
    (when (> (length ranges) *maximum-array-elements*)
      (error 'parameter-error :parameter ranges
             :message "PostgreSQL multirange has too many ranges"))
    (loop for range across ranges
          do (unless (postgres-range-p range)
               (error 'parameter-error :parameter range
                      :message "PostgreSQL multirange ranges must be postgres-range instances"))
             (when (postgres-range-empty-p range)
               (error 'parameter-error :parameter range
                      :message "PostgreSQL multirange ranges must not contain empty ranges")))
    (%make-postgres-multirange :ranges ranges)))

(defun %ensure-type-definition-oid (value parameter)
  (unless (and (integerp value) (<= 0 value))
    (error 'parameter-error :parameter parameter
           :message "PostgreSQL type OID must be a non-negative integer."))
  value)

(defun %ensure-type-definition-name (value parameter)
  (unless (and (stringp value) (plusp (length value))
               (not (find #\Null value)))
    (error 'parameter-error :parameter parameter
           :message "PostgreSQL type name must be a non-empty string without NUL."))
  value)

(defun %copy-type-definition-vector (value parameter message)
  (cond ((and (vectorp value) (not (stringp value)))
         (copy-seq value))
        ((and (listp value) (not (stringp value)))
         (coerce value 'vector))
        (t
         (error 'parameter-error :parameter parameter :message message))))

(defun %ensure-type-definition-oids (value parameter message)
  (let ((result (%copy-type-definition-vector value parameter message)))
    (loop for oid across result
          do (%ensure-type-definition-oid oid parameter))
    result))
