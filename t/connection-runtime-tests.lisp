(in-package #:cl-postgresql-kit/test)

(deftest transport-base-boundaries-and-tls-option-validation
  (let ((transport (make-instance 'transport)))
    (is (not (transport-alive-p transport)))
    (is (eq transport (transport-open transport)))
    (is (transport-alive-p transport))
    (is (zerop (length (transport-read-available transport))))
    (is (not (transport-wait-readable transport)))
    (is (null (transport-channel-binding-data transport)))
    (is (transport-flush transport))
    (assert-signals 'unsupported-feature
                   (lambda ()
                     (transport-start-tls transport
                                           :hostname "db.example"
                                           :verify t)))
    (is (eq transport (transport-close transport)))
    (is (not (transport-alive-p transport))))
  (let ((transport
          (make-socket-transport
           :host "db.example"
           :port 5433
           :timeout 5
           :tls-options '(:certificate #p"client.crt"
                          :key #p"client.key"
                          :password nil
                          :alpn-protocols ("postgresql")
                          :cipher-list "HIGH"
                          :method :tlsv1.3
                          :verify-location #p"ca.crt"))))
    (is (equal '(:certificate "client.crt"
                 :key "client.key"
                 :password nil
                 :alpn-protocols ("postgresql")
                 :cipher-list "HIGH"
                 :method :tlsv1.3
                 :verify-location "ca.crt")
               (socket-transport-tls-options transport))))
  (it-signals-each 'parameter-error
      ((:invalid-alpn-plist)
       (:non-string-certificate)
       (:non-string-password)
       (:non-string-verify-location)
       (:invalid-verify-location-list)
       (:unsupported-option)
       (:duplicate-certificate)
       (:missing-certificate-value)
       (:not-a-plist)
       (:missing-key-pair)
       (:dotted-options-tail)
       (:dotted-verify-location-tail)
       (:dotted-alpn-tail)
       (:circular-options-list))
    "rejects invalid TLS option case ~A"
    (label)
    (case label
      (:invalid-alpn-plist
       (cl-postgresql-kit::%normalize-tls-options
        '(:alpn-protocols ("postgresql" 1))))
      (:non-string-certificate
       (cl-postgresql-kit::%normalize-tls-options
        '(:certificate 42)))
      (:non-string-password
       (cl-postgresql-kit::%normalize-tls-options
        '(:password 42)))
      (:non-string-verify-location
       (cl-postgresql-kit::%normalize-tls-options
        '(:verify-location 42)))
      (:invalid-verify-location-list
       (cl-postgresql-kit::%normalize-tls-options
        '(:verify-location ("ca.crt" 1))))
      (:unsupported-option
       (cl-postgresql-kit::%normalize-tls-options
        '(:unsupported "x")))
      (:duplicate-certificate
       (cl-postgresql-kit::%normalize-tls-options
        '(:certificate "a" :certificate "b")))
      (:missing-certificate-value
       (cl-postgresql-kit::%normalize-tls-options
        '(:certificate)))
      (:not-a-plist
       (cl-postgresql-kit::%normalize-tls-options :not-a-plist))
      (:missing-key-pair
       (cl-postgresql-kit::%normalize-tls-options
        '(:certificate "a" :key)))
      (:dotted-options-tail
       (let ((options (list :certificate "a")))
         (setf (cdr (last options)) :tail)
         (cl-postgresql-kit::%normalize-tls-options options)))
      (:dotted-verify-location-tail
       (let ((locations (list "ca.crt")))
         (setf (cdr (last locations)) "tail")
         (cl-postgresql-kit::%normalize-tls-verify-location
          locations)))
      (:dotted-alpn-tail
       (let ((protocols (list "postgresql")))
         (setf (cdr (last protocols)) 1)
         (cl-postgresql-kit::%normalize-tls-option-value
          :alpn-protocols protocols)))
      (:circular-options-list
       (let ((options (list :certificate "a")))
         (setf (cdr (last options)) options)
         (cl-postgresql-kit::%normalize-tls-options options))))))

(deftest-condition-retires-connection
    failed-exchange-retires-connection
    protocol-error
    (ready-memory-connection :input (make-frame #\D (octets 0 0 255)))
  (query connection "select 1"))

(deftest-condition-retires-connection
    unknown-backend-message-retires-connection
    protocol-error
    (ready-memory-connection :input (make-frame #\? #()))
  (query connection "select 1"))

(deftest-condition-retires-connection
    wrong-copy-response-retires-connection
    copy-error
    (ready-memory-connection
     :input (make-frame #\H (copy-response-payload)))
  (copy-in connection "copy target from stdin"))

(deftest memory-transport-rejects-closed-write
  (let ((transport (make-memory-transport)))
    (transport-open transport)
    (transport-close transport)
    (assert-signals 'transport-error
                   (lambda ()
                     (transport-write-all transport (octets 1))))))

(deftest memory-transport-read-write-boundaries
  (let ((transport (make-memory-transport)))
    (it-signals-each 'transport-error
        ((:read-available-before-open)
         (:wait-readable-before-open))
      "rejects memory transport operation case ~A before open"
      (label)
      (case label
        (:read-available-before-open
         (transport-read-available transport))
        (:wait-readable-before-open
         (transport-wait-readable transport))))
    (transport-open transport)
    (is (zerop (length (transport-read-available transport))))
    (is (not (transport-wait-readable transport)))
    (assert-signals 'parameter-error
                   (lambda () (transport-read-exactly transport -1)))
    (assert-signals 'transport-error
                   (lambda () (transport-read-exactly transport 1)))
    (memory-transport-append-input transport (octets 7 8))
    (is (transport-wait-readable transport))
    (is (equalp (octets 7) (transport-read-exactly transport 1)))
    (is (equalp (octets 8) (transport-read-available transport)))
    (is (not (transport-wait-readable transport)))
    (is (equalp (octets 9 10)
                (transport-write-all transport (octets 9 10))))
    (is (transport-flush transport))
    (is (equalp (octets 9 10) (memory-transport-output transport)))
    (transport-close transport)
    (is (not (transport-alive-p transport)))
    (it-signals-each 'transport-error
        ((:flush-after-close))
      "rejects memory transport operation case ~A after close"
      (label)
      (declare (ignore label))
      (transport-flush transport))))

(deftest auth-protocol-buffer-and-handler-boundaries
  (let* ((frame (make-frame #\Z (octets (char-code #\I))))
         (split 4)
         (connection (ready-memory-connection
                      :input (subseq frame 0 split)
                      :max-notifications 0))
         (transport (connection-transport connection))
         (notices nil)
         (notifications nil)
         (notification-payload
           (join-octets (octets 0 0 0 1)
                        (cstring "events")
                        (cstring "created"))))
    (unwind-protect
         (progn
           (setf (connection-notice-handler connection)
                 (lambda (ignored notice)
                   (declare (ignore ignored))
                   (push notice notices)))
           (setf (connection-notification-handler connection)
                 (lambda (ignored process-id channel payload)
                   (declare (ignore ignored))
                   (push (list process-id channel payload)
                         notifications)))
           (is (= split
                  (cl-postgresql-kit::%connection-fill-read-buffer connection)))
           (is (null
                (cl-postgresql-kit::%connection-buffered-backend-message
                 connection)))
           (memory-transport-append-input transport (subseq frame split))
           (is (= (- (length frame) split)
                  (cl-postgresql-kit::%connection-fill-read-buffer connection)))
           (let ((message
                   (cl-postgresql-kit::%connection-buffered-backend-message
                    connection)))
             (is (= (char-code #\Z) (backend-message-type message)))
             (is (equalp (octets (char-code #\I))
                         (backend-message-payload message))))
           (is (null
                (cl-postgresql-kit::%connection-buffered-backend-message
                 connection)))
           (cl-postgresql-kit::%handle-notice-fields
            connection '((:message . "notice")))
           (cl-postgresql-kit::%handle-notification
            connection notification-payload)
           (is (= 1 (length notices)))
           (is (= 1 (length notifications)))
           (is (string= "notice"
                        (cdr (assoc :message
                                    (notice-fields (first notices))))))
           (is (equal '((1 "events" "created")) notifications))
           (is (null (connection-notifications connection)))
           (is (null
                (cl-postgresql-kit::%send-frontend-message
                 connection (make-frame #\Q #()) :flush-p nil)))
           (is (equalp (make-frame #\Q #())
                       (memory-transport-output transport)))
           (setf (connection-state connection) :closed)
           (assert-signals 'connection-error
                          (lambda ()
                            (cl-postgresql-kit::%send-frontend-message
                             connection (make-frame #\Q #())))))
      (disconnect connection))))

(deftest auth-protocol-rejects-invalid-buffered-frames
  (let ((connection
          (ready-memory-connection
           :input (octets (char-code #\Z) 0 0 0 3))))
    (unwind-protect
         (progn
           (cl-postgresql-kit::%connection-fill-read-buffer connection)
           (assert-signals 'protocol-error
                          (lambda ()
                            (cl-postgresql-kit::%connection-buffered-backend-message
                             connection))))
      (disconnect connection)))
  (let ((*maximum-frame-size* 4)
        (connection
          (ready-memory-connection
           :input (octets (char-code #\Z) 0 0 0 5 0))))
    (unwind-protect
         (progn
           (cl-postgresql-kit::%connection-fill-read-buffer connection)
           (assert-signals 'protocol-error
                          (lambda ()
                            (cl-postgresql-kit::%connection-buffered-backend-message
                             connection))))
      (disconnect connection))))

(deftest wire-and-transport-boundaries
  (let ((builder (make-octet-builder)))
    (it-signals-each 'parameter-error
        ((:append-u8-overflow)
         (:append-i16-overflow)
         (:invalid-frame-type)
         (:cstring-with-null))
      "rejects invalid wire builder inputs"
      (label)
      (case label
        (:append-u8-overflow
         (append-u8 builder 256))
        (:append-i16-overflow
         (append-i16 builder 32768))
        (:invalid-frame-type
         (make-frame 256 #()))
        (:cstring-with-null
         (append-cstring builder (format nil "a~Cb" #\Null))))))
  (it-signals-each 'parameter-error
      ((:octet-out-of-range)
       (:invalid-sequence-designator))
    "rejects invalid copy-data payloads"
    (label)
    (case label
      (:octet-out-of-range
       (encode-copy-data-message '(256)))
      (:invalid-sequence-designator
       (encode-copy-data-message :invalid))))
  (let ((transport (make-memory-transport)))
    (assert-signals 'parameter-error
                   (lambda () (transport-read-exactly transport -1))))
  (let ((*maximum-frame-size* 16))
    (assert-signals 'parameter-error
                   (lambda ()
                     (encode-startup-message
                      :user "user"
                      :parameters (list (cons "large" (make-string 20
                                                                      :initial-element #\x)))))))
  (let* ((message (parse-frame (encode-bind-message (list nil))))
         (expected (octets 0 0 0 0 0 1 0 0 0 0 0 0)))
    (is (equalp expected (backend-message-payload message)))))
