(in-package #:cl-postgresql-kit/test)

(deftest parsers-reject-trailing-payload
  (is-case-each ((parser payload)
                 `((,#'parse-row-description ,(octets 0 0))
                   (,#'parse-data-row ,(octets 0 0))
                   (,#'parse-error-response
                    ,(join-octets (octets (char-code #\M)) (cstring "known")
                                  (octets 0)))
                   (,#'parse-copy-response ,(octets 0 0 0))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (funcall parser
                              (join-octets payload (octets 255)))))))

(deftest protocol-response-parsers-cover-boundaries
  (flet ((authentication (code &optional (tail #()))
           (let ((builder (make-octet-builder)))
             (append-u32 builder code)
             (append-octets builder tail)
             (builder-octets builder))))
    (is (equal '(:type :ok)
               (parse-authentication (authentication 0))))
    (is (equal '(:type :cleartext-password)
               (parse-authentication (authentication 3))))
    (let ((message (parse-authentication
                    (authentication 5 (octets 1 2 3 4)))))
      (is (equal '(:type :md5-password) (subseq message 0 2)))
      (is (equalp (octets 1 2 3 4) (getf message :salt))))
    (let ((builder (make-octet-builder)))
      (append-u32 builder 10)
      (append-cstring builder "SCRAM-SHA-256")
      (append-cstring builder "PLAIN")
      (append-u8 builder 0)
      (is (equal '(:type :sasl :mechanisms ("SCRAM-SHA-256" "PLAIN"))
                 (parse-authentication (builder-octets builder)))))
    (is (string= "continue"
                 (getf (parse-authentication
                        (authentication
                         11
                         (cl-codec-kit:string-to-octets
                          "continue" :encoding :utf-8)))
                       :data)))
    (is (string= "final"
                 (getf (parse-authentication
                        (authentication
                         12
                         (cl-codec-kit:string-to-octets
                          "final" :encoding :utf-8)))
                       :data)))
    (it-signals-each 'protocol-error
        ((:short-md5-salt)
         (:unterminated-sasl-mechanism-list))
      "authentication parser rejects malformed payload case ~A"
      (label)
      (ecase label
        (:short-md5-salt
         (parse-authentication (authentication 5)))
        (:unterminated-sasl-mechanism-list
         (parse-authentication (authentication 10 (cstring "SCRAM"))))))
    (is (equal '(:type :gss :data #())
               (parse-authentication (authentication 7))))
    (let ((payload (octets 9 8 7)))
      (is (equalp (list :type :gss-continue :data payload)
                  (parse-authentication (authentication 8 payload)))))
    (is (equal '(:type :sspi :data #())
               (parse-authentication (authentication 9))))
    (let ((builder (make-octet-builder)))
      (append-cstring builder "client_encoding")
      (append-cstring builder "UTF8")
      (is (equalp '("client_encoding" . "UTF8")
                  (parse-parameter-status (builder-octets builder)))))
    (let ((builder (make-octet-builder)))
      (append-i32 builder 42)
      (append-i32 builder -7)
      (multiple-value-bind (pid secret)
          (parse-backend-key-data (builder-octets builder))
        (is (= 42 pid))
        (is (= -7 secret))))
    (is-each (status '(#\I #\T #\E))
      (is (char= status (parse-ready-for-query (octets (char-code status))))))
    (assert-signals 'protocol-error
                   (lambda () (parse-ready-for-query (octets (char-code #\X)))))
    (let ((builder (make-octet-builder)))
      (append-u16 builder 1)
      (append-cstring builder "id")
      (append-u32 builder 42)
      (append-i16 builder 1)
      (append-u32 builder 23)
      (append-i16 builder 4)
      (append-i32 builder -1)
      (append-i16 builder 0)
      (let ((columns (parse-row-description (builder-octets builder))))
        (is (= 1 (length columns)))
        (is (string= "id" (column-name (aref columns 0))))
        (is (= 42 (column-table-oid (aref columns 0))))
        (is (= 23 (column-type-oid (aref columns 0))))
        (is (zerop (column-format-code (aref columns 0))))))
    (let ((builder (make-octet-builder)))
      (append-u16 builder 1)
      (append-cstring builder "payload")
      (append-u32 builder 0)
      (append-i16 builder 0)
      (append-u32 builder 17)
      (append-i16 builder -1)
      (append-i32 builder 0)
      (append-i16 builder 1)
      (is (= 1 (column-format-code
                (aref (parse-row-description (builder-octets builder)) 0)))))
    (let ((builder (make-octet-builder)))
      (append-u16 builder 2)
      (append-i32 builder 3)
      (append-octets builder
                     (cl-codec-kit:string-to-octets "abc" :encoding :utf-8))
      (append-i32 builder -1)
      (let ((values (parse-data-row (builder-octets builder))))
        (is (= 2 (length values)))
        (is (equalp (cl-codec-kit:string-to-octets "abc" :encoding :utf-8)
                    (aref values 0)))
        (is (sql-null-p (aref values 1)))))
    (is (= 0 (length (parse-data-row (octets 0 0)))))
    (let ((builder (make-octet-builder)))
      (append-cstring builder "INSERT 0 1")
      (is (string= "INSERT 0 1"
                   (parse-command-complete (builder-octets builder)))))
    (let ((builder (make-octet-builder)))
      (append-i32 builder 42)
      (append-cstring builder "events")
      (append-cstring builder "created")
      (let ((notification (parse-notification-response
                           (builder-octets builder))))
        (is (= 42 (getf notification :pid)))
        (is (string= "events" (getf notification :channel)))
        (is (string= "created" (getf notification :payload)))))
    (let ((builder (make-octet-builder)))
      (append-u8 builder 0)
      (append-u16 builder 2)
      (append-u16 builder 0)
      (append-u16 builder 1)
      (let ((response (parse-copy-response (builder-octets builder))))
        (is (zerop (getf response :format)))
        (is (= 2 (getf response :column-count)))
        (is (equalp #(0 1) (getf response :formats)))))
    (is (equalp (octets 1 2 3)
                (parse-copy-data (octets 1 2 3))))
    (it-signals-each 'protocol-error
        ((:short-row-description)
         (:invalid-row-format-code)
         (:negative-data-row-length)
         (:invalid-copy-format-code)
         (:truncated-copy-format-vector))
      "response parser rejects malformed payload case ~A"
      (label)
      (ecase label
        (:short-row-description
         (parse-row-description (octets 0 1)))
        (:invalid-row-format-code
         (let ((builder (make-octet-builder)))
           (append-u16 builder 1)
           (append-cstring builder "id")
           (append-u32 builder 0)
           (append-i16 builder 0)
           (append-u32 builder 23)
           (append-i16 builder 4)
           (append-i32 builder 0)
           (append-i16 builder 2)
           (parse-row-description (builder-octets builder))))
        (:negative-data-row-length
         (let ((builder (make-octet-builder)))
           (append-u16 builder 1)
           (append-i32 builder -2)
           (parse-data-row (builder-octets builder))))
        (:invalid-copy-format-code
         (let ((builder (make-octet-builder)))
           (append-u8 builder 2)
           (append-u16 builder 0)
           (parse-copy-response (builder-octets builder))))
        (:truncated-copy-format-vector
         (let ((builder (make-octet-builder)))
           (append-u8 builder 0)
           (append-u16 builder 1)
           (append-u16 builder 2)
           (parse-copy-response (builder-octets builder))))))))

(deftest protocol-v32-wire-boundaries
  (is (equalp (octets 0 3 0 2)
              (subseq (encode-startup-message
                       :user "alice"
                       :protocol-version +protocol-version-3.2+)
                      4 8)))
  (is (equalp (octets 0 0 0 8 4 210 22 48)
              (encode-gssenc-request)))
  (let ((secret (octets 1 2 3 4 5 6)))
    (is (equalp (join-octets
                 (octets 0 0 0 18 4 210 22 46)
                 (octets 0 0 0 42)
                 secret)
                (encode-cancel-request 42 secret)))
    (let ((builder (make-octet-builder)))
      (append-i32 builder 42)
      (append-octets builder secret)
      (multiple-value-bind (pid parsed-secret)
          (parse-backend-key-data (builder-octets builder)
                                  :protocol-version +protocol-version-3.2+)
        (is (= 42 pid))
        (is (equalp secret parsed-secret)))))
  (it-signals-each 'protocol-error
      ((:short-cancel-secret 3)
       (:oversized-cancel-secret 257))
    "protocol v3.2 backend key parser rejects malformed secret case ~A"
    (label secret-length)
    (declare (ignore label))
    (let ((builder (make-octet-builder)))
      (append-i32 builder 42)
      (append-octets builder
                     (make-array secret-length
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 0))
      (parse-backend-key-data
       (builder-octets builder)
       :protocol-version +protocol-version-3.2+)))
  (let ((builder (make-octet-builder)))
    (append-i32 builder 42)
    (append-i32 builder 2)
    (append-cstring builder "unknown_option")
    (append-cstring builder "another_option")
    (is (equal '(:newest-minor-version 42
                 :unrecognized-options ("unknown_option" "another_option"))
               (parse-negotiate-protocol-version (builder-octets builder)))))
  (assert-signals 'parameter-error
                 (lambda ()
                   (parse-backend-key-data
                    (join-octets (octets 0 0 0 42)
                                 (octets 1 2 3 4))
                    :protocol-version #x00030001)))
  (is (eq :negotiate-protocol-version
          (backend-message-kind (char-code #\v))))
  (is (equalp (make-frame #\p (octets 1 2 3))
              (encode-gss-response (octets 1 2 3)))))
