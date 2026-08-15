(in-package #:cl-postgresql-kit/test)

(cl-weave:it-property "UTF-8 text codec round trip"
  ((value (cl-weave:gen-string :min-length 0
                               :max-length 64
                               :alphabet "abcdefghijklmnopqrstuvwxyz 0123456789")))
  (is (string= value
               (cl-postgresql-kit::%decode-utf8
                (cl-postgresql-kit::%encode-utf8 value)))))

(deftest jsonb-and-sql-null
  (let* ((registry (default-type-registry))
         (json (json-kit:parse "{\"ok\":true}"))
         (encoded (encode-value registry 3802
                                (make-json-value :value json)
                                :format 1))
         (decoded (decode-value registry 3802 encoded :format 1)))
    (is (= (aref encoded 0) 1) "JSONB must carry protocol version 1.")
    (is (typep decoded 'json-value) "JSONB must decode to a JSON value.")
    (is (eq t
            (gethash "ok"
                     (json-kit:parse
                      (json-kit:stringify (json-value-value decoded)))))
        "JSONB must preserve the decoded JSON value.")
    (is (sql-null-p +sql-null+))
    (is (null (encode-value registry 23 +sql-null+)))
    (is (equalp (encode-value registry 16 nil)
                (cl-codec-kit:string-to-octets "f" :encoding :utf-8))
        "Ordinary NIL must encode as PostgreSQL false.")
    (is (sql-null-p (decode-value registry 23 nil)))
    (it-signals-each 'protocol-error
        ((:invalid-jsonb-version)
         (:empty-jsonb-payload))
      "jsonb decoder rejects malformed binary payload case ~A"
      (label)
      (ecase label
        (:invalid-jsonb-version
         (decode-value registry 3802 (octets 2 123) :format 1))
        (:empty-jsonb-payload
         (decode-value registry 3802 #() :format 1))))))

(deftest sql-null-wire-encoding
  (let* ((message (parse-frame (encode-bind-message (list +sql-null+))))
         (expected (octets 0 0 0 0 0 1 #xff #xff #xff #xff 0 0))
         (data-row (parse-data-row (octets 0 1 #xff #xff #xff #xff))))
    (is (equalp (backend-message-payload message) expected))
    (is (sql-null-p (aref data-row 0)))))

(deftest protocol-encode-defaults-and-validation
  (let ((startup
          (encode-startup-message
           :user "alice"
           :database "app"
           :application-name "kit"
           :parameters '(("search_path" . "public")))))
    (multiple-value-bind (declared-length version-position)
        (cl-postgresql-kit::%read-u32 startup 0)
      (is (= (length startup) declared-length))
      (multiple-value-bind (version parameter-position)
          (cl-postgresql-kit::%read-u32 startup version-position)
        (is (= #x00030000 version))
        (labels ((read-parameters (position)
                   (multiple-value-bind (name next)
                       (cl-postgresql-kit::%read-cstring startup position)
                     (if (string= name "")
                         (values nil next)
                         (multiple-value-bind (value after-value)
                             (cl-postgresql-kit::%read-cstring startup next)
                           (multiple-value-bind (rest end)
                               (read-parameters after-value)
                             (values (cons (cons name value) rest) end)))))))
          (multiple-value-bind (parameters end)
              (read-parameters parameter-position)
            (is (equal '( ("user" . "alice")
                          ("database" . "app")
                          ("application_name" . "kit")
                          ("search_path" . "public"))
                        parameters))
            (is (= (length startup) end)))))))
  (let ((parse (parse-frame (encode-parse-message "select 1")))
        (bind (parse-frame (encode-bind-message (list "x")))))
    (multiple-value-bind (statement next)
        (cl-postgresql-kit::%read-cstring (backend-message-payload parse) 0)
      (is (string= "" statement))
      (multiple-value-bind (sql next)
          (cl-postgresql-kit::%read-cstring (backend-message-payload parse) next)
        (is (string= "select 1" sql))
        (multiple-value-bind (count end)
            (cl-postgresql-kit::%read-u16 (backend-message-payload parse) next)
          (declare (ignore end))
          (is (zerop count)))))
    (is (equalp (octets 0 0 0 0 0 1 0 0 0 1 120 0 0)
                (backend-message-payload bind)))
    (is-each (kind '(:portal :statement))
      (is (= (char-code (if (eq kind :portal) #\P #\S))
             (aref (backend-message-payload
                    (parse-frame (encode-describe-message "name" :kind kind)))
                   0)))
      (is (= (char-code (if (eq kind :portal) #\P #\S))
             (aref (backend-message-payload
                    (parse-frame (encode-close-message "name" :kind kind)))
                   0))))
    (is (equalp (octets 0 0 0 0 0)
                (backend-message-payload
                 (parse-frame (encode-execute-message))))))
  (it-signals-each 'parameter-error
      ((:malformed-parameters)
       (:non-string-parameter-value)
       (:startup-frame-too-large)
       (:invalid-parameter-type-oid)
       (:invalid-result-format))
    "wire encoder rejects invalid input case ~A"
    (label)
    (ecase label
      (:malformed-parameters
       (encode-startup-message :parameters '(42)))
      (:non-string-parameter-value
       (encode-startup-message :parameters '(("bad" . 42))))
      (:startup-frame-too-large
       (let ((*maximum-frame-size* 4))
         (encode-startup-message :user "alice")))
      (:invalid-parameter-type-oid
       (encode-parse-message "select 1"
                             :parameter-type-oids '(#x100000000)))
      (:invalid-result-format
       (encode-bind-message (list "x") :result-formats '(2))))))

(deftest typed-copy-text-row-round-trip
  (let* ((registry (default-type-registry))
         (values (list 42
                       (format nil "hello~Cworld~Cnext\\row"
                               #\Tab #\Newline)
                       "a|b"
                       +sql-null+))
         (oids '(23 25 25 23))
         (wire (encode-copy-row values oids
                                :type-registry registry
                                :delimiter #\|))
         (text (cl-codec-kit:octets-to-string wire :encoding :utf-8))
         (decoded (decode-copy-row wire oids
                                   :type-registry registry
                                   :delimiter #\|)))
    (is (string= text
                (format nil "42|hello\\tworld\\nnext\\\\row|a\\|b|\\N~C"
                        #\Newline)))
    (is (= (aref wire (1- (length wire))) 10))
    (is (= (first decoded) 42))
    (is (string= (second decoded) (second values)))
    (is (string= (third decoded) "a|b"))
    (is (sql-null-p (fourth decoded)))))

(deftest typed-copy-binary-row-round-trip
  (let* ((registry (default-type-registry))
         (oids '(23 25 23))
         (wire (encode-copy-row (list 42 "hello" +sql-null+) oids
                                :type-registry registry
                                :format :binary))
         (decoded (decode-copy-row wire oids
                                   :type-registry registry
                                   :format :binary)))
    (is (equalp (subseq wire 0 2) (octets 0 3)))
    (is (= (length wire) 23))
    (is (= (first decoded) 42))
    (is (string= (second decoded) "hello"))
    (is (sql-null-p (third decoded)))
    (it-signals-each 'protocol-error
        ((:trailing-octet))
      "binary copy row decoder rejects malformed payload case ~A"
      (label)
      (declare (ignore label))
      (decode-copy-row (join-octets wire (octets 0)) oids
                       :type-registry registry
                       :format :binary))))

(deftest typed-copy-binary-stream-round-trip
  (let* ((registry (default-type-registry))
         (oids '(23 25 23))
         (rows (list (list 42 "hello" +sql-null+)
                     (list 7 "world" 9)))
         (extension (octets 1 2 3))
         (wire (encode-copy-binary-stream rows oids
                                          :type-registry registry
                                          :flags 3
                                          :extension extension)))
    (is (equalp (subseq wire 0 11)
                (octets 80 71 67 79 80 89 10 255 13 10 0)))
    (is (equalp (subseq wire (- (length wire) 2))
                (octets 255 255)))
    (multiple-value-bind (decoded flags decoded-extension)
        (decode-copy-binary-stream wire oids :type-registry registry)
      (is (equalp decoded-extension extension))
      (is (= flags 3))
      (is (= (length decoded) 2))
      (is (= (first (first decoded)) 42))
      (is (string= (second (first decoded)) "hello"))
      (is (sql-null-p (third (first decoded))))
      (is (= (third (second decoded)) 9)))
    (it-signals-each 'protocol-error
        ((:bad-signature)
         (:missing-trailer)
         (:trailing-octet))
      "binary copy stream decoder rejects malformed payload case ~A"
      (label)
      (ecase label
        (:bad-signature
         (let ((bad-signature (copy-seq wire)))
           (setf (aref bad-signature 0) 0)
           (decode-copy-binary-stream bad-signature oids)))
        (:missing-trailer
         (decode-copy-binary-stream
          (subseq wire 0 (- (length wire) 2))
          oids))
        (:trailing-octet
         (decode-copy-binary-stream
          (join-octets wire (octets 0))
          oids))))))

(deftest typed-copy-text-rejects-truncated-hex-escape
  (it-signals-each 'protocol-error
      ((:truncated-hex-escape #(92 120 52)))
    "text copy row decoder rejects malformed payload case ~A"
    (label payload)
    (declare (ignore label))
    (decode-copy-row payload '(25))))

(deftest flush-and-function-call-wire-format
  (let* ((flush (parse-frame (encode-flush-message)))
         (call (parse-frame
                (encode-function-call-message
                 123
                 (list (octets 1 2) +sql-null+)
                 :argument-formats '(1 0)
                 :result-format 1)))
         (expected (octets 0 0 0 123
                           0 2 0 1 0 0
                           0 2
                           0 0 0 2 1 2
                           #xff #xff #xff #xff
                           0 1)))
    (is (= (backend-message-type flush) (char-code #\H)))
    (is (zerop (length (backend-message-payload flush))))
    (is (= (backend-message-type call) (char-code #\F)))
    (is (equalp (backend-message-payload call) expected)))
  (is (equalp (parse-function-call-response (octets 0 0 0 3 65 66 67))
              (octets 65 66 67)))
  (is (sql-null-p
       (parse-function-call-response (octets #xff #xff #xff #xff))))
  (it-signals-each 'protocol-error
      ((:trailing-octet #(0 0 0 0 1)))
    "function call response parser rejects malformed payload case ~A"
    (label payload)
    (declare (ignore label))
    (parse-function-call-response payload)))
