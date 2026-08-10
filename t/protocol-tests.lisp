(in-package #:cl-postgresql-kit/test)

(deftest frame-round-trip
  (let* ((payload (cl-codec-kit:string-to-octets "select 1" :encoding :utf-8))
         (frame (make-frame #\Q payload))
         (message (parse-frame frame)))
    (is (= (backend-message-type message) (char-code #\Q)))
    (is (equalp (backend-message-payload message) payload))))

(cl-weave:it-property "UTF-8 text codec round trip"
  ((value (cl-weave:gen-string :min-length 0
                               :max-length 64
                               :alphabet "abcdefghijklmnopqrstuvwxyz 0123456789")))
  (is (string= value
               (cl-postgresql-kit::%decode-utf8
                (cl-postgresql-kit::%encode-utf8 value)))))

(deftest crypto-known-vectors
  (is (string= "900150983CD24FB0D6963F7D28E17F72"
               (octets-as-hex (md5-digest "abc"))))
  (is (string= "BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD"
               (octets-as-hex (sha256-digest "abc"))))
  (is (string= "F7BC83F430538424B13298E6AA6FB143EF4D59A14946175997479DBC2D1A3CD8"
               (octets-as-hex
                (hmac-sha256 "key" "The quick brown fox jumps over the lazy dog"))))
  (is (equalp (cl-codec-kit:string-to-octets "foo" :encoding :utf-8)
              (base64-to-octets "Zm9v"))))

(deftest public-structure-predicates
  (let* ((builder (make-octet-builder))
         (message (parse-frame (make-frame #\Q #())))
         (column (cl-postgresql-kit::%make-column "id" 0 0 23 4 -1 0))
         (registry (make-type-registry :include-defaults nil))
         (typed (make-typed-value "1" 23))
         (json (make-json-value :value nil))
         (bytea (make-bytea-value :value #()))
         (date (make-date-value :value "2026-01-01"))
         (time (make-time-value :value "00:00:00"))
         (timestamp (make-timestamp-value :value "2026-01-01 00:00:00"))
         (timestamptz (make-timestamptz-value :value "2026-01-01 00:00:00+00"))
         (interval (make-interval-value :value "0 seconds"))
         (result (cl-postgresql-kit::%make-query-result :columns #()
                                                        :rows nil
                                                        :row-count 0))
         (statement (cl-postgresql-kit::%make-prepared-statement
                    :name "statement" :sql "select 1" :connection nil)))
    (dolist (case (list (cons builder #'octet-builder-p)
                        (cons message #'backend-message-p)
                        (cons column #'column-p)
                        (cons registry #'type-registry-p)
                        (cons typed #'typed-value-p)
                        (cons json #'json-value-p)
                        (cons bytea #'bytea-value-p)
                        (cons date #'date-value-p)
                        (cons time #'time-value-p)
                        (cons timestamp #'timestamp-value-p)
                        (cons timestamptz #'timestamptz-value-p)
                        (cons interval #'interval-value-p)
                        (cons result #'query-result-p)
                        (cons statement #'prepared-statement-p)))
      (is (funcall (cdr case) (car case))))))

(deftest pipeline-data-structure-contracts
  (let* ((request
           (cl-postgresql-kit::%make-pipeline-request
            :sql "select $1"
            :parameters '("value")
            :parameter-type-oids '(25)
            :parameter-formats '(0)
            :result-formats '(0)
            :statement-name "stmt"
            :portal-name "portal"))
         (entry
           (cl-postgresql-kit::%make-pipeline-entry
            :request request
            :statement-name "stmt"
            :portal-name "portal"
            :close-statement-p t
            :columns '(:id)
            :raw-rows '((1))
            :command-tag "SELECT 1"
            :portal-suspended-p nil
            :notices '(:notice)
            :results '(:result)
            :current-result-p t
            :pending-error :none)))
    (is (cl-postgresql-kit::pipeline-request-p request))
    (is (string= "select $1"
                 (cl-postgresql-kit::pipeline-request-sql request)))
    (is (equal '("value")
               (cl-postgresql-kit::pipeline-request-parameters request)))
    (is (equal '(25)
               (cl-postgresql-kit::pipeline-request-parameter-type-oids request)))
    (is (equal '(0)
               (cl-postgresql-kit::pipeline-request-parameter-formats request)))
    (is (equal '(0)
               (cl-postgresql-kit::pipeline-request-result-formats request)))
    (is (string= "stmt"
                 (cl-postgresql-kit::pipeline-request-statement-name request)))
    (is (string= "portal"
                 (cl-postgresql-kit::pipeline-request-portal-name request)))
    (is (cl-postgresql-kit::pipeline-entry-p entry))
    (is (eq request (cl-postgresql-kit::pipeline-entry-request entry)))
    (is (string= "stmt"
                 (cl-postgresql-kit::pipeline-entry-statement-name entry)))
    (is (string= "portal"
                 (cl-postgresql-kit::pipeline-entry-portal-name entry)))
    (is (cl-postgresql-kit::pipeline-entry-close-statement-p entry))
    (is (equal '(:id) (cl-postgresql-kit::pipeline-entry-columns entry)))
    (is (equal '((1)) (cl-postgresql-kit::pipeline-entry-raw-rows entry)))
    (is (string= "SELECT 1"
                 (cl-postgresql-kit::pipeline-entry-command-tag entry)))
    (is (not (cl-postgresql-kit::pipeline-entry-portal-suspended-p entry)))
    (is (equal '(:notice) (cl-postgresql-kit::pipeline-entry-notices entry)))
    (is (equal '(:result) (cl-postgresql-kit::pipeline-entry-results entry)))
    (is (cl-postgresql-kit::pipeline-entry-current-result-p entry))
    (is (eq :none (cl-postgresql-kit::pipeline-entry-pending-error entry)))))

(deftest cursor-state-predicates
  (let ((cursor (make-instance 'cursor
                               :connection nil
                               :statement-name "statement"
                               :portal-name "portal"
                               :fetch-size 10)))
    (is (cursor-p cursor))
    (is (not (cursor-p nil)))
    (is (not (cursor-suspended-p cursor)))
    (setf (cl-postgresql-kit::cursor--suspended-p cursor) t)
    (is (cursor-suspended-p cursor))
    (assert-signals 'type-error
                   (lambda () (cursor-suspended-p nil)))))

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
    (assert-signals 'protocol-error
                   (lambda () (decode-value registry 3802 (octets 2 123) :format 1)))
    (assert-signals 'protocol-error
                   (lambda () (decode-value registry 3802 #() :format 1)))))

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
    (dolist (kind (list :portal :statement))
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
  (assert-signals 'parameter-error
                 (lambda ()
                   (encode-startup-message :parameters '(42))))
  (assert-signals 'parameter-error
                 (lambda ()
                   (encode-startup-message :parameters '( ("bad" . 42)))))
  (let ((*maximum-frame-size* 4))
    (assert-signals 'parameter-error
                   (lambda () (encode-startup-message :user "alice"))))
  (assert-signals 'parameter-error
                 (lambda ()
                   (encode-parse-message "select 1"
                                         :parameter-type-oids '(#x100000000))))
  (assert-signals 'parameter-error
                 (lambda ()
                   (encode-bind-message (list "x") :result-formats '(2)))))

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
    (assert-signals 'protocol-error
                   (lambda ()
                      (decode-copy-row (join-octets wire (octets 0)) oids
                                       :type-registry registry
                                       :format :binary)))))

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
    (let ((bad-signature (copy-seq wire)))
      (setf (aref bad-signature 0) 0)
      (assert-signals 'protocol-error
                     (lambda ()
                       (decode-copy-binary-stream bad-signature oids))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-copy-binary-stream
                      (subseq wire 0 (- (length wire) 2))
                      oids)))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-copy-binary-stream
                      (join-octets wire (octets 0))
                      oids)))))

(deftest typed-copy-text-rejects-truncated-hex-escape
  (assert-signals 'protocol-error
                 (lambda ()
                   (decode-copy-row (octets 92 120 52) '(25)))))

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
  (assert-signals 'protocol-error
                 (lambda ()
                   (parse-function-call-response (octets 0 0 0 0 1)))))

(deftest unknown-error-fields-are-preserved
  (let* ((payload (join-octets
                   (octets (char-code #\S)) (cstring "ERROR")
                   (octets (char-code #\M)) (cstring "known")
                   (octets (char-code #\Z)) (cstring "future-field")
                   (octets 0)))
         (fields (parse-error-response payload))
         (unknown (cdr (assoc :unknown-fields fields))))
    (is (string= (cdr (assoc :message fields)) "known"))
    (is (string= (cdr (assoc #\Z unknown)) "future-field"))))

(deftest error-field-catalog-round-trips
  (let* ((specifications
           '((#\S :severity "ERROR")
             (#\V :severity-localized "LOCALIZED")
             (#\C :sqlstate "42601")
             (#\M :message "syntax error")
             (#\D :detail "detail")
             (#\H :hint "hint")
             (#\P :position "7")
             (#\p :internal-position "3")
             (#\q :internal-query "select")
             (#\W :where "executor")
             (#\s :schema "public")
             (#\t :table "items")
             (#\c :column "name")
             (#\d :datatype "text")
             (#\n :constraint "items_name_key")
             (#\F :file "postgres.c")
             (#\L :line "42")
             (#\R :routine "parser")))
         (payload
           (apply #'join-octets
                  (append
                   (loop for (code key value) in specifications
                         collect (join-octets
                                  (octets (char-code code))
                                  (cstring value)))
                   (list (octets 0)))))
         (fields (parse-error-response payload))
         (condition (cl-postgresql-kit::%server-error-from-fields fields)))
    (dolist (specification specifications)
      (destructuring-bind (code key value) specification
        (declare (ignore code))
        (is (string= value (cdr (assoc key fields))))))
    (is (string= "LOCALIZED" (server-error-severity condition)))
    (is (string= "syntax error" (server-error-message condition)))
    (is (string= "42601" (server-error-sqlstate condition)))
    (is (string= "detail" (server-error-detail condition)))
    (is (string= "hint" (server-error-hint condition)))
    (is (string= "7" (server-error-position condition)))
    (is (string= "executor" (server-error-where condition)))
    (is (string= "public" (server-error-schema condition)))
    (is (string= "items" (server-error-table condition)))
    (is (string= "name" (server-error-column condition)))
    (is (string= "text" (server-error-datatype condition)))
    (is (string= "items_name_key" (server-error-constraint condition)))
    (is (string= "postgres.c" (server-error-file condition)))
    (is (string= "42" (server-error-line condition)))
    (is (string= "parser" (server-error-routine condition)))
    (is (null (server-error-unknown-fields condition)))))

(deftest condition-reports-and-accessors
  (let* ((protocol (make-condition 'protocol-error
                                   :message "bad wire"
                                   :context :wire
                                   :expected 2
                                   :actual 1))
         (transport (make-condition 'transport-error
                                    :message "read failed"
                                    :operation :read
                                    :cause :cause))
         (timeout (make-condition 'timeout-error
                                  :message "timed out"
                                  :cancel-sent-p t
                                  :connection-retired-p nil))
         (tls (make-condition 'tls-error
                              :message "tls failed"
                              :cause :cause))
         (pool (make-condition 'pool-exhausted
                               :message "busy"
                               :timeout 3))
         (feature (make-condition 'unsupported-feature
                                  :message "unsupported"
                                  :feature :tls))
         (parameter (make-condition 'parameter-error
                                     :message "bad parameter"
                                     :parameter 42))
         (server (make-condition 'server-error
                                 :fields '((:message . "syntax error")
                                           (:sqlstate . "42601")
                                           (:detail . "detail")
                                           (:hint . "hint"))
                                 :sqlstate "42601"
                                 :detail "detail"
                                 :hint "hint"))
         (notice (make-condition 'notice
                                 :fields '((:message . "notice")
                                           (:sqlstate . "01000")))))
    (is (string= "bad wire" (postgresql-condition-message protocol)))
    (is (eq :wire (protocol-error-context protocol)))
    (is (= 2 (protocol-error-expected protocol)))
    (is (= 1 (protocol-error-actual protocol)))
    (is (eq :read (transport-error-operation transport)))
    (is (eq :cause (transport-error-cause transport)))
    (is (timeout-error-cancel-sent-p timeout))
    (is (not (timeout-error-connection-retired-p timeout)))
    (is (eq :cause (tls-error-cause tls)))
    (is (= 3 (pool-exhausted-timeout pool)))
    (is (eq :tls (unsupported-feature-name feature)))
    (is (= 42 (parameter-error-parameter parameter)))
    (is (search "bad wire" (princ-to-string protocol)))
    (is (search "read failed" (princ-to-string transport)))
    (is (search "syntax error" (princ-to-string server)))
    (is (search "42601" (princ-to-string server)))
    (is (search "detail" (princ-to-string server)))
    (is (search "hint" (princ-to-string server)))
    (is (search "notice" (princ-to-string notice)))
    (is (search "01000" (princ-to-string notice)))
    (let ((default-server (make-condition 'server-error))
          (default-notice (make-condition 'notice)))
      (is (search "PostgreSQL server error"
                  (princ-to-string default-server)))
      (is (search "PostgreSQL notice"
                  (princ-to-string default-notice))))))

(deftest parsers-reject-trailing-payload
  (dolist (case (list
                 (list #'parse-row-description (octets 0 0))
                 (list #'parse-data-row (octets 0 0))
                 (list #'parse-error-response
                       (join-octets (octets (char-code #\M)) (cstring "known")
                                    (octets 0)))
                 (list #'parse-copy-response (octets 0 0 0))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (funcall (first case)
                              (join-octets (second case) (octets 255)))))))

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
    (assert-signals 'protocol-error
                   (lambda () (parse-authentication (authentication 5))))
    (assert-signals 'protocol-error
                   (lambda () (parse-authentication (authentication 10 (cstring "SCRAM")))))
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
    (dolist (status '(#\I #\T #\E))
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
    (assert-signals 'protocol-error
                   (lambda () (parse-row-description (octets 0 1))))
    (let ((builder (make-octet-builder)))
      (append-u16 builder 1)
      (append-cstring builder "id")
      (append-u32 builder 0)
      (append-i16 builder 0)
      (append-u32 builder 23)
      (append-i16 builder 4)
      (append-i32 builder 0)
      (append-i16 builder 2)
      (assert-signals 'protocol-error
                     (lambda ()
                       (parse-row-description (builder-octets builder)))))
    (let ((builder (make-octet-builder)))
      (append-u16 builder 1)
      (append-i32 builder -2)
      (assert-signals 'protocol-error
                     (lambda () (parse-data-row (builder-octets builder)))))
    (let ((builder (make-octet-builder)))
      (append-u8 builder 2)
      (append-u16 builder 0)
      (assert-signals 'protocol-error
                     (lambda () (parse-copy-response (builder-octets builder)))))
    (let ((builder (make-octet-builder)))
      (append-u8 builder 0)
      (append-u16 builder 1)
      (append-u16 builder 2)
    (assert-signals 'protocol-error
                   (lambda () (parse-copy-response (builder-octets builder)))))))

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
  (let ((builder (make-octet-builder)))
    (append-i32 builder 42)
    (append-octets builder (octets 1 2 3))
    (assert-signals 'protocol-error
                   (lambda ()
                     (parse-backend-key-data
                      (builder-octets builder)
                      :protocol-version +protocol-version-3.2+))))
  (let ((builder (make-octet-builder)))
    (append-i32 builder 42)
    (append-octets builder
                   (make-array 257
                               :element-type '(unsigned-byte 8)
                               :initial-element 0))
    (assert-signals 'protocol-error
                   (lambda ()
                     (parse-backend-key-data
                      (builder-octets builder)
                      :protocol-version +protocol-version-3.2+))))
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

(deftest type-registry-isolates-connections
  (let ((left (ready-memory-connection))
        (right (ready-memory-connection)))
    (unwind-protect
         (progn
           (is (not (eq (connection-type-registry left)
                        (connection-type-registry right))))
           (register-type (connection-type-registry left)
                          :oid 90000
                          :name "local_type")
           (is (find-type-codec (connection-type-registry left) 90000))
           (is (null (find-type-codec (connection-type-registry right) 90000))))
      (disconnect left)
      (disconnect right))))

(deftest type-registry-replacement-is-atomic
  (let ((registry (make-type-registry :include-defaults nil))
        (first (make-type-codec :oid 90001 :name "old_name"))
        (second (make-type-codec :oid 90001 :name "new_name")))
    (register-type registry :codec first)
    (register-type registry :codec second)
    (is (eq second (find-type-codec registry 90001)))
    (is (eq second (find-type-codec registry "new_name")))
    (is (null (find-type-codec registry "old_name")))))

(deftest type-registry-registration-boundaries
  (let ((registry (make-type-registry :include-defaults nil)))
    (assert-signals 'parameter-error
                    (lambda () (register-type registry :codec 1)))
    (assert-signals 'parameter-error
                    (lambda ()
                      (register-type
                       registry
                       :codec (make-type-codec :oid -1 :name "bad"))))
    (assert-signals 'parameter-error
                    (lambda ()
                      (register-type
                       registry
                       :codec (make-type-codec :oid 90002 :name ""))))
    (let ((first (make-type-codec :oid 90002 :name "same"))
          (second (make-type-codec :oid 90003 :name "same")))
      (register-type registry :codec first)
      (register-type registry :codec second)
      (is (null (find-type-codec registry 90002)))
      (is (eq second (find-type-codec registry 90003)))
      (is (eq second (find-type-codec registry 'same))))
    (assert-signals 'simple-type-error
                    (lambda () (find-type-codec nil 1)))
    (register-enum-type registry :oid 90010 :name "mood" :labels '("ok"))
    (assert-signals 'parameter-error
                    (lambda () (encode-value registry 90010 "bad")))
    (assert-signals 'parameter-error
                    (lambda ()
                      (register-enum-type
                       registry
                       :oid 90011
                       :name "bad-enum"
                       :labels '("a" "a"))))
    (assert-signals 'parameter-error
                    (lambda ()
                      (register-enum-type
                       registry
                       :oid 90012
                       :name "nul-enum"
                       :labels (list (format nil "bad~Cvalue" (code-char 0))))))
    (assert-signals 'parameter-error
                    (lambda ()
                      (register-domain-type
                       registry
                       :oid 90013
                       :name "bad-domain"
                       :base-oid 90013)))
    (assert-signals 'parameter-error
                    (lambda ()
                      (register-range-type
                       registry
                       :oid 90014
                       :name "bad-range"
                       :subtype-oid 90014)))
    (assert-signals 'parameter-error
                    (lambda ()
                      (register-composite-type
                       registry
                       :oid 90015
                       :name "bad-composite"
                       :field-oids #(23)
                       :field-names #("first" "extra"))))
    (assert-signals 'parameter-error
                    (lambda ()
                      (register-composite-type
                       registry
                       :oid 90016
                       :name "recursive"
                       :field-oids #(90016)
                       :field-names #("self"))))))

(deftest malformed-type-payloads-are-protocol-errors
  (let ((registry (default-type-registry)))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 17
                      (cl-codec-kit:string-to-octets "\\x0" :encoding :utf-8))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 17
                      (cl-codec-kit:string-to-octets "\\xgg" :encoding :utf-8))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 23
                      (cl-codec-kit:string-to-octets
                       "not-an-integer"
                       :encoding :utf-8))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 22
                      (cl-codec-kit:string-to-octets
                       "1 32768"
                       :encoding :utf-8))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 701
                      (cl-codec-kit:string-to-octets "BOGUS" :encoding :utf-8))))
    (is (equalp (octets 1 2 92 65)
                (decode-value
                 registry 17
                 (cl-codec-kit:string-to-octets "\\001\\002\\\\A"
                                                 :encoding :utf-8))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value registry 23 (octets 0 1) :format 1)))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value registry 26 (octets 0) :format 1)))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value registry 16
                                   (cl-codec-kit:string-to-octets "" :encoding :utf-8))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value registry 21
                                   (cl-codec-kit:string-to-octets "32768"
                                                                   :encoding :utf-8))))
    (is (= #xffffffff
           (decode-value registry 26
                         (cl-codec-kit:string-to-octets "4294967295"
                                                         :encoding :utf-8))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value registry 26
                                   (cl-codec-kit:string-to-octets "4294967296"
                                                                   :encoding :utf-8))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value registry 701
                                   (cl-codec-kit:string-to-octets "#C(1 2)"
                                                                   :encoding :utf-8))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 701
                      (cl-codec-kit:string-to-octets ""
                                                      :encoding :utf-8))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 701
                      (cl-codec-kit:string-to-octets "1.0 trailing"
                                                      :encoding :utf-8))))
    (assert-signals 'parameter-error
                   (lambda () (encode-value registry 23 2147483648)))
    (assert-signals 'parameter-error
                   (lambda () (encode-value registry 22 1)))
    (assert-signals 'parameter-error
                   (lambda () (encode-value registry 22 #(32768))))
    (assert-signals 'parameter-error
                   (lambda () (encode-value registry 23 "1" :format 1)))
    (assert-signals 'parameter-error
                   (lambda () (encode-value registry 26 "1" :format 1)))
    (assert-signals 'parameter-error
                   (lambda () (encode-value registry 26 -1 :format 1)))
    (assert-signals 'parameter-error
                   (lambda () (encode-value registry 26 #x100000000 :format 1)))
    (assert-signals 'parameter-error
                   (lambda () (encode-value registry 21 32768 :format 1)))
    (assert-signals 'parameter-error
                   (lambda () (decode-value registry 23 #() :format 2)))))

(deftest floating-point-binary-codecs
  (let ((registry (make-type-registry)))
    (let ((float4 (decode-value registry 700 (octets 63 192 0 0) :format 1))
          (float8 (decode-value registry 701
                                (octets 63 248 0 0 0 0 0 0)
                                :format 1)))
      (is (typep float4 'single-float))
      (is (= 1.5f0 float4))
      (is (typep float8 'double-float))
      (is (= 1.5d0 float8))
      (is (equalp (octets 63 192 0 0)
                  (encode-value registry 700 float4 :format 1)))
      (is (equalp (octets 63 248 0 0 0 0 0 0)
                  (encode-value registry 701 float8 :format 1))))
    (let ((negative (decode-value registry 700 (octets 192 32 0 0)
                                    :format 1)))
      (is (= -2.5f0 negative))
      (is (equalp (octets 192 32 0 0)
                  (encode-value registry 700 negative :format 1))))
    (is (string= "Infinity"
                 (decode-value registry 700 (octets 127 128 0 0)
                               :format 1)))
    (is (string= "-Infinity"
                 (decode-value registry 700 (octets 255 128 0 0)
                               :format 1)))
    (is (string= "NaN"
                 (decode-value registry 700 (octets 127 192 0 0)
                               :format 1)))
    (is (equalp (octets 127 128 0 0)
                (encode-value registry 700 "Infinity" :format 1)))
    (is (equalp (octets 255 128 0 0)
                (encode-value registry 700 "-Infinity" :format 1)))
    (is (equalp (octets 127 128 0 1)
                (encode-value registry 700 "NaN" :format 1)))
    (let ((subnormal (decode-value registry 700 (octets 0 0 0 1)
                                      :format 1)))
      (is (plusp subnormal))
      (is (equalp (octets 0 0 0 1)
                  (encode-value registry 700 subnormal :format 1))))
    (let ((negative-zero (decode-value registry 700 (octets 128 0 0 0)
                                          :format 1)))
      (is (= 0.0f0 negative-zero))
      (is (equalp (octets 128 0 0 0)
                  (encode-value registry 700 negative-zero :format 1))))
    (let ((wire (encode-value registry 700 1.5f0)))
      (is (= 1.5f0
             (decode-value registry 700 wire))))
    (is (string= "Infinity"
                 (cl-codec-kit:octets-to-string
                  (encode-value registry 700 "+Infinity")
                  :encoding :utf-8)))
    (is (string= "Infinity"
                 (decode-value
                  registry 701
                  (cl-codec-kit:string-to-octets "Infinity" :encoding :utf-8))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value registry 701 (octets 0 0 0) :format 1)))
    (assert-signals 'parameter-error
                   (lambda ()
                     (encode-value registry 700 #C(1 2) :format 1)))))

(deftest binary-boolean-codec-boundaries
  (let ((registry (make-type-registry)))
    (is (null (decode-value registry 16 (octets 0) :format 1)))
    (is (eq t (decode-value registry 16 (octets 1) :format 1)))
    (is (equalp (octets 0)
                (encode-value registry 16 nil :format 1)))
    (is (equalp (octets 1)
                (encode-value registry 16 t :format 1)))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value registry 16 #() :format 1)))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value registry 16 (octets 0 1) :format 1)))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value registry 16 (octets 2) :format 1)))))

(deftest numeric-codecs
  (let ((registry (make-type-registry)))
    (is (= 5
           (decode-value
            registry 1700
            (cl-codec-kit:string-to-octets
             "  +.5e+1  " :encoding :utf-8))))
    (is (= (- 1/20)
           (decode-value
            registry 1700
            (cl-codec-kit:string-to-octets
             "-5.e-2" :encoding :utf-8))))
    (is (zerop
         (decode-value
          registry 1700
          (cl-codec-kit:string-to-octets
           "-0.00" :encoding :utf-8))))
    (is (equalp
         (cl-codec-kit:string-to-octets "0" :encoding :utf-8)
         (encode-value registry 1700 0)))
    (is (= 1.5
           (decode-value registry 1700
                         (encode-value registry 1700 1.5f0))))
    (is (equalp
         (cl-codec-kit:string-to-octets "NaN" :encoding :utf-8)
         (encode-value registry 1700 "NaN")))
    (is (= (/ 1234567890123456789012345 100000)
           (decode-value
            registry 1700
            (cl-codec-kit:string-to-octets
             "12345678901234567890.12345" :encoding :utf-8))))
    (is (equalp
         (cl-codec-kit:string-to-octets "-0.0012" :encoding :utf-8)
         (encode-value registry 1700 (/ -3 2500))))
    (assert-signals 'parameter-error
                   (lambda () (encode-value registry 1700 '(1 2 3))))
    (let ((binary (octets 0 2 0 0 0 0 0 2 0 123 17 148)))
      (is (= (/ 2469 20)
             (decode-value registry 1700 binary :format 1)))
      (is (equalp binary
                  (encode-value registry 1700 (/ 2469 20) :format 1))))
    (let ((binary (encode-value registry 1700 (- (/ 2469 20)) :format 1)))
      (is (= (- (/ 2469 20))
             (decode-value registry 1700 binary :format 1))))
    (is (string= "NaN"
                 (decode-value registry 1700
                               (octets 0 0 0 0 192 0 0 0)
                               :format 1)))
    (is (equalp (octets 0 0 0 0 192 0 0 0)
                (encode-value registry 1700 "NaN" :format 1)))
    (is (equalp (octets 0 0 0 0 0 0 0 0)
                (encode-value registry 1700 0 :format 1)))
    (is (zerop (decode-value registry 1700
                             (octets 0 0 0 0 0 0 0 0)
                             :format 1)))
    (is (zerop (decode-value registry 1700
                             (octets 0 0 0 0 64 0 0 0)
                             :format 1)))
    (dolist (payload (list #()
                           (octets 0 0 0 0 0 1 0 0)
                           (octets 0 0 0 0 0 0 255 255)
                           (octets 0 1 0 0 192 0 0 0 0 1)
                           (octets 0 1 0 0 0 0 0 0 39 16)
                           (octets 0 1 0 0 0 0 0 0)
                           #(256 0 0 0 0 0 0 0)))
      (assert-signals 'protocol-error
                     (lambda ()
                       (decode-value registry 1700 payload :format 1))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 1700
                      (cl-codec-kit:string-to-octets
                       "1e100001" :encoding :utf-8))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 1700
                      (cl-codec-kit:string-to-octets
                       (concatenate
                        'string "0."
                        (make-string (1+ *maximum-numeric-digits*)
                                     :initial-element #\0))
                       :encoding :utf-8))))
    (dolist (value '("" "." "1.2.3" "1e" "1e+" "1x" "1e2x"))
      (assert-signals 'protocol-error
                     (lambda ()
                       (decode-value
                        registry 1700
                        (cl-codec-kit:string-to-octets value
                                                       :encoding :utf-8)))))
    (assert-signals 'parameter-error
                   (lambda () (encode-value registry 1700 "1x")))
    (assert-signals 'parameter-error
                   (lambda () (encode-value registry 1700 "Infinity" :format 1)))
    (assert-signals 'parameter-error
                   (lambda () (encode-value registry 1700 (/ 1 3))))))

(deftest uuid-and-array-codecs
  (let ((registry (make-type-registry)))
    (let* ((uuid-text "550E8400-E29B-41D4-A716-446655440000")
           (uuid (decode-value
                  registry 2950
                  (cl-codec-kit:string-to-octets uuid-text :encoding :utf-8)))
           (uuid-text-wire (encode-value registry 2950 uuid))
           (uuid-binary (encode-value registry 2950 uuid :format 1))
           (uuid-round-trip (decode-value registry 2950 uuid-binary :format 1)))
      (is (uuid-value-p uuid))
      (is (string= "550e8400-e29b-41d4-a716-446655440000"
                   (uuid-value-value uuid)))
      (is (equalp (cl-codec-kit:string-to-octets
                   "550e8400-e29b-41d4-a716-446655440000"
                   :encoding :utf-8)
                  uuid-text-wire))
      (is (= 16 (length uuid-binary)))
      (is (string= (uuid-value-value uuid)
                   (uuid-value-value uuid-round-trip)))
      (assert-signals 'protocol-error
                     (lambda ()
                       (decode-value
                        registry 2950
                        (cl-codec-kit:string-to-octets
                         "550e8400-e29b-41d4-a716-44665544000Z"
                         :encoding :utf-8))))
      (assert-signals 'parameter-error
                     (lambda () (encode-value registry 2950 "not-a-uuid"))))
    (let* ((array (decode-value
                   registry 1007
                   (cl-codec-kit:string-to-octets "{1,2,NULL}"
                                                  :encoding :utf-8)))
           (elements (postgres-array-elements array))
           (text-wire (encode-value registry 1007 array))
           (binary-wire (encode-value registry 1007 array :format 1))
           (binary-round-trip
             (decode-value registry 1007 binary-wire :format 1)))
      (is (postgres-array-p array))
      (is (equalp #(3) (postgres-array-dimensions array)))
      (is (equalp #(1) (postgres-array-lower-bounds array)))
      (is (= 1 (aref elements 0)))
      (is (= 2 (aref elements 1)))
      (is (sql-null-p (aref elements 2)))
      (is (equalp (cl-codec-kit:string-to-octets "{\"1\",\"2\",NULL}"
                                                  :encoding :utf-8)
                  text-wire))
      (is (equalp (postgres-array-elements binary-round-trip)
                  elements))
      (is (equalp (postgres-array-dimensions binary-round-trip)
                  (postgres-array-dimensions array))))
    (let* ((array (decode-value
                   registry 1007
                   (cl-codec-kit:string-to-octets "{{1,2},{3,4}}"
                                                  :encoding :utf-8)))
           (elements (postgres-array-elements array)))
      (is (equalp #(2 2) (postgres-array-dimensions array)))
      (is (= 1 (aref (aref elements 0) 0)))
      (is (= 4 (aref (aref elements 1) 1))))
    (let* ((array (decode-value
                   registry 1000
                   (cl-codec-kit:string-to-octets "{t,f,NULL}"
                                                  :encoding :utf-8)))
           (elements (postgres-array-elements array)))
      (is (eq t (aref elements 0)))
      (is (null (aref elements 1)))
      (is (sql-null-p (aref elements 2))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 1007
                      (join-octets
                       (encode-value
                        registry 1007
                        (decode-value
                         registry 1007
                         (cl-codec-kit:string-to-octets "{1}"
                                                        :encoding :utf-8))
                        :format 1)
                       (octets 0)))))))

(deftest built-in-array-registration-completeness
  (let ((registry (make-type-registry)))
    (let* ((array
             (decode-value
              registry 271
              (cl-codec-kit:string-to-octets
               "{42,18446744073709551615}" :encoding :utf-8)))
           (elements (postgres-array-elements array))
           (wire (encode-value registry 271 array :format 1))
           (round-trip (decode-value registry 271 wire :format 1)))
      (is (find-type-codec registry 271))
      (is (= 5069 (postgres-array-element-oid array)))
      (is (= 42 (aref elements 0)))
      (is (= (1- (ash 1 64)) (aref elements 1)))
      (is (equalp elements (postgres-array-elements round-trip))))
    (dolist (spec '((1017 600)
                    (1018 601)
                    (1019 602)
                    (1020 603)
                    (1027 604)
                    (629 628)
                    (719 718)
                    (1034 1033)
                    (5039 5038)))
      (destructuring-bind (array-oid element-oid) spec
        (let* ((array
                 (decode-value
                  registry array-oid
                  (cl-codec-kit:string-to-octets
                   "{\"one\",\"two\"}" :encoding :utf-8)))
               (elements (postgres-array-elements array)))
          (is (find-type-codec registry array-oid))
          (is (= element-oid (postgres-array-element-oid array)))
          (is (equalp #("one" "two") elements))
          (is (equalp
               (cl-codec-kit:string-to-octets
                "{\"one\",\"two\"}" :encoding :utf-8)
               (encode-value registry array-oid array))))))))

(deftest array-codec-validates-shape-and-wire-boundaries
  (let ((registry (make-type-registry)))
    (let* ((array
             (decode-value
              registry 1009
              (cl-codec-kit:string-to-octets
               "{\"a,b\",\"quoted\"}" :encoding :utf-8)))
           (elements (postgres-array-elements array)))
      (is (equalp #("a,b" "quoted") elements))
      (is (equalp
           (cl-codec-kit:string-to-octets
            "{\"a,b\",\"quoted\"}" :encoding :utf-8)
           (encode-value registry 1009 array))))
    (let* ((array
             (make-postgres-array
              :elements #(1 2)
              :dimensions #(2)
              :lower-bounds #(0)
              :element-oid 23))
           (wire (encode-value registry 1007 array :format 1))
           (round-trip (decode-value registry 1007 wire :format 1)))
      (is (equalp #(1 2) (postgres-array-elements round-trip)))
      (is (equalp #(2) (postgres-array-dimensions round-trip)))
      (is (equalp #(0) (postgres-array-lower-bounds round-trip))))
    (let* ((array
             (make-postgres-array
              :elements (vector (octets 0 1 255))
              :dimensions #(1)
              :lower-bounds #(1)
              :element-oid 17))
           (wire (encode-value registry 1001 array :format 1))
           (round-trip (decode-value registry 1001 wire :format 1)))
      (is (equalp (vector (octets 0 1 255))
                  (postgres-array-elements round-trip))))
    (assert-signals 'parameter-error
                    (lambda ()
                      (encode-value
                       registry 1007
                       (make-postgres-array
                        :elements #(#(1 2) #(3))
                        :element-oid 23))))
    (assert-signals 'parameter-error
                    (lambda ()
                      (encode-value
                       registry 1007
                       (make-postgres-array
                        :elements #(1)
                        :element-oid 25))))
    (assert-signals 'parameter-error
                    (lambda ()
                      (encode-value
                       registry 1007
                       (make-postgres-array
                        :elements #(1 2)
                        :dimensions #(3)
                        :element-oid 23))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (decode-value
                       registry 1009
                       (cl-codec-kit:string-to-octets
                        "{\"unterminated"
                        :encoding :utf-8))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (decode-value
                       registry 1007
                       (octets
                        0 0 0 1
                        0 0 0 2
                        0 0 0 23
                        0 0 0 1
                        0 0 0 1
                       0 0 0 4
                       0 0 0 1)
                       :format 1)))))

(deftest array-codec-covers-list-and-malformed-boundaries
  (let ((registry (make-type-registry))
        (escaped-text (format nil "{\"a~C~Cb\",\"a~C~Cb\"}"
                              #\\ #\\ #\\ #\")))
    (labels ((array-wire (dimensions-count has-null element-oid
                          dimensions lower-bounds &optional body)
               (let ((builder (make-octet-builder)))
                 (append-i32 builder dimensions-count)
                 (append-i32 builder has-null)
                 (append-i32 builder element-oid)
                 (dolist (dimension dimensions)
                   (append-i32 builder dimension))
                 (dolist (lower-bound lower-bounds)
                   (append-i32 builder lower-bound))
                 (append-octets builder (or body #()))
                 (builder-octets builder))))
      (is (equalp
           (cl-codec-kit:string-to-octets "{\"1\",\"2\"}" :encoding :utf-8)
           (encode-value registry 1007 '(1 2))))
      (assert-signals 'parameter-error
                      (lambda ()
                        (encode-value registry 1007 "scalar")))
      (let* ((wire
               (encode-value
                registry 1007
                (make-postgres-array
                 :elements '(1 2)
                 :dimensions '(2)
                 :lower-bounds '(0)
                 :element-oid 23)
                :format 1))
             (array (decode-value registry 1007 wire :format 1)))
        (is (equalp #(2) (postgres-array-dimensions array)))
        (is (equalp #(0) (postgres-array-lower-bounds array))))
      (let* ((wire
               (encode-value
                registry 1007
                (make-postgres-array
                 :elements #()
                 :element-oid 23)
                :format 1))
             (array (decode-value registry 1007 wire :format 1)))
        (is (equalp #() (postgres-array-elements array)))
        (is (equalp #(0) (postgres-array-dimensions array))))
      (assert-signals 'parameter-error
                      (lambda ()
                        (encode-value
                         registry 1007
                         (make-postgres-array
                          :elements #(1 2)
                          :lower-bounds '(0 1)
                          :element-oid 23))))
      (assert-signals 'parameter-error
                      (lambda ()
                        (encode-value
                         registry 1007
                         (make-postgres-array
                          :elements #(1 2)
                          :lower-bounds #(2147483648)
                          :element-oid 23))))
      (let* ((array
               (decode-value
                registry 1009
                (cl-codec-kit:string-to-octets escaped-text
                                               :encoding :utf-8)))
             (elements (postgres-array-elements array)))
        (is (equalp
             (vector (format nil "a~Cb" #\\)
                     (format nil "a~Cb" #\"))
             elements))
        (is (equalp
             (cl-codec-kit:string-to-octets escaped-text :encoding :utf-8)
             (encode-value registry 1009 array))))
      (dolist (text (list "1"
                          "{1,}"
                          "{1"
                          "{{"
                          "{{1};2}"
                          "{}junk"
                          (concatenate 'string "{\"x" (string #\\))))
        (assert-signals 'protocol-error
                        (lambda ()
                          (decode-value
                           registry 1009
                           (cl-codec-kit:string-to-octets text
                                                          :encoding :utf-8)))))
      (assert-signals 'protocol-error
                      (lambda ()
                        (decode-value
                         registry 1007
                         (array-wire 7 0 23 nil nil)
                         :format 1)))
      (assert-signals 'protocol-error
                      (lambda ()
                        (decode-value
                         registry 1007
                         (array-wire 1 2 23 '(1) '(1))
                         :format 1)))
      (assert-signals 'protocol-error
                      (lambda ()
                        (decode-value
                         registry 1007
                         (array-wire 1 0 25 '(1) '(1))
                         :format 1)))
      (assert-signals 'protocol-error
                      (lambda ()
                        (decode-value
                         registry 1007
                         (array-wire 1 0 23 '(-1) '(1))
                         :format 1)))
      (assert-signals 'protocol-error
                      (lambda ()
                        (decode-value
                         registry 1007
                         (array-wire 1 0 23 '(1000001) '(1))
                         :format 1)))
      (assert-signals 'protocol-error
                      (lambda ()
                        (decode-value
                         registry 1007
                         (array-wire 1 0 23 '(1) '(1)
                                     (octets 255 255 255 255))
                         :format 1)))
      (assert-signals 'protocol-error
                      (lambda ()
                        (decode-value
                         registry 1007
                         (array-wire 1 0 23 '(1) '(1)
                                     (octets 255 255 255 254))
                         :format 1)))
      (assert-signals 'protocol-error
                      (lambda ()
                        (decode-value
                         registry 1007
                         (array-wire 1 0 23 '(1) '(1)
                                     (octets 0 0 0 4 1))
                         :format 1)))
      (assert-signals 'protocol-error
                      (lambda ()
                        (decode-value
                         registry 1007
                         (array-wire 1 0 23 '(1) '(1)
                                     (octets 0 0 0 4 0 0 0 1 9))
                         :format 1)))
      (assert-signals 'protocol-error
                      (lambda ()
                        (decode-value
                         registry 1007
                         (array-wire 1 0 23 '(1) '(1)
                                     (octets 0 0 0 1 1 9))
                         :format 1))))))

(deftest extended-built-in-type-codecs
  (let ((registry (make-type-registry)))
    (dolist (spec '((18 "char")
                    (19 "name")
                    (142 "xml")
                    (2205 "regclass")
                    (3220 "1/2")
                    (4072 "jsonpath")))
      (destructuring-bind (oid value) spec
        (let ((wire (cl-codec-kit:string-to-octets value :encoding :utf-8)))
          (is (string= value (decode-value registry oid wire)))
          (is (equalp wire (encode-value registry oid value))))))
    (let* ((wire (cl-codec-kit:string-to-octets "1 -2 32767"
                                                :encoding :utf-8))
           (value (decode-value registry 22 wire)))
      (is (equalp #(1 -2 32767) value))
      (is (equalp wire (encode-value registry 22 value))))
    (let* ((wire (cl-codec-kit:string-to-octets "0 42 4294967295"
                                                :encoding :utf-8))
           (value (decode-value registry 30 wire)))
      (is (equalp #(0 42 4294967295) value))
      (is (equalp wire (encode-value registry 30 value))))
    (let ((xid-wire (octets 0 0 0 42))
          (xid8-wire (octets 0 0 0 0 0 0 0 42)))
      (is (= 42 (decode-value registry 28 xid-wire :format 1)))
      (is (equalp xid-wire (encode-value registry 28 42 :format 1)))
      (is (= 42 (decode-value registry 5069 xid8-wire :format 1)))
      (is (equalp xid8-wire (encode-value registry 5069 42 :format 1))))
    (let* ((array (decode-value
                   registry 1002
                   (cl-codec-kit:string-to-octets "{a,b}"
                                                  :encoding :utf-8))))
      (is (equalp #("a" "b") (postgres-array-elements array)))
      (is (equalp (cl-codec-kit:string-to-octets "{\"a\",\"b\"}"
                                                 :encoding :utf-8)
                  (encode-value registry 1002 array))))
    (let* ((array (decode-value
                   registry 1041
                   (cl-codec-kit:string-to-octets "{192.0.2.1,192.0.2.2}"
                                                  :encoding :utf-8))))
      (is (every #'postgres-inet-p
                 (coerce (postgres-array-elements array) 'list))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 22
                      (cl-codec-kit:string-to-octets "1 invalid"
                                                     :encoding :utf-8))))))

(deftest binary-built-in-character-name-tid-vector-and-registry-codecs
  (let ((registry (make-type-registry)))
    (let ((char-value (string (code-char 255))))
      (is (= 255
             (char-code
              (aref (decode-value registry 18 (octets 255) :format 1) 0))))
      (is (equalp (octets 255)
                  (encode-value registry 18 char-value :format 1)))
      (assert-signals 'parameter-error
                     (lambda ()
                       (encode-value registry 18 "ab" :format 1))))
    (let ((name "postgres")
          (wire (octets 112 111 115 116 103 114 101 115)))
      (is (string= name (decode-value registry 19 wire :format 1)))
      (is (equalp wire (encode-value registry 19 name :format 1)))
      (let ((long-name (make-string 64 :initial-element #\x)))
        (assert-signals 'parameter-error
                       (lambda ()
                         (encode-value registry 19 long-name :format 1))))
      (assert-signals 'protocol-error
                     (lambda ()
                       (decode-value registry 19 (octets 0) :format 1))))
    (let* ((wire (octets 0 0 0 42 1 2))
           (value (decode-value registry 27 wire :format 1)))
      (is (postgres-tid-p value))
      (is (= 42 (postgres-tid-block-number value)))
      (is (= 258 (postgres-tid-offset-number value)))
      (is (equalp wire (encode-value registry 27 value :format 1))))
    (assert-signals 'parameter-error
                   (lambda ()
                     (make-postgres-tid :block-number (ash 1 32))))
    (dolist (oid '(24 2202 2203 2204 2205 2206 3734 3769 4089 4096 4191))
      (is (= 42 (decode-value registry oid (octets 0 0 0 42) :format 1)))
      (is (equalp (octets 0 0 0 42)
                  (encode-value registry oid 42 :format 1))))
    (flet ((vector-wire (element-oid values &key (lower-bound 0) (has-null 0))
             (let ((builder (make-octet-builder)))
               (append-i32 builder 1)
               (append-i32 builder has-null)
               (append-i32 builder element-oid)
               (append-i32 builder (length values))
               (append-i32 builder lower-bound)
               (loop for value across values
                     do (if (= element-oid 21)
                            (progn
                              (append-i32 builder 2)
                              (append-i16 builder value))
                            (progn
                              (append-i32 builder 4)
                              (append-u32 builder value))))
               (builder-octets builder))))
      (let ((wire (vector-wire 21 #(1 -2 32767)))
            (value nil))
        (setf value (decode-value registry 22 wire :format 1))
        (is (equalp #(1 -2 32767) value))
        (is (equalp wire (encode-value registry 22 value :format 1))))
      (let ((wire (vector-wire 26 #(0 42 4294967295)))
            (value nil))
        (setf value (decode-value registry 30 wire :format 1))
        (is (equalp #(0 42 4294967295) value))
        (is (equalp wire (encode-value registry 30 value :format 1))))
      (assert-signals 'protocol-error
                     (lambda ()
                       (decode-value registry 22
                                     (vector-wire 21 #(1) :lower-bound 1)
                                     :format 1)))
      (assert-signals 'protocol-error
                     (lambda ()
                       (decode-value registry 22
                                     (vector-wire 21 #(1) :has-null 1)
                                     :format 1))))
    (let* ((array (make-postgres-array
                   :elements (list #(1 -2) #(32767))
                   :lower-bounds #(0)
                   :element-oid 22))
           (wire (encode-value registry 1006 array :format 1))
           (decoded (decode-value registry 1006 wire :format 1)))
      (is (equalp (list #(1 -2) #(32767))
                  (coerce (postgres-array-elements decoded) 'list)))
      (is (equalp wire (encode-value registry 1006 decoded :format 1))))
    (let* ((array (make-postgres-array
                   :elements #("a" "b")
                   :lower-bounds #(0)
                   :element-oid 18))
           (wire (encode-value registry 1002 array :format 1))
           (decoded (decode-value registry 1002 wire :format 1)))
      (is (equalp #("a" "b") (postgres-array-elements decoded)))
      (is (equalp wire (encode-value registry 1002 decoded :format 1))))))

(deftest bit-and-mac-address-codecs
  (let ((registry (make-type-registry)))
    (let* ((bit-text (cl-codec-kit:string-to-octets "010011" :encoding :utf-8))
           (bit (decode-value registry 1560 bit-text))
           (varbit (decode-value registry 1562 bit-text)))
      (is (postgres-bit-string-p bit))
      (is (string= "010011" (postgres-bit-string-bits bit)))
      (is (null (postgres-bit-string-varying-p bit)))
      (is (postgres-bit-string-varying-p varbit))
      (is (equalp bit-text (encode-value registry 1560 bit)))
      (is (equalp bit-text (encode-value registry 1562 varbit))))
    (let* ((binary (octets 0 0 0 9 128 128))
           (bit (decode-value registry 1560 binary :format 1)))
      (is (string= "100000001" (postgres-bit-string-bits bit)))
      (is (equalp binary (encode-value registry 1560 bit :format 1))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value registry 1560 (octets 0 0 0 1) :format 1)))
    (assert-signals 'parameter-error
                   (lambda ()
                     (encode-value registry 1560 "0102")))
    (let* ((mac-text "08:00:2b:01:02:03")
           (mac (decode-value
                 registry 829
                 (cl-codec-kit:string-to-octets mac-text :encoding :utf-8)))
           (mac-binary (encode-value registry 829 mac :format 1))
           (mac8 (decode-value
                  registry 774
                  (cl-codec-kit:string-to-octets
                   "0800.2bff.fe01.0203" :encoding :utf-8))))
      (is (postgres-mac-address-p mac))
      (is (equalp (octets 8 0 43 1 2 3)
                  (postgres-mac-address-octets mac)))
      (is (string= mac-text
                   (cl-codec-kit:octets-to-string
                    (encode-value registry 829 mac) :encoding :utf-8)))
      (is (equalp (octets 8 0 43 1 2 3) mac-binary))
      (is (equalp (octets 8 0 43 255 254 1 2 3)
                  (postgres-mac-address-octets mac8)))
      (is (equalp (octets 8 0 43 1 2 3)
                  (encode-value
                   registry 829
                   (make-postgres-mac-address
                    :octets '(8 0 43 1 2 3))
                   :format 1))))
    (dolist (oid '(775 1040 1561 1563))
      (is (functionp (type-codec-binary-decoder
                      (find-type-codec registry oid)))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 829
                      (cl-codec-kit:string-to-octets
                       "08:00:2b:01:02:zz" :encoding :utf-8))))
    (assert-signals 'parameter-error
                   (lambda ()
                     (encode-value registry 774 "08:00:2b:01:02:03")))))

(deftest dynamic-custom-type-codecs
  (let ((registry (make-type-registry)))
    (register-enum-type registry
                        :oid 8000
                        :name "mood"
                        :labels '("sad" "ok" "happy"))
    (register-domain-type registry
                          :oid 8001
                          :name "positive_int"
                          :base-oid 23)
    (register-range-type registry
                         :oid 8002
                         :name "int4range"
                         :subtype-oid 23)
    (register-composite-type registry
                             :oid 8003
                             :name "number_label"
                             :field-oids #(23 25)
                             :field-names #("number" "label"))
    (register-array-type registry
                         :oid 8004
                         :name "mood[]"
                         :element-oid 8000)
    (let ((wire (cl-codec-kit:string-to-octets "happy" :encoding :utf-8)))
      (dolist (format '(0 1))
        (is (string= "happy" (decode-value registry 8000 wire :format format)))
        (is (equalp wire (encode-value registry 8000 "happy" :format format)))))
    (let ((wire (cl-codec-kit:string-to-octets "42" :encoding :utf-8))
          (binary-wire (octets 0 0 0 42)))
      (is (= 42 (decode-value registry 8001 wire)))
      (is (equalp wire (encode-value registry 8001 42)))
      (is (= 42 (decode-value registry 8001 binary-wire :format 1)))
      (is (equalp binary-wire (encode-value registry 8001 42 :format 1))))
    (let* ((wire (cl-codec-kit:string-to-octets "[1,10)" :encoding :utf-8))
           (value (decode-value registry 8002 wire)))
      (is (postgres-range-p value))
      (is (= 1 (postgres-range-lower value)))
      (is (= 10 (postgres-range-upper value)))
      (is (postgres-range-lower-inclusive value))
      (is (not (postgres-range-upper-inclusive value)))
      (is (equalp wire (encode-value registry 8002 value))))
    (let* ((wire (octets 2 0 0 0 4 0 0 0 1
                         0 0 0 4 0 0 0 10))
           (value (decode-value registry 8002 wire :format 1)))
      (is (= 1 (postgres-range-lower value)))
      (is (= 10 (postgres-range-upper value)))
      (is (equalp wire (encode-value registry 8002 value :format 1))))
    (let* ((wire (cl-codec-kit:string-to-octets
                  "(42,\"hello, world\")"
                  :encoding :utf-8))
           (value (decode-value registry 8003 wire))
           (fields (postgres-composite-fields value)))
      (is (postgres-composite-p value))
      (is (= 42 (aref fields 0)))
      (is (string= "hello, world" (aref fields 1)))
      (is (equalp #("number" "label")
                  (postgres-composite-field-names value)))
      (is (equalp wire (encode-value registry 8003 value))))
    (let* ((label-wire (cl-codec-kit:string-to-octets "hello, world"
                                                       :encoding :utf-8))
           (builder (make-octet-builder)))
      (append-i32 builder 2)
      (append-i32 builder 4)
      (append-octets builder (octets 0 0 0 42))
      (append-i32 builder (length label-wire))
      (append-octets builder label-wire)
      (let* ((wire (builder-octets builder))
             (value (decode-value registry 8003 wire :format 1))
             (fields (postgres-composite-fields value)))
        (is (= 42 (aref fields 0)))
        (is (string= "hello, world" (aref fields 1)))
        (is (equalp wire (encode-value registry 8003 value :format 1)))))
    (let* ((array (decode-value
                   registry 8004
                   (cl-codec-kit:string-to-octets "{sad,happy}"
                                                  :encoding :utf-8)))
           (binary-wire (encode-value registry 8004 array :format 1))
           (binary-array (decode-value registry 8004 binary-wire :format 1)))
      (is (equalp #("sad" "happy") (postgres-array-elements array)))
      (is (equalp (cl-codec-kit:string-to-octets "{\"sad\",\"happy\"}"
                                                 :encoding :utf-8)
                  (encode-value registry 8004 array)))
      (is (equalp #("sad" "happy")
                  (postgres-array-elements binary-array)))
      (is (equalp binary-wire
                  (encode-value registry 8004 binary-array :format 1))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 8000
                      (cl-codec-kit:string-to-octets "angry"
                                                     :encoding :utf-8))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 8002
                      (cl-codec-kit:string-to-octets "[1"
                                                     :encoding :utf-8))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 8003
                      (cl-codec-kit:string-to-octets "(42)"
                                                     :encoding :utf-8))))))

(deftest range-binary-boundaries
  (let ((registry (make-type-registry)))
    (register-range-type registry
                         :oid 8002
                         :name "int4range"
                         :subtype-oid 23)
    (let* ((value (make-postgres-range :empty-p t))
           (wire (encode-value registry 8002 value :format 1))
           (decoded (decode-value registry 8002 wire :format 1)))
      (is (equalp (octets 1) wire))
      (is (postgres-range-empty-p decoded)))
    (let* ((value (make-postgres-range
                   :lower 1
                   :upper 10
                   :lower-inclusive t
                   :upper-inclusive t))
           (wire (encode-value registry 8002 value :format 1))
           (decoded (decode-value registry 8002 wire :format 1)))
      (is (equalp (octets 6 0 0 0 4 0 0 0 1
                          0 0 0 4 0 0 0 10)
                  wire))
      (is (postgres-range-lower-inclusive decoded))
      (is (postgres-range-upper-inclusive decoded)))
    (let* ((value (make-postgres-range :lower nil :upper nil))
           (wire (encode-value registry 8002 value :format 1))
           (decoded (decode-value registry 8002 wire :format 1)))
      (is (equalp (octets #x18) wire))
      (is (null (postgres-range-lower decoded)))
      (is (null (postgres-range-upper decoded)))
      (is (not (postgres-range-lower-inclusive decoded)))
      (is (not (postgres-range-upper-inclusive decoded))))
    (dolist (wire (list
                    (octets #x20)
                    (octets #x01 0)
                    (octets #x0a)
                    (octets #x14)
                    (octets #x02 #xff #xff #xff #xff)
                    (octets #x08 #xff #xff #xff #xff)
                    (octets #x02 0 0 0 4 0)
                    (octets #x08 0 0 0 4 0)
                    (octets #x18 0)))
      (assert-signals 'protocol-error
                     (lambda ()
                       (decode-value registry 8002 wire :format 1))))
    (assert-signals 'parameter-error
                   (lambda ()
                     (encode-value registry 8002 1 :format 1)))))

(deftest multirange-codec-boundaries
  (let ((registry (make-type-registry)))
    (register-range-type registry
                         :oid 8002
                         :name "int4range"
                         :subtype-oid 23)
    (register-multirange-type registry
                              :oid 8011
                              :name "int4multirange"
                              :subtype-oid 23)
    (let* ((value
             (make-postgres-multirange
              :ranges
              (list (make-postgres-range :lower 1
                                          :upper 10
                                          :lower-inclusive t)
                    (make-postgres-range :lower 20
                                          :upper 30
                                          :upper-inclusive t))))
           (text-wire (cl-codec-kit:string-to-octets
                       "{[1,10),(20,30]}"
                       :encoding :utf-8))
           (binary-wire
             (octets
              0 0 0 2
              0 0 0 #x11
              #x02
              0 0 0 4 0 0 0 1
              0 0 0 4 0 0 0 10
              0 0 0 #x11
              #x04
              0 0 0 4 0 0 0 20
              0 0 0 4 0 0 0 30)))
      (is (equalp text-wire (encode-value registry 8011 value)))
      (let ((decoded (decode-value registry 8011 text-wire)))
        (is (postgres-multirange-p decoded))
        (is (= 2 (length (postgres-multirange-ranges decoded))))
        (is (= 1 (postgres-range-lower
                  (aref (postgres-multirange-ranges decoded) 0))))
        (is (= 30 (postgres-range-upper
                   (aref (postgres-multirange-ranges decoded) 1)))))
      (is (equalp binary-wire
                  (encode-value registry 8011 value :format 1)))
      (let ((decoded (decode-value registry 8011 binary-wire :format 1)))
        (is (= 2 (length (postgres-multirange-ranges decoded))))
        (is (equalp binary-wire
                    (encode-value registry 8011 decoded :format 1)))))
    (let ((decoded
            (decode-value
             registry
             8011
             (cl-codec-kit:string-to-octets
              "{empty,[1,10),empty}"
              :encoding :utf-8))))
      (is (= 1 (length (postgres-multirange-ranges decoded))))
      (is (= 1 (postgres-range-lower
                (aref (postgres-multirange-ranges decoded) 0))))
      (is (= 10 (postgres-range-upper
                 (aref (postgres-multirange-ranges decoded) 0)))))
    (let* ((range-wire
             (encode-value
              registry
              8002
              (make-postgres-range :lower 1
                                   :upper 10
                                   :lower-inclusive t)
              :format 1))
           (builder (make-octet-builder)))
      (append-i32 builder 2)
      (append-i32 builder 1)
      (append-u8 builder 1)
      (append-i32 builder (length range-wire))
      (append-octets builder range-wire)
      (let ((decoded (decode-value registry 8011 (builder-octets builder)
                                   :format 1)))
        (is (= 1 (length (postgres-multirange-ranges decoded))))))
    (dolist (wire (list
                    (octets #xff #xff #xff #xff)
                    (octets 0 0 0 1)
                    (octets 0 0 0 1 0 0 0 2 1)
                    (octets 0 0 0 1 0 0 0 1 1 0)))
      (assert-signals 'protocol-error
                     (lambda ()
                       (decode-value registry 8011 wire :format 1))))
    (dolist (wire '("x" "{[1,10),}" "{[1,10) [2,3)}" "{foo}"))
      (assert-signals 'protocol-error
                     (lambda ()
                       (decode-value
                        registry
                        8011
                        (cl-codec-kit:string-to-octets wire :encoding :utf-8)))))
    (assert-signals 'parameter-error
                   (lambda ()
                     (make-postgres-multirange
                      :ranges (list (make-postgres-range :empty-p t)))))
    (assert-signals 'parameter-error
                   (lambda ()
                     (make-postgres-multirange :ranges (list 1))))
    (assert-signals 'parameter-error
                   (lambda ()
                     (encode-value registry 8011 1)))))

(deftest network-codec-boundaries
  (let* ((defaults (default-type-registry))
         (registry (make-type-registry :include-defaults nil)))
    (dolist (oid '(650 651 869 1041))
      (register-type registry
                     :codec (find-type-codec defaults oid)))
    (let* ((cidr-wire (cl-codec-kit:string-to-octets
                       "192.0.2.0/24"
                       :encoding :utf-8))
           (cidr (decode-value registry 650 cidr-wire))
           (inet (decode-value
                  registry
                  869
                  (cl-codec-kit:string-to-octets
                   "192.0.2.1/24"
                   :encoding :utf-8)))
           (ipv6 (decode-value
                  registry
                  869
                  (cl-codec-kit:string-to-octets
                   "2001:0DB8:0:0:0:0:0:1/64"
                   :encoding :utf-8))))
      (is (postgres-inet-p cidr))
      (is (= 4 (postgres-inet-family cidr)))
      (is (= 24 (postgres-inet-netmask cidr)))
      (is (postgres-inet-cidr-p cidr))
      (is (equalp cidr-wire (encode-value registry 650 cidr)))
      (is (not (postgres-inet-cidr-p inet)))
      (is (equalp
           (cl-codec-kit:string-to-octets
            "192.0.2.1/24"
            :encoding :utf-8)
           (encode-value registry 869 inet)))
      (is (equalp
           (cl-codec-kit:string-to-octets
            "2001:db8::1/64"
            :encoding :utf-8)
           (encode-value registry 869 ipv6))))
    (let* ((wire (octets 2 32 0 4 192 0 2 1))
           (value (decode-value registry 869 wire :format 1)))
      (is (= 4 (postgres-inet-family value)))
      (is (= 32 (postgres-inet-netmask value)))
      (is (equalp wire (encode-value registry 869 value :format 1))))
    (let* ((wire (octets 3 32 1 16
                         32 1 #x0d #xb8
                         0 0 0 0 0 0 0 0 0 0 0 0))
           (value (decode-value registry 650 wire :format 1)))
      (is (postgres-inet-cidr-p value))
      (is (equalp wire (encode-value registry 650 value :format 1))))
    (let* ((array (decode-value
                   registry
                   1041
                   (cl-codec-kit:string-to-octets
                    "{\"192.0.2.1\",\"198.51.100.2\"}"
                    :encoding :utf-8)))
           (wire (encode-value registry 1041 array :format 1))
           (round-trip (decode-value registry 1041 wire :format 1)))
      (is (postgres-array-p round-trip))
      (is (every #'postgres-inet-p
                 (coerce (postgres-array-elements round-trip) 'list))))
    (dolist (wire (list
                    (octets 2 33 0 4 192 0 2 1)
                    (octets 2 32 0 16 192 0 2 1)
                    (octets 4 32 0 4 192 0 2 1)
                    (octets 2 32 2 4 192 0 2 1)))
      (assert-signals 'protocol-error
                      (lambda ()
                        (decode-value registry 869 wire :format 1))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value registry 650
                                   (octets 2 32 0 4 192 0 2 0)
                                   :format 1)))
    (assert-signals 'protocol-error
                    (lambda ()
                      (decode-value
                       registry
                       650
                       (cl-codec-kit:string-to-octets
                        "192.0.2.1/24"
                        :encoding :utf-8))))
    (dolist (text '("2001:::1" "2001:db8::1::2" "1.2.3.4/33"))
      (assert-signals 'protocol-error
                      (lambda ()
                        (decode-value
                         registry
                         869
                         (cl-codec-kit:string-to-octets
                          text
                          :encoding :utf-8)))))
    (assert-signals 'parameter-error
                    (lambda ()
                      (make-postgres-inet
                       :octets #(192 0 2 1)
                       :netmask 24
                       :cidr-p t)))
    (assert-signals 'parameter-error
                    (lambda ()
                      (make-postgres-inet :octets #(1 2 3))))))

(deftest structured-codec-boundaries
  (let ((registry (make-type-registry)))
    (register-range-type registry
                         :oid 8002
                         :name "int4range"
                         :subtype-oid 23)
    (register-composite-type registry
                             :oid 8003
                             :name "number_label"
                             :field-oids #(23 25)
                             :field-names #("number" "label"))
    (register-range-type registry
                         :oid 8005
                         :name "text_range"
                         :subtype-oid 25)
    (register-composite-type registry
                             :oid 8006
                             :name "empty_composite"
                             :field-oids #()
                             :field-names #())
    (let ((wire (cl-codec-kit:string-to-octets "empty" :encoding :utf-8))
          (value (decode-value
                  registry 8002
                  (cl-codec-kit:string-to-octets "empty" :encoding :utf-8))))
      (is (postgres-range-empty-p value))
      (is (equalp wire (encode-value registry 8002 value))))
    (let* ((value (make-postgres-range :lower nil :upper nil))
           (wire (cl-codec-kit:string-to-octets "(,)" :encoding :utf-8))
           (encoded (encode-value registry 8002 value))
           (decoded (decode-value registry 8002 wire)))
      (is (equalp wire encoded))
      (is (null (postgres-range-lower decoded)))
      (is (null (postgres-range-upper decoded))))
    (let* ((wire (cl-codec-kit:string-to-octets
                  "[\"a,b\",\"a\\\\b\"]"
                  :encoding :utf-8))
           (value (decode-value registry 8005 wire)))
      (is (string= "a,b" (postgres-range-lower value)))
      (is (string= "a\\b" (postgres-range-upper value)))
      (is (equalp wire (encode-value registry 8005 value))))
    (dolist (wire '("x" "[1,2}" "[1,2,3)" "[1\"2\",3)"
                    "[\"1\"x,2)" "[\"1,2)"))
      (assert-signals 'protocol-error
                     (lambda ()
                       (decode-value
                        registry 8002
                        (cl-codec-kit:string-to-octets wire
                                                       :encoding :utf-8)))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value registry 8002 (octets #xc3 #x28))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry
                      8002
                      (cl-codec-kit:string-to-octets "x1,2)"
                                                      :encoding :utf-8))))
    (assert-signals 'parameter-error
                   (lambda () (encode-value registry 8002 1)))
    (let* ((wire (cl-codec-kit:string-to-octets "(,hello)"
                                                  :encoding :utf-8))
           (value (decode-value registry 8003 wire))
           (fields (postgres-composite-fields value)))
      (is (sql-null-p (aref fields 0)))
      (is (string= "hello" (aref fields 1)))
      (is (equalp wire (encode-value registry 8003 value))))
    (is (equalp (cl-codec-kit:string-to-octets "(42,hello)"
                                               :encoding :utf-8)
                (encode-value registry 8003 '(42 "hello"))))
    (let* ((value (decode-value
                   registry 8006
                   (cl-codec-kit:string-to-octets "()" :encoding :utf-8)))
           (fields (postgres-composite-fields value)))
      (is (zerop (length fields)))
      (is (equalp (cl-codec-kit:string-to-octets "()" :encoding :utf-8)
                  (encode-value registry 8006 value))))
    (dolist (wire '("x" "(42" "42)"))
      (assert-signals 'protocol-error
                     (lambda ()
                       (decode-value
                        registry 8003
                        (cl-codec-kit:string-to-octets wire
                                                       :encoding :utf-8)))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value
                      registry 8003
                      (octets #x28 #xc3 #x28 #x2c #x68 #x69 #x29))))
    (assert-signals 'parameter-error
                   (lambda ()
                     (encode-value
                      registry
                      8003
                      (make-postgres-composite
                       :fields #(42 "hello")
                       :field-oids #(23 23)))))
    (let* ((value (make-postgres-composite
                   :fields (vector +sql-null+ "hello")
                   :field-oids #(23 25)))
           (label-wire (cl-codec-kit:string-to-octets "hello"
                                                       :encoding :utf-8))
           (builder (make-octet-builder)))
      (append-i32 builder 2)
      (append-i32 builder -1)
      (append-i32 builder (length label-wire))
      (append-octets builder label-wire)
      (let* ((wire (builder-octets builder))
             (decoded (decode-value registry 8003 wire :format 1))
             (fields (postgres-composite-fields decoded)))
        (is (sql-null-p (aref fields 0)))
        (is (string= "hello" (aref fields 1)))
        (is (equalp wire (encode-value registry 8003 value :format 1)))))
    (dolist (wire (list
                    (octets #xff #xff #xff #xff)
                    (octets 0 0 0 1)
                    (octets 0 0 0 2 #xff #xff #xff #xfe)
                    (octets 0 0 0 2 0 0 0 4 1)
                    (octets 0 0 0 2
                            #xff #xff #xff #xff
                            #xff #xff #xff #xff
                            0)))
      (assert-signals 'protocol-error
                     (lambda ()
                       (decode-value registry 8003 wire :format 1))))
    (register-type
     registry
     :oid 8007
     :name "nullish"
     :text-decoder #'identity
     :text-encoder (lambda (value)
                     (declare (ignore value))
                     nil))
    (register-composite-type registry
                             :oid 8008
                             :name "nullish_composite"
                             :field-oids #(8007 25)
                             :field-names #("value" "label"))
    (assert-signals 'parameter-error
                   (lambda ()
                     (encode-value registry 8008 '("bad" "hello"))))
    (register-type
     registry
     :oid 8009
     :name "nullish_range_base"
     :text-decoder #'identity
     :text-encoder (lambda (value)
                     (declare (ignore value))
                     nil))
    (register-range-type registry
                         :oid 8010
                         :name "nullish_range"
                         :subtype-oid 8009)
    (assert-signals 'parameter-error
                   (lambda ()
                     (encode-value
                      registry
                      8010
                      (make-postgres-range :lower "bad" :upper nil))))))

(deftest dynamic-type-registry-loading
  (let* ((registry (make-type-registry))
         (responses
           (vector
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("enumlabel" 25 -1))
             '(("8000" "mood" "sad")
               ("8000" "mood" "happy")))
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("typbasetype" 20 8))
             '(("8001" "positive_int" "23")))
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("rngsubtype" 20 8))
             '(("8002" "int4range" "23")))
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("attnum" 23 4)
               ("attname" 25 -1) ("atttypid" 20 8))
             '(("8003" "number_label" "1" "number" "23")
               ("8003" "number_label" "2" "label" "25")))
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("typelem" 20 8))
             '(("8004" "mood[]" "8000")))))
         (query-index 0)
         (connection
           (ready-memory-connection
            :on-write
            (lambda (transport octets)
              (when (and (plusp (length octets))
                         (= (aref octets 0) (char-code #\Q)))
                (is (< query-index (length responses)))
                (memory-transport-append-input
                 transport (aref responses query-index))
                (incf query-index))))))
    (unwind-protect
         (progn
           (is (eq registry (load-type-registry connection
                                                :registry registry)))
           (is (= 5 query-index))
           (is (string= "happy"
                        (decode-value
                         registry 8000
                         (cl-codec-kit:string-to-octets "happy"
                                                        :encoding :utf-8))))
           (is (= 42
                  (decode-value registry 8001
                                (octets 0 0 0 42) :format 1)))
           (let ((value
                   (decode-value
                    registry 8002
                    (cl-codec-kit:string-to-octets "[1,10)"
                                                   :encoding :utf-8))))
             (is (postgres-range-p value))
             (is (= 1 (postgres-range-lower value)))
             (is (= 10 (postgres-range-upper value))))
           (let ((value
                   (decode-value
                    registry 8003
                    (cl-codec-kit:string-to-octets "(42,hello)"
                                                   :encoding :utf-8))))
             (is (postgres-composite-p value))
             (is (= 42 (aref (postgres-composite-fields value) 0)))
             (is (string= "hello"
                          (aref (postgres-composite-fields value) 1))))
           (let ((value
                   (decode-value
                    registry 8004
                    (cl-codec-kit:string-to-octets "{sad,happy}"
                                                   :encoding :utf-8))))
             (is (equalp #("sad" "happy")
                         (postgres-array-elements value)))))
      (disconnect connection))))

(deftest dynamic-multirange-type-registry-loading
  (let* ((registry (make-type-registry))
         (responses
           (vector
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("enumlabel" 25 -1))
             '(("8000" "mood" "sad")
               ("8000" "mood" "happy")))
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("typbasetype" 20 8))
             '(("8001" "positive_int" "23")))
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("rngsubtype" 20 8))
             '(("8002" "int4range" "23")))
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("rngsubtype" 20 8))
             '(("8011" "int4multirange" "23")))
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("attnum" 23 4)
               ("attname" 25 -1) ("atttypid" 20 8))
             '(("8003" "number_label" "1" "number" "23")
               ("8003" "number_label" "2" "label" "25")))
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("typelem" 20 8))
             '(("8004" "mood[]" "8000")))))
         (query-index 0)
         (connection
           (ready-memory-connection
            :on-write
            (lambda (transport octets)
              (when (and (plusp (length octets))
                         (= (aref octets 0) (char-code #\Q)))
                (is (< query-index (length responses)))
                (memory-transport-append-input
                 transport (aref responses query-index))
                (incf query-index))))))
    (setf (gethash "server_version_num"
                   (connection-parameters connection))
          "140000")
    (unwind-protect
         (progn
           (is (eq registry (load-type-registry connection
                                                :registry registry)))
           (is (= 6 query-index))
           (let ((value
                   (decode-value
                    registry
                    8011
                    (cl-codec-kit:string-to-octets "{[1,10)}"
                                                   :encoding :utf-8))))
             (is (postgres-multirange-p value))
             (is (= 1 (length (postgres-multirange-ranges value))))
             (is (= 1 (postgres-range-lower
                       (aref (postgres-multirange-ranges value) 0))))
             (is (= 10 (postgres-range-upper
                        (aref (postgres-multirange-ranges value) 0))))))
      (disconnect connection))))

(deftest type-registry-catalog-validation
  (flet ((row (&rest values)
           (coerce values 'vector)))
    (is (eq :value
            (cl-postgresql-kit::%type-registry-catalog-value
             :value :field)))
    (assert-signals 'protocol-error
                    (lambda ()
                      (cl-postgresql-kit::%type-registry-catalog-value
                       +sql-null+ :field)))

    (dolist (value (list 0 #xffffffff))
      (is (= value
             (cl-postgresql-kit::%type-registry-catalog-oid
              value :oid))))
    (dolist (value (list -1 #x100000000 "23"))
      (assert-signals
       'protocol-error
       (lambda ()
         (cl-postgresql-kit::%type-registry-catalog-oid value :oid))))

    (is (= 0
           (cl-postgresql-kit::%type-registry-catalog-integer
            0 :integer)))
    (assert-signals
     'protocol-error
     (lambda ()
       (cl-postgresql-kit::%type-registry-catalog-integer
        "1" :integer)))

    (is (string= "name"
                 (cl-postgresql-kit::%type-registry-catalog-name
                  "name" :name)))
    (dolist (value (list "" (format nil "bad~Cname" #\Null) 42 +sql-null+))
      (assert-signals
       'protocol-error
       (lambda ()
         (cl-postgresql-kit::%type-registry-catalog-name value :name))))

    (let ((definitions
            (cl-postgresql-kit::%type-registry-enum-definitions
             (list (row 8000 "mood" "sad")
                   (row 8000 "mood" "happy")
                   (row 8001 "state" "ready")))))
      (is (= 2 (length definitions)))
      (is (= 8000 (getf (first definitions) :oid)))
      (is (string= "mood" (getf (first definitions) :name)))
      (is (equal '(
                 "sad" "happy")
                 (getf (first definitions) :labels))))
    (assert-signals
     'protocol-error
     (lambda ()
       (cl-postgresql-kit::%type-registry-enum-definitions
        (list (row 8000 "mood"))))
     )
    (assert-signals
     'protocol-error
     (lambda ()
       (cl-postgresql-kit::%type-registry-enum-definitions
        (list (row 8000 "mood" "sad")
              (row 8000 "state" "happy")))))

    (let ((definitions
            (cl-postgresql-kit::%type-registry-simple-definitions
             (list (row 8002 "int4range" 3904))
             :range)))
      (is (equal '(:oid 8002 :name "int4range" :base-oid 3904)
                 (first definitions))))
    (assert-signals
     'protocol-error
     (lambda ()
       (cl-postgresql-kit::%type-registry-simple-definitions
        (list (row 8002 "int4range"))
        :range)))

    (let ((definitions
            (cl-postgresql-kit::%type-registry-composite-definitions
             (list (row 8003 "number_label" 1 "number" 23)
                   (row 8003 "number_label" 2 "label" 25)))))
      (is (= 1 (length definitions)))
      (is (equalp #("number" "label")
                  (getf (first definitions) :field-names)))
      (is (equalp #(23 25)
                  (getf (first definitions) :field-oids))))
    (let ((definitions
            (cl-postgresql-kit::%type-registry-composite-definitions
             (list (row 8004 "empty" +sql-null+ +sql-null+ +sql-null+)))))
      (is (equalp #() (getf (first definitions) :field-names)))
      (is (equalp #() (getf (first definitions) :field-oids))))
    (dolist (rows
             (list
              (list (row 8003 "composite" 1 "field"))
              (list (row 8003 "composite" 1 "field" 23)
                    (row 8003 "other" 2 "next" 25))
              (list (row 8003 "composite" +sql-null+ "field" +sql-null+))
              (list (row 8003 "composite" 0 "field" 23))
              (list (row 8003 "composite" "1" "field" 23))
              (list (row 8003 "composite" 1 "" 23))
              (list (row 8003 "composite" 1 "field" -1))
              (list (row 8003 "composite"
                            +sql-null+ +sql-null+ +sql-null+)
                    (row 8003 "composite" 1 "field" 23))
              (list (row 8003 "composite" 1 "field" 23)
                    (row 8003 "composite"
                            +sql-null+ +sql-null+ +sql-null+))
              (list (row 8003 "composite" 2 "field" 23)
                    (row 8003 "composite" 1 "next" 25))
              (list (row 8003 "composite" 1 "field" 23)
                    (row 8003 "composite" 1 "next" 25))))
      (assert-signals
       'protocol-error
       (lambda ()
         (cl-postgresql-kit::%type-registry-composite-definitions rows))))

    (let ((definitions
            (cl-postgresql-kit::%type-registry-array-definitions
             (list (row 8005 "mood[]" 8000)))))
      (is (equal '(:oid 8005 :name "mood[]" :element-oid 8000)
                 (first definitions))))
    (dolist (rows (list (list (row 8005 "mood[]"))
                        (list (row 8005 "mood[]" -1))))
      (assert-signals
       'protocol-error
       (lambda ()
         (cl-postgresql-kit::%type-registry-array-definitions rows))))))

(deftest temporal-binary-codecs
  (let ((registry (make-type-registry)))
    (let* ((value (cl-date-kit:local-date-of 2024 2 29))
           (wire (encode-value registry 1082 value :format 1))
           (decoded (decode-value registry 1082 wire :format 1))
           (round-trip (date-value-value decoded)))
      (is (date-value-p decoded))
      (is (cl-date-kit:local-date-p round-trip))
      (is (= 2024 (cl-date-kit:local-date-year round-trip)))
      (is (= 2 (cl-date-kit:local-date-month round-trip)))
      (is (= 29 (cl-date-kit:local-date-day round-trip))))
    (let* ((value (cl-date-kit:local-time-of 12 34 56 123456000))
           (wire (encode-value registry 1083 value :format 1))
           (decoded (decode-value registry 1083 wire :format 1))
           (round-trip (time-value-value decoded)))
      (is (time-value-p decoded))
      (is (cl-date-kit:local-time-p round-trip))
      (is (= (cl-date-kit:local-time-to-nano-of-day value)
             (cl-date-kit:local-time-to-nano-of-day round-trip))))
    (let* ((value
             (cl-date-kit:local-date-time-of
              1999 12 31 23 59 59 123456000))
           (wire (encode-value registry 1114 value :format 1))
           (decoded (decode-value registry 1114 wire :format 1))
           (round-trip (timestamp-value-value decoded)))
      (is (timestamp-value-p decoded))
      (is (cl-date-kit:local-date-time-p round-trip))
      (is (= (cl-date-kit:local-date-time-year value)
             (cl-date-kit:local-date-time-year round-trip)))
      (is (= (cl-date-kit:local-date-time-month value)
             (cl-date-kit:local-date-time-month round-trip)))
      (is (= (cl-date-kit:local-date-time-day value)
             (cl-date-kit:local-date-time-day round-trip)))
      (is (= (cl-date-kit:local-date-time-hour value)
             (cl-date-kit:local-date-time-hour round-trip)))
      (is (= (cl-date-kit:local-date-time-minute value)
             (cl-date-kit:local-date-time-minute round-trip)))
      (is (= (cl-date-kit:local-date-time-second value)
             (cl-date-kit:local-date-time-second round-trip)))
      (is (= (cl-date-kit:local-date-time-nanosecond value)
             (cl-date-kit:local-date-time-nanosecond round-trip))))
    (let* ((value (cl-date-kit:instant-of-epoch-micros 1700000000123456))
           (wire (encode-value registry 1184 value :format 1))
           (decoded (decode-value registry 1184 wire :format 1))
           (round-trip (timestamptz-value-value decoded)))
      (is (timestamptz-value-p decoded))
      (is (cl-date-kit:instant-p round-trip))
      (is (= (cl-date-kit:instant-to-epoch-micros value)
             (cl-date-kit:instant-to-epoch-micros round-trip))))
    (dolist (value '("infinity" "-infinity"))
      (dolist (oid '(1082 1114 1184))
        (let* ((wire (encode-value registry oid value :format 1))
               (decoded (decode-value registry oid wire :format 1)))
          (is (cond ((date-value-p decoded)
                     (string= value (date-value-value decoded)))
                    ((timestamp-value-p decoded)
                     (string= value (timestamp-value-value decoded)))
                    ((timestamptz-value-p decoded)
                     (string= value (timestamptz-value-value decoded)))
                    (t nil))))))
    (let* ((wire (cl-codec-kit:string-to-octets "1 day 02:03:04"
                                                :encoding :utf-8))
           (decoded (decode-value registry 1186 wire))
           (encoded (encode-value registry 1186 decoded)))
           (is (interval-value-p decoded))
      (is (string= "1 day 02:03:04" (interval-value-value decoded)))
      (is (equalp wire encoded)))
    (let* ((value (make-postgres-interval :months 14
                                          :days -2
                                          :microseconds -1234567))
           (wrapped (make-interval-value :value value))
           (wire (encode-value registry 1186 wrapped :format 1))
           (decoded (decode-value registry 1186 wire :format 1))
           (round-trip (interval-value-value decoded)))
      (is (equalp
           (octets #xff #xff #xff #xff #xff #xed #x29 #x79
                   #xff #xff #xff #xfe
                   0 0 0 14)
           wire))
      (is (interval-value-p decoded))
      (is (postgres-interval-p round-trip))
      (is (= 14 (postgres-interval-months round-trip)))
      (is (= -2 (postgres-interval-days round-trip)))
      (is (= -1234567 (postgres-interval-microseconds round-trip)))
      (is (equalp wire (encode-value registry 1186 decoded :format 1))))
    (let* ((values (vector
                    (make-interval-value
                     :value (make-postgres-interval :months 1))
                    (make-interval-value
                    :value (make-postgres-interval :days 2))))
           (wire (encode-value registry 1187 values :format 1))
           (decoded (decode-value registry 1187 wire :format 1))
           (elements (coerce (postgres-array-elements decoded) 'list)))
      (is (= 2 (length elements)))
      (is (= 1
             (postgres-interval-months
              (interval-value-value (first elements)))))
      (is (= 2
             (postgres-interval-days
              (interval-value-value (second elements)))))
      (is (equalp wire (encode-value registry 1187 decoded :format 1))))
    (assert-signals 'protocol-error
                   (lambda ()
                     (decode-value registry 1186
                                   (subseq (octets 0 0 0 0 0 0 0 0
                                                   0 0 0 0 0 0 0)
                                           0 15)
                                   :format 1)))
    (assert-signals 'parameter-error
                   (lambda ()
                     (encode-value registry 1186
                                   (make-interval-value :value "1 second")
                                   :format 1)))
    (dolist (microseconds
             (list -1 cl-postgresql-kit::*postgres-microseconds-per-day*))
      (assert-signals 'protocol-error
                     (lambda ()
                       (decode-value
                        registry 1083
                        (cl-postgresql-kit::%encode-binary-signed
                         microseconds 8)
                        :format 1))))))

(deftest money-and-timetz-binary-codecs
  (let ((registry (make-type-registry)))
    (let* ((value -42)
           (wire (encode-value registry 790 value :format 1))
           (decoded (decode-value registry 790 wire :format 1)))
      (is (equalp (octets 255 255 255 255 255 255 255 214) wire))
      (is (= value decoded)))
    (let* ((value (make-postgres-time-with-time-zone
                   :microseconds 1
                   :timezone-seconds -1))
           (wrapped (make-timetz-value :value value))
           (wire (encode-value registry 1266 wrapped :format 1))
           (decoded (decode-value registry 1266 wire :format 1))
           (round-trip (timetz-value-value decoded)))
      (is (equalp (octets 0 0 0 0 0 0 0 1 255 255 255 255) wire))
      (is (timetz-value-p decoded))
      (is (postgres-time-with-time-zone-p round-trip))
      (is (= 1 (postgres-time-with-time-zone-microseconds round-trip)))
      (is (= -1 (postgres-time-with-time-zone-timezone-seconds round-trip)))
      (is (equalp wire (encode-value registry 1266 decoded :format 1))))
    (let* ((values #(42 -7))
           (wire (encode-value registry 791 values :format 1))
           (decoded (decode-value registry 791 wire :format 1)))
      (is (equalp values (postgres-array-elements decoded)))
      (is (equalp wire (encode-value registry 791 decoded :format 1))))
    (let* ((values (vector
                    (make-timetz-value
                     :value (make-postgres-time-with-time-zone
                             :microseconds 1 :timezone-seconds -1))
                    (make-timetz-value
                     :value (make-postgres-time-with-time-zone
                             :microseconds 2 :timezone-seconds 1))))
           (wire (encode-value registry 1270 values :format 1))
           (decoded (decode-value registry 1270 wire :format 1))
           (elements (coerce (postgres-array-elements decoded) 'list)))
      (is (= 2 (length elements)))
      (is (= -1
             (postgres-time-with-time-zone-timezone-seconds
              (timetz-value-value (first elements)))))
      (is (= 1
             (postgres-time-with-time-zone-timezone-seconds
              (timetz-value-value (second elements)))))
      (is (equalp wire (encode-value registry 1270 decoded :format 1))))
    (let* ((value (make-postgres-time-with-time-zone
                   :microseconds cl-postgresql-kit::+postgres-microseconds-per-day+
                   :timezone-seconds 0))
           (wire (encode-value registry 1266 value :format 1))
           (decoded (decode-value registry 1266 wire :format 1)))
      (is (= cl-postgresql-kit::+postgres-microseconds-per-day+
             (postgres-time-with-time-zone-microseconds
              (timetz-value-value decoded)))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (decode-value registry 790
                                    (octets 0 0 0 0 0 0 0)
                                    :format 1)))
    (assert-signals 'protocol-error
                    (lambda ()
                      (decode-value registry 1266
                                    (octets 0 0 0 0 0 0 0 0 0 0 0)
                                    :format 1)))
    (dolist (microseconds
             (list -1
                   (1+ cl-postgresql-kit::+postgres-microseconds-per-day+)))
      (let ((builder (make-octet-builder 12)))
        (append-i64 builder microseconds)
        (append-i32 builder 0)
        (assert-signals 'protocol-error
                        (lambda ()
                          (decode-value registry 1266
                                        (builder-octets builder)
                                        :format 1)))))
    (dolist (timezone-seconds
             (list cl-postgresql-kit::+postgres-timetz-zone-limit+
                   (- cl-postgresql-kit::+postgres-timetz-zone-limit+)))
      (let ((builder (make-octet-builder 12)))
        (append-i64 builder 0)
        (append-i32 builder timezone-seconds)
        (assert-signals 'protocol-error
                        (lambda ()
                          (decode-value registry 1266
                                        (builder-octets builder)
                                        :format 1)))))
    (assert-signals 'parameter-error
                    (lambda ()
                      (encode-value registry 1266 "12:00:00+00" :format 1)))))

(cl-weave:it-each
    ((1082 "2024-02-29" date-value-p)
     (1083 "12:34:56" time-value-p)
     (1114 "1999-12-31T23:59:59" timestamp-value-p)
     (1184 "2023-11-14T22:13:20Z" timestamptz-value-p))
  "temporal binary codecs accept ISO-8601 string parameters"
  (oid value predicate)
  (let* ((registry (make-type-registry))
         (wire (encode-value registry oid value :format 1))
         (decoded (decode-value registry oid wire :format 1)))
    (is (funcall predicate decoded))))

(cl-weave:it-each
    ((1082 date date-value-p)
     (1083 time time-value-p)
     (1114 timestamp timestamp-value-p)
     (1184 timestamptz timestamptz-value-p))
  "temporal binary codecs unwrap value wrappers"
  (oid wrapper-kind predicate)
  (let* ((registry (make-type-registry))
         (value
           (ecase wrapper-kind
             (date (make-date-value :value "2024-02-29"))
             (time (make-time-value :value "12:34:56"))
             (timestamp (make-timestamp-value :value "1999-12-31T23:59:59"))
             (timestamptz
              (make-timestamptz-value :value "2023-11-14T22:13:20Z"))))
         (wire (encode-value registry oid value :format 1))
         (decoded (decode-value registry oid wire :format 1)))
    (is (funcall predicate decoded))))

(cl-weave:it-each
    ((1082 "not-a-date")
     (1083 "not-a-time")
     (1114 "not-a-timestamp")
     (1184 "not-a-timestamptz"))
  "invalid temporal binary parameters signal parameter-error"
  (oid value)
  (let ((registry (make-type-registry)))
    (assert-signals 'parameter-error
                    (lambda ()
                      (encode-value registry oid value :format 1)))))

(cl-weave:it-each
    ((1082 "2024-02-29" date-value-p)
     (1083 "12:34:56" time-value-p)
     (1114 "1999-12-31T23:59:59" timestamp-value-p)
     (1184 "2023-11-14T22:13:20Z" timestamptz-value-p))
  "temporal text codecs round-trip ISO-8601 values"
  (oid value predicate)
  (let* ((registry (make-type-registry))
         (wire (cl-codec-kit:string-to-octets value :encoding :utf-8))
         (decoded (decode-value registry oid wire))
         (encoded (encode-value registry oid decoded)))
    (is (funcall predicate decoded))
    (is (equalp wire encoded))))

(deftest protocol-counts-are-bounded-before-allocation
  (assert-signals 'protocol-error
                 (lambda () (parse-row-description (octets 255 255))))
  (assert-signals 'protocol-error
                 (lambda () (parse-data-row (octets 255 255))))
  (assert-signals 'protocol-error
                 (lambda () (parse-copy-response (octets 0 255 255))))
  (assert-signals 'protocol-error
                 (lambda () (parse-parameter-description (octets 255 255)))))

(deftest protocol-format-code-validation
  (is (= 0 (cl-postgresql-kit::%protocol-format-code 0)))
  (is (= 1 (cl-postgresql-kit::%protocol-format-code 1)))
  (dolist (value (list -1 2 nil "0"))
    (assert-signals 'parameter-error
                   (lambda ()
                     (cl-postgresql-kit::%protocol-format-code value))))
  (is (equal '(0 0)
             (cl-postgresql-kit::%protocol-format-list 0 2)))
  (is (equal '(1 1)
             (cl-postgresql-kit::%protocol-format-list '(1) 2)))
  (is (equal '(0 1)
             (cl-postgresql-kit::%protocol-format-list #(0 1) 2)))
  (is (equal '(1)
             (cl-postgresql-kit::%protocol-format-list 1 1)))
  (is (null (cl-postgresql-kit::%protocol-format-list nil 0)))
  (assert-signals 'parameter-error
                 (lambda ()
                   (cl-postgresql-kit::%protocol-format-list '(0 1 0) 2))))

(deftest function-call-round-trip
  (let* ((input
           (join-octets
            (make-frame #\V (octets 0 0 0 2 111 107))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input))
         (arguments (list (octets 1) +sql-null+)))
    (unwind-protect
         (let ((result (function-call connection 123 arguments
                                      :argument-formats '(0 0)
                                      :result-format 0)))
           (is (equalp result (octets 111 107)))
           (is (equalp
                (memory-transport-output (connection-transport connection))
                (join-octets
                 (encode-function-call-message
                  123 arguments :argument-formats '(0 0) :result-format 0)
                 (encode-sync-message)))))
      (disconnect connection))))

(deftest function-call-default-result-format
  (let* ((input
           (join-octets
            (make-frame #\V (octets 0 0 0 2 111 107))
            (make-frame #\Z (octets (char-code #\I)))))
         (connection (ready-memory-connection :input input)))
    (unwind-protect
         (progn
           (is (equalp (octets 111 107)
                       (function-call connection 123 nil)))
           (is (equalp
                (memory-transport-output (connection-transport connection))
                (join-octets
                 (encode-function-call-message 123 nil)
                 (encode-sync-message)))))
      (disconnect connection))))

(deftest function-call-protocol-boundaries
  (labels ((ready-input (payload)
             (join-octets
              (make-frame #\V payload)
              (make-frame #\Z (octets (char-code #\I)))))
           (assert-function-error (input condition-type)
             (let ((connection (ready-memory-connection :input input)))
               (unwind-protect
                    (assert-signals condition-type
                                    (lambda ()
                                      (function-call connection 123 nil)))
                 (disconnect connection)))))
    (let ((connection (ready-memory-connection)))
      (unwind-protect
           (dolist (function-oid (list -1 #x100000000 "123"))
             (assert-signals 'parameter-error
                            (lambda ()
                              (function-call connection function-oid nil))))
        (disconnect connection)))
    (let ((connection
            (ready-memory-connection
             :input (ready-input (octets 0 0 0 2 52 50)))))
      (unwind-protect
           (is (= 42 (function-call connection 123 nil :result-type-oid 23)))
        (disconnect connection)))
    (let ((connection
            (ready-memory-connection
             :input (ready-input (octets #xff #xff #xff #xff)))))
      (unwind-protect
           (is (sql-null-p
                (function-call connection 123 nil :result-type-oid 23)))
        (disconnect connection)))
    (let* ((notice-payload
             (join-octets
              (octets (char-code #\S)) (cstring "NOTICE")
              (octets (char-code #\M)) (cstring "function notice")
              (octets 0)))
           (notification-payload
             (join-octets (octets 0 0 0 42)
                          (cstring "events")
                          (cstring "created")))
           (input
             (join-octets
              (make-frame #\N notice-payload)
              (make-frame #\A notification-payload)
              (make-frame #\V (octets 0 0 0 2 111 107))
              (make-frame #\Z (octets (char-code #\I)))))
           (connection (ready-memory-connection :input input)))
      (unwind-protect
           (progn
             (is (equalp (octets 111 107)
                         (function-call connection 123 nil)))
             (is (= 2 (length (connection-notifications connection)))))
        (disconnect connection)))
    (assert-function-error
     (join-octets
      (make-frame #\E
                  (join-octets
                   (octets (char-code #\S)) (cstring "ERROR")
                   (octets (char-code #\C)) (cstring "XX000")
                   (octets (char-code #\M)) (cstring "function failed")
                   (octets 0)))
      (make-frame #\Z (octets (char-code #\I))))
     'server-error)
    (assert-function-error
     (make-frame #\Z (octets (char-code #\I)))
     'protocol-error)
    (assert-function-error
     (join-octets
      (make-frame #\V (octets 0 0 0 2 111 107))
      (make-frame #\V (octets 0 0 0 2 111 107))
      (make-frame #\Z (octets (char-code #\I))))
     'protocol-error)
    (assert-function-error
     (join-octets
      (make-frame #\C (cstring "CALL"))
      (make-frame #\Z (octets (char-code #\I))))
     'protocol-error)))
