(in-package #:cl-postgresql-kit)

(defun parse-authentication (payload)
  (multiple-value-bind (code position) (%read-u32 payload 0)
    (case code
      (0 (progn
           (%ensure-payload-end payload position :authentication)
           (list :type :ok)))
      (3 (progn
           (%ensure-payload-end payload position :authentication)
           (list :type :cleartext-password)))
      (2 (progn
           (%ensure-payload-end payload position :authentication)
           (list :type :kerberos-v5 :data #())))
      (5
       (%with-payload-slice (salt end payload position 4
                                   :authentication
                                   "Invalid MD5 authentication payload"
                                   4
                                   (- (length payload) position))
         (%ensure-payload-end payload end :authentication)
         (list :type :md5-password :salt salt)))
      (7
       (%ensure-payload-end payload position :authentication)
       (list :type :gss :data #()))
      (8
       (list :type :gss-continue :data (subseq payload position)))
      (9
       (%ensure-payload-end payload position :authentication)
       (list :type :sspi :data #()))
      (10
       (let ((mechanisms nil) (cursor position) (terminated nil))
         (loop while (< cursor (length payload))
               do (if (zerop (%octet-at payload cursor))
                      (progn
                        (incf cursor)
                        (setf terminated t)
                        (return))
                      (multiple-value-bind (mechanism next) (%read-cstring payload cursor)
                        (push mechanism mechanisms)
                        (setf cursor next))))
         (unless (and terminated (= cursor (length payload)))
           (error 'protocol-error :message "Invalid SASL mechanism list"
                  :context :authentication :expected :nul-terminated :actual cursor))
         (list :type :sasl :mechanisms (nreverse mechanisms))))
      (11 (list :type :sasl-continue
                :data (cl-codec-kit:octets-to-string (subseq payload position)
                                                     :encoding :utf-8)))
      (12 (list :type :sasl-final
                :data (cl-codec-kit:octets-to-string (subseq payload position)
                                                     :encoding :utf-8)))
      (otherwise
       (error 'protocol-error :message "Unsupported PostgreSQL authentication request"
              :context :authentication :expected '(0 2 3 5 7 8 9 10 11 12) :actual code)))))

(defun parse-parameter-status (payload)
  (multiple-value-bind (name position) (%read-cstring payload 0)
    (multiple-value-bind (value end) (%read-cstring payload position)
      (%ensure-payload-end payload end :parameter-status)
      (cons name value))))

(defun parse-backend-key-data (payload &key protocol-version)
  (when (and protocol-version
             (not (%supported-protocol-version-p protocol-version)))
    (error 'parameter-error
           :parameter protocol-version
           :message "Only PostgreSQL protocol versions 3.0 and 3.2 are supported."))
  (multiple-value-bind (pid position) (%read-i32 payload 0)
    (if (and protocol-version
             (= protocol-version +protocol-version-3.2+))
        (let ((secret-length (- (length payload) position)))
          (unless (<= 4 secret-length 256)
            (error 'protocol-error
                   :message "Invalid variable-length PostgreSQL backend secret key"
                   :context :backend-key-data
                   :expected "4 to 256 octets"
                   :actual secret-length))
          (values pid (subseq payload position)))
        (multiple-value-bind (secret end) (%read-i32 payload position)
          (%ensure-payload-end payload end :backend-key-data)
          (values pid secret)))))

(defun parse-negotiate-protocol-version (payload)
  (multiple-value-bind (newest-minor-version position) (%read-i32 payload 0)
    (when (minusp newest-minor-version)
      (error 'protocol-error
             :message "PostgreSQL protocol negotiation returned a negative minor version"
             :context :negotiate-protocol-version
             :expected '(integer 0 *)
             :actual newest-minor-version))
    (multiple-value-bind (count position) (%read-i32 payload position)
      (%ensure-count-capacity payload position count 1 :negotiate-protocol-version)
      (let ((options nil)
            (cursor position))
        (loop repeat count
              do (multiple-value-bind (option next) (%read-cstring payload cursor)
                   (push option options)
                   (setf cursor next)))
        (%ensure-payload-end payload cursor :negotiate-protocol-version)
        (list :newest-minor-version newest-minor-version
              :unrecognized-options (nreverse options))))))

(defun parse-ready-for-query (payload)
  (%ensure-payload-end payload 1 :ready-for-query)
  (let ((status (code-char (%octet-at payload 0))))
    (unless (member status '(#\I #\T #\E))
      (error 'protocol-error :message "Invalid ReadyForQuery transaction status"
             :context :ready-for-query :expected "I, T, or E" :actual status))
    status))
