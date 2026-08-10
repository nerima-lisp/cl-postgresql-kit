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
      (5
       (let ((end (+ position 4)))
         (when (> end (length payload))
           (error 'protocol-error :message "Invalid MD5 authentication payload"
                  :context :authentication :expected 4 :actual (- (length payload) position)))
         (%ensure-payload-end payload end :authentication)
         (list :type :md5-password :salt (subseq payload position end))))
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
              :context :authentication :expected '(0 3 5 7 8 9 10 11 12) :actual code)))))

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

(defstruct (column (:constructor %make-column
                                (name table-oid attribute-number type-oid type-size
                                      type-modifier format-code)))
  (name "" :type string)
  (table-oid 0 :type integer)
  (attribute-number 0 :type integer)
  (type-oid 0 :type integer)
  (type-size 0 :type integer)
  (type-modifier 0 :type integer)
  (format-code 0 :type integer))

(defun parse-row-description (payload)
  (multiple-value-bind (count position) (%read-u16 payload 0)
    (%ensure-count-capacity payload position count 19 :row-description)
    (let ((columns (make-array count)) (cursor position))
      (loop for index from 0 below count
            do (multiple-value-bind (name next) (%read-cstring payload cursor)
                 (setf cursor next)
                 (multiple-value-bind (table-oid next) (%read-u32 payload cursor)
                   (setf cursor next)
                   (multiple-value-bind (attribute-number next) (%read-i16 payload cursor)
                     (setf cursor next)
                     (multiple-value-bind (type-oid next) (%read-u32 payload cursor)
                       (setf cursor next)
                       (multiple-value-bind (type-size next) (%read-i16 payload cursor)
                         (setf cursor next)
                         (multiple-value-bind (type-modifier next) (%read-i32 payload cursor)
                           (setf cursor next)
                           (multiple-value-bind (format-code next) (%read-i16 payload cursor)
                             (setf cursor next)
                             (unless (member format-code '(0 1))
                               (error 'protocol-error
                                      :message "Invalid PostgreSQL column format code"
                                      :context :row-description
                                      :expected '(0 1)
                                      :actual format-code))
                             (setf (aref columns index)
                                   (%make-column name table-oid attribute-number type-oid
                                                 type-size type-modifier format-code))))))))))
      (%ensure-payload-end payload cursor :row-description)
      columns)))

(defun parse-data-row (payload)
  (multiple-value-bind (count position) (%read-u16 payload 0)
    (%ensure-count-capacity payload position count 4 :data-row)
    (let ((values (make-array count)) (cursor position))
      (loop for index from 0 below count
            do (multiple-value-bind (length next) (%read-i32 payload cursor)
                 (setf cursor next)
                 (if (= length -1)
                     (setf (aref values index) +sql-null+)
                     (progn
                       (when (or (< length 0) (> (+ cursor length) (length payload)))
                         (error 'protocol-error :message "Invalid DataRow value length"
                                :context :data-row :expected (length payload) :actual length))
                       (setf (aref values index) (subseq payload cursor (+ cursor length)))
                       (incf cursor length)))))
      (%ensure-payload-end payload cursor :data-row)
      values)))

(defun parse-command-complete (payload)
  (multiple-value-bind (tag position) (%read-cstring payload 0)
    (%ensure-payload-end payload position :command-complete)
    tag))

(defun %field-key (code)
  (case code
    (#\S :severity)
    (#\V :severity-localized)
    (#\C :sqlstate)
    (#\M :message)
    (#\D :detail)
    (#\H :hint)
    (#\P :position)
    (#\p :internal-position)
    (#\q :internal-query)
    (#\W :where)
    (#\s :schema)
    (#\t :table)
    (#\c :column)
    (#\d :datatype)
    (#\n :constraint)
    (#\F :file)
    (#\L :line)
    (#\R :routine)
    (otherwise nil)))

(defun %parse-fields (payload &key error-p)
  (let ((fields nil) (unknown-fields nil) (position 0) (terminated nil))
    (loop while (< position (length payload))
          for code = (code-char (%octet-at payload position))
          do (incf position)
             (if (zerop (%octet-at payload (1- position)))
                 (progn
                   (setf terminated t)
                   (return))
                 (multiple-value-bind (value next) (%read-cstring payload position)
                   (setf position next)
                   (let ((key (%field-key code)))
                     (if key
                         (push (cons key value) fields)
                         (push (cons code value) unknown-fields))))))
    (unless (and terminated (= position (length payload)))
      (error 'protocol-error :message "Unterminated PostgreSQL field list"
             :context (if error-p :error-response :notice-response)))
    (when unknown-fields
      (push (cons :unknown-fields (nreverse unknown-fields)) fields))
    (nreverse fields)))

(defun parse-error-response (payload)
  (%parse-fields payload :error-p t))

(defun parse-notification-response (payload)
  (multiple-value-bind (pid position) (%read-i32 payload 0)
    (multiple-value-bind (channel position) (%read-cstring payload position)
      (multiple-value-bind (payload-string end) (%read-cstring payload position)
        (%ensure-payload-end payload end :notification-response)
        (list :pid pid :channel channel :payload payload-string)))))

(defun parse-copy-response (payload)
  (let ((format (%octet-at payload 0)))
    (unless (member format '(0 1))
      (error 'protocol-error
             :context :copy-response
             :message "COPY format must be text (0) or binary (1)."))
    (multiple-value-bind (count position) (%read-u16 payload 1)
      (%ensure-count-capacity payload position count 2 :copy-response)
      (let ((formats (make-array count)))
        (loop for index from 0 below count
              do (multiple-value-bind (code next) (%read-u16 payload position)
                   (unless (member code '(0 1))
                     (error 'protocol-error
                            :context :copy-response
                            :message "COPY column format must be text (0) or binary (1)."))
                   (setf position next (aref formats index) code)))
        (%ensure-payload-end payload position :copy-response)
        (list :format format :column-count count :formats formats)))))

(defun parse-copy-data (payload)
  (copy-seq payload))
