(in-package #:cl-postgresql-kit)

(defmacro %with-payload-slice
    ((slice end payload position length context message expected actual) &body body)
  `(let ((,end (+ ,position ,length)))
     (when (or (< ,length 0) (> ,end (length ,payload)))
       (error 'protocol-error
              :message ,message
              :context ,context
              :expected ,expected
              :actual ,actual))
     (let ((,slice (subseq ,payload ,position ,end)))
       ,@body)))

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

(defun %read-column-description (payload position)
  (multiple-value-bind (name next) (%read-cstring payload position)
    (multiple-value-bind (table-oid next) (%read-u32 payload next)
      (multiple-value-bind (attribute-number next) (%read-i16 payload next)
        (multiple-value-bind (type-oid next) (%read-u32 payload next)
          (multiple-value-bind (type-size next) (%read-i16 payload next)
            (multiple-value-bind (type-modifier next) (%read-i32 payload next)
              (multiple-value-bind (format-code next) (%read-i16 payload next)
                (unless (member format-code '(0 1))
                  (error 'protocol-error
                         :message "Invalid PostgreSQL column format code"
                         :context :row-description
                         :expected '(0 1)
                         :actual format-code))
                (values (%make-column name table-oid attribute-number type-oid
                                      type-size type-modifier format-code)
                        next)))))))))

(defun parse-row-description (payload)
  (multiple-value-bind (count position) (%read-u16 payload 0)
    (%ensure-count-capacity payload position count 19 :row-description)
    (let ((columns (make-array count)) (cursor position))
      (loop for index from 0 below count
            do (multiple-value-bind (column next)
                   (%read-column-description payload cursor)
                 (setf cursor next
                       (aref columns index) column)))
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
                     (%with-payload-slice (value end payload cursor length
                                                 :data-row
                                                 "Invalid DataRow value length"
                                                 (length payload)
                                                 length)
                       (setf (aref values index) value
                             cursor end)))))
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
    (multiple-value-bind (channel next) (%read-cstring payload position)
      (multiple-value-bind (payload-string next) (%read-cstring payload next)
        (%ensure-payload-end payload next :notification-response)
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

(defun parse-function-call-response (payload)
  (multiple-value-bind (length position) (%read-i32 payload 0)
    (cond ((= length -1)
           (%ensure-payload-end payload position :function-call-response)
           +sql-null+)
          ((minusp length)
           (error 'protocol-error
                  :message "Invalid FunctionCallResponse result length."
                  :context :function-call-response
                  :expected "-1 or a non-negative length"
                  :actual length))
          (t
           (%with-payload-slice (result end payload position length
                                        :function-call-response
                                        "FunctionCallResponse result exceeds its payload."
                                        length
                                        (- (length payload) position))
             (%ensure-payload-end payload end :function-call-response)
             result)))))

(defun parse-parameter-description (payload)
  (multiple-value-bind (count position) (%read-u16 payload 0)
    (%ensure-count-capacity payload position count 4 :parameter-description)
    (let ((oids (make-array count)))
      (loop for index from 0 below count
            do (multiple-value-bind (oid next) (%read-u32 payload position)
                 (setf position next (aref oids index) oid)))
      (%ensure-payload-end payload position :parameter-description)
      oids)))
