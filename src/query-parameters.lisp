(in-package #:cl-postgresql-kit)

(defun %parameter-value-parts (connection parameter format)
  (if (typed-value-p parameter)
      (values (typed-value-type-oid parameter)
              (or format (typed-value-format parameter) 0)
              (encode-value (connection-type-registry connection)
                            (typed-value-type-oid parameter)
                            (typed-value-value parameter)
                            :format (or format (typed-value-format parameter) 0)))
      (values 0 (or format 0)
              (if (and (vectorp parameter)
                       (subtypep (array-element-type parameter)
                                '(unsigned-byte 8)))
                  parameter
                  (encode-value (connection-type-registry connection)
                                25 parameter :format (or format 0))))))

(defun %prepare-parameters (connection parameters parameter-type-oids parameter-formats)
  (loop for parameter in parameters
        for index from 0
        for explicit-oid = (and parameter-type-oids
                                (elt parameter-type-oids index))
        for explicit-format = (and parameter-formats
                                   (elt parameter-formats index))
        collect (multiple-value-bind (typed-oid format value)
                    (%parameter-value-parts connection parameter explicit-format)
                  (list :oid (or explicit-oid typed-oid)
                        :format format
                        :value value))))

(defun %parameter-sequence-list (value parameter)
  (cond ((null value) nil)
        ((listp value) (copy-list value))
        ((and (vectorp value) (not (stringp value)))
         (coerce value 'list))
        (t (error 'parameter-error
                  :parameter value
                  :message (format nil "~A must be a list or vector."
                                   parameter)))))

(defun %parameter-oids-list (value &optional count)
  (let ((oids (%parameter-sequence-list value :parameter-type-oids)))
    (when (and count oids (/= (length oids) count))
      (error 'parameter-error
             :parameter value
             :message "The parameter type OID count must match the parameter count."))
    (dolist (oid oids)
      (unless (and (integerp oid) (<= 0 oid #xffffffff))
        (error 'parameter-error
               :parameter oid
               :message "Parameter type OIDs must be unsigned 32-bit integers.")))
    oids))

(defun %parameter-formats-list (value count)
  (if (zerop count)
      (progn
        (dolist (format (cond ((null value) nil)
                             ((listp value) value)
                             ((and (vectorp value) (not (stringp value)))
                              (coerce value 'list))
                             (t (list value))))
          (%protocol-format-code format))
        nil)
      (%protocol-format-list value count)))

(defun %query-string-or-nil (value parameter)
  (when value
    (unless (stringp value)
      (error 'parameter-error
             :parameter value
             :message (format nil "~A must be a string or NIL." parameter))))
  value)

(defun %resolve-prepared-statement (connection statement-name sql
                                    parameter-count parameter-type-oids
                                    &optional expected-prepared-statement)
  "Resolve and validate a cached prepared statement for an extended query.

The returned values are the cached statement, when present, and the effective
parameter type OIDs.  Keeping this validation in one place ensures that the
single-exchange and pipeline builders reject the same stale cache entries."
  (let* ((prepared-statement
           (and statement-name
                (gethash statement-name
                         (connection--prepared-statements connection))))
         (cached-parameter-oids
           (when prepared-statement
             (unless (typep prepared-statement 'prepared-statement)
               (error 'parameter-error
                      :parameter statement-name
                      :message "The cached prepared statement is invalid."))
             (unless (string= sql (prepared-statement-sql prepared-statement))
               (error 'parameter-error
                      :parameter sql
                      :message "SQL does not match the cached prepared statement."))
             (%parameter-oids-list
              (prepared-statement-parameter-type-oids prepared-statement)
              parameter-count)))
         (effective-parameter-type-oids parameter-type-oids))
    (when expected-prepared-statement
      (unless (eq prepared-statement expected-prepared-statement)
        (error 'parameter-error
               :parameter statement-name
               :message
               "The prepared statement is no longer active in the connection cache.")))
    (when (and prepared-statement cached-parameter-oids parameter-type-oids
               (not (equal cached-parameter-oids parameter-type-oids)))
      (error 'parameter-error
             :parameter parameter-type-oids
             :message "Parameter type OIDs do not match the cached prepared statement."))
    (when prepared-statement
      (setf effective-parameter-type-oids
            (or parameter-type-oids cached-parameter-oids)))
    (values prepared-statement effective-parameter-type-oids)))

(defun %pipeline-result-formats-list (value)
  (mapcar #'%protocol-format-code
          (cond ((null value) nil)
                ((listp value) (copy-list value))
                ((and (vectorp value) (not (stringp value)))
                 (coerce value 'list))
                (t (list value)))))

(defun make-pipeline-request (sql &key parameters parameter-type-oids
                                      parameter-formats result-formats
                                      statement-name portal-name)
  "Create one request for QUERY-PIPELINE.

The request is always sent through PostgreSQL's extended query protocol.  A
string passed directly to QUERY-PIPELINE is shorthand for a request with no
parameters and generated statement and portal names."
  (check-type sql string)
  (let* ((parameter-list (%parameter-sequence-list parameters :parameters))
         (parameter-count (length parameter-list))
         (type-oids (%parameter-oids-list parameter-type-oids parameter-count))
         (formats (%parameter-formats-list parameter-formats parameter-count))
         (result-format-list (%pipeline-result-formats-list result-formats)))
    (%make-pipeline-request
     :sql sql
     :parameters parameter-list
     :parameter-type-oids type-oids
     :parameter-formats formats
     :result-formats result-format-list
     :statement-name (%query-string-or-nil statement-name :statement-name)
     :portal-name (%query-string-or-nil portal-name :portal-name))))

(defun %pipeline-request-list (value)
  (let ((items
          (cond ((stringp value) (list value))
                ((typep value 'pipeline-request) (list value))
                ((listp value) (copy-list value))
                ((and (vectorp value) (not (stringp value)))
                 (coerce value 'list))
                (t (error 'parameter-error
                          :parameter value
                          :message "Pipeline requests must be a request, string, list, or vector.")))))
    (unless items
      (error 'parameter-error
             :parameter value
             :message "QUERY-PIPELINE requires at least one request."))
    (mapcar (lambda (item)
              (cond ((stringp item)
                     (make-pipeline-request item))
                    ((typep item 'pipeline-request)
                     (make-pipeline-request
                      (pipeline-request-sql item)
                      :parameters (pipeline-request-parameters item)
                      :parameter-type-oids (pipeline-request-parameter-type-oids item)
                      :parameter-formats (pipeline-request-parameter-formats item)
                      :result-formats (pipeline-request-result-formats item)
                      :statement-name (pipeline-request-statement-name item)
                      :portal-name (pipeline-request-portal-name item)))
                    (t
                     (error 'parameter-error
                            :parameter item
                            :message "Each pipeline request must be a string or PIPELINE-REQUEST."))))
            items)))

(defun %max-rows-value (value)
  (when value
    (unless (and (integerp value) (<= 0 value #xffffffff))
      (error 'parameter-error
             :parameter value
             :message "MAX-ROWS must be an unsigned 32-bit integer.")))
  value)

(defun %max-result-bytes-value (value)
  (when value
    (unless (and (integerp value) (<= 0 value))
      (error 'parameter-error
             :parameter value
             :message "MAX-RESULT-BYTES must be a non-negative integer.")))
  value)

(defun %query-notice-list (notices)
  (nreverse notices))
