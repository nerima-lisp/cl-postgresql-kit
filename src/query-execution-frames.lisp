(in-package #:cl-postgresql-kit)

(defun %extended-query-parse-frame
    (sql parameter-type-oids statement-name prepared-statement prepared-parameters)
  (unless prepared-statement
    (encode-parse-message
     sql
     :statement-name (or statement-name "")
     :parameter-type-oids
     (or parameter-type-oids
         (mapcar (lambda (parameter) (getf parameter :oid))
                 prepared-parameters)))))

(defun %extended-query-bind-frame
    (prepared-parameters statement-name portal-name result-formats)
  (encode-bind-message
   (mapcar (lambda (parameter) (getf parameter :value))
           prepared-parameters)
   :portal-name (or portal-name "")
   :statement-name (or statement-name "")
   :parameter-formats
   (mapcar (lambda (parameter) (getf parameter :format))
           prepared-parameters)
   :result-formats result-formats))

(defun %extended-query-exchange-frames
    (sql parameter-type-oids statement-name portal-name max-rows
     result-formats prepared-statement prepared-parameters)
  "Build the frontend frames for an extended query exchange."
  (let ((parse-frame
          (%extended-query-parse-frame
           sql parameter-type-oids statement-name prepared-statement
           prepared-parameters)))
    (append (when parse-frame (list parse-frame))
            (list
             (%extended-query-bind-frame
              prepared-parameters statement-name portal-name result-formats)
             (encode-describe-message (or portal-name "") :kind :portal)
             (encode-execute-message :portal-name (or portal-name "")
                                     :max-rows (or max-rows 0))
             (encode-sync-message)))))

(defun %normalize-query-exchange-parameters
    (parameters parameter-type-oids parameter-formats)
  (let* ((parameter-list (%parameter-sequence-list parameters :parameters))
         (parameter-count (length parameter-list))
         (parameter-type-oids (%parameter-oids-list parameter-type-oids
                                                    parameter-count))
         (parameter-formats (%parameter-formats-list parameter-formats
                                                      parameter-count)))
    (values parameter-list parameter-count parameter-type-oids parameter-formats)))

(defun %query-exchange-frames
    (connection sql parameters parameter-type-oids parameter-formats
     result-formats statement-name portal-name max-rows
     expected-prepared-statement)
  "Normalize one query request and build all of its frontend frames.

The complete frame list is built before the caller writes to the transport.
This keeps parameter encoding failures from leaving a partially-written
extended-query exchange on the socket."
  (let* ((extended-p
           (or parameters parameter-type-oids parameter-formats result-formats
               statement-name portal-name max-rows))
         (statement-name (%query-string-or-nil statement-name :statement-name))
         (portal-name (%query-string-or-nil portal-name :portal-name))
         (max-rows (%max-rows-value max-rows)))
    (multiple-value-bind (parameter-list parameter-count parameter-type-oids
                          parameter-formats)
        (%normalize-query-exchange-parameters
         parameters parameter-type-oids parameter-formats)
      (multiple-value-bind (prepared-statement effective-parameter-type-oids)
          (%resolve-prepared-statement connection statement-name sql
                                        parameter-count parameter-type-oids
                                        expected-prepared-statement)
        (if extended-p
            (%extended-query-exchange-frames
             sql effective-parameter-type-oids statement-name portal-name max-rows
             result-formats prepared-statement
             (%prepare-parameters connection parameter-list
                                   effective-parameter-type-oids parameter-formats))
            (list (encode-query-message sql)))))))
