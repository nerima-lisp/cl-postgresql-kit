(in-package #:cl-postgresql-kit)

(defun %pipeline-request-frames (connection request)
  "Normalize REQUEST and build its frontend protocol frames.

The returned frame list is in wire order.  The caller may therefore combine
several requests without exposing a partially-built pipeline to the
transport."
  (let* ((parameter-list (pipeline-request-parameters request))
         (parameter-count (length parameter-list))
         (parameter-type-oids
           (pipeline-request-parameter-type-oids request))
         (parameter-formats
           (pipeline-request-parameter-formats request))
         (statement-name
           (or (pipeline-request-statement-name request)
               (format nil "pg_kit_pipeline_stmt_~D"
                       (incf *statement-counter*))))
         (portal-name
           (or (pipeline-request-portal-name request)
               (format nil "pg_kit_pipeline_portal_~D"
                       (incf *statement-counter*))))
         (prepared-statement nil)
         (effective-parameter-type-oids parameter-type-oids))
    (multiple-value-setq (prepared-statement effective-parameter-type-oids)
      (%resolve-prepared-statement
       connection
       (pipeline-request-statement-name request)
       (pipeline-request-sql request)
       parameter-count
       parameter-type-oids))
    (let* ((prepared-parameters
             (%prepare-parameters connection parameter-list
                                   effective-parameter-type-oids
                                   parameter-formats))
           (entry
             (%make-pipeline-entry
              :request request
              :statement-name statement-name
              :portal-name portal-name
              :close-statement-p (not prepared-statement)
              :columns #()))
           (frames nil))
      (unless prepared-statement
        (push (encode-parse-message
                (pipeline-request-sql request)
                :statement-name statement-name
                :parameter-type-oids
                (or effective-parameter-type-oids
                    (mapcar (lambda (parameter) (getf parameter :oid))
                            prepared-parameters)))
              frames))
      (push (encode-bind-message
              (mapcar (lambda (parameter) (getf parameter :value))
                      prepared-parameters)
              :portal-name portal-name
              :statement-name statement-name
              :parameter-formats
              (mapcar (lambda (parameter) (getf parameter :format))
                      prepared-parameters)
              :result-formats (pipeline-request-result-formats request))
            frames)
      (push (encode-describe-message portal-name :kind :portal) frames)
      (push (encode-execute-message :portal-name portal-name :max-rows 0)
            frames)
      (push (encode-close-message portal-name :kind :portal) frames)
      (unless prepared-statement
        (push (encode-close-message statement-name :kind :statement) frames))
      (push (encode-sync-message) frames)
      (values entry (nreverse frames)))))
