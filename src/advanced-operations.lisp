(in-package #:cl-postgresql-kit)

(defun query-pipeline (connection requests &key max-result-rows max-result-bytes)
  "Execute REQUESTS in one PostgreSQL extended-query pipeline.

REQUESTS is a non-empty sequence of SQL strings or PIPELINE-REQUEST objects.
The return value is a list containing one QUERY-RESULT for each request in
input order.  Each request is terminated independently at a ReadyForQuery
boundary, while the frontend messages are flushed as one batch."
  (%query-pipeline connection requests
                   :max-result-rows max-result-rows
                   :max-result-bytes max-result-bytes))

(defun query-pipeline-async (connection requests &key max-result-rows max-result-bytes)
  "Execute QUERY-PIPELINE asynchronously using CL-CONCURRENT-KIT."
  (cl-concurrent-kit:future
    (%query-pipeline connection requests
                     :max-result-rows max-result-rows
                     :max-result-bytes max-result-bytes)))

(defun query-async (connection sql &rest arguments)
  "Execute QUERY asynchronously using CL-CONCURRENT-KIT's future API."
  (cl-concurrent-kit:future
    (apply #'query connection sql arguments)))

(defun query-cps (connection sql on-success
                 &key on-error parameters parameter-type-oids parameter-formats
                   result-formats statement-name portal-name max-rows
                   max-result-rows max-result-bytes)
  "Execute QUERY and compose its Promise with success and error continuations.

ON-SUCCESS receives the QUERY-RESULT.  When supplied, ON-ERROR receives the
condition signaled by QUERY.  The returned Promise represents the value
returned by the selected continuation."
  (%query-cps
   (query-async connection sql
                :parameters parameters
                :parameter-type-oids parameter-type-oids
                :parameter-formats parameter-formats
                :result-formats result-formats
                :statement-name statement-name
                :portal-name portal-name
                :max-rows max-rows
                :max-result-rows max-result-rows
                :max-result-bytes max-result-bytes)
   on-success
   on-error))

(defun query-pipeline-cps (connection requests on-success
                           &key on-error max-result-rows max-result-bytes)
  "Execute QUERY-PIPELINE and compose its Promise with continuations.

ON-SUCCESS receives the list of QUERY-RESULT objects.  When supplied,
ON-ERROR receives a condition.  The returned Promise represents the value
returned by the selected continuation."
  (%query-cps
   (query-pipeline-async connection requests
                         :max-result-rows max-result-rows
                         :max-result-bytes max-result-bytes)
   on-success
   on-error))
