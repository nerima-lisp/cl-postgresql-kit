(in-package #:cl-postgresql-kit/test)

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
    (is-case-each ((value predicate)
                   `((,builder ,#'octet-builder-p)
                     (,message ,#'backend-message-p)
                     (,column ,#'column-p)
                     (,registry ,#'type-registry-p)
                     (,typed ,#'typed-value-p)
                     (,json ,#'json-value-p)
                     (,bytea ,#'bytea-value-p)
                     (,date ,#'date-value-p)
                     (,time ,#'time-value-p)
                     (,timestamp ,#'timestamp-value-p)
                     (,timestamptz ,#'timestamptz-value-p)
                     (,interval ,#'interval-value-p)
                     (,result ,#'query-result-p)
                     (,statement ,#'prepared-statement-p)))
      (is (funcall predicate value)))))

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
