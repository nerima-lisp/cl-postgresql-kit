(in-package #:cl-postgresql-kit)

(defun result-row (result index)
  (aref (query-result-rows result) index))

(defun %result-column-index (result column)
  (if (integerp column)
      column
      (or (position column (query-result-columns result)
                    :key #'column-name :test #'string-equal)
          (error 'parameter-error :parameter column
                 :message "Unknown result column."))))

(defun result-column (result column)
  (let ((index (%result-column-index result column)))
    (coerce (loop for row across (query-result-rows result)
                  collect (aref row index)) 'vector)))

(defun result-row-alist (result index)
  (let ((row (result-row result index)))
    (loop for column across (query-result-columns result)
          for value across row
          collect (cons (column-name column) value))))

(defun result-rows-as-alists (result)
  (loop for index below (query-result-row-count result)
        collect (result-row-alist result index)))

(defun result-columns (result)
  (query-result-columns result))

(defun result-rows (result)
  (query-result-rows result))

(defun result-command-tag (result)
  (query-result-command-tag result))

(defun result-row-count (result)
  (query-result-row-count result))

(defun result-transaction-status (result)
  (query-result-transaction-status result))

(defun result-notices (result)
  (query-result-notices result))

(defun result-portal-suspended-p (result)
  (query-result-portal-suspended-p result))

(defun result-row-alists (result)
  (result-rows-as-alists result))

(defun row-value (result row-index column)
  "Return one value from RESULT's ROW-INDEX and COLUMN.

COLUMN may be a zero-based integer or a column name string."
  (aref (result-row result row-index)
        (%result-column-index result column)))

(defun %decode-query-row (connection columns raw-row)
  (unless (= (length columns) (length raw-row))
    (error 'protocol-error
           :message "PostgreSQL DataRow column count does not match RowDescription."
           :expected (length columns)
           :actual (length raw-row)))
  (let ((decoded (make-array (length raw-row))))
    (loop for index below (length raw-row)
          for column = (and (< index (length columns)) (aref columns index))
          for cell = (aref raw-row index)
          do (setf (aref decoded index)
                   (if (sql-null-p cell)
                       +sql-null+
                       (decode-value (connection-type-registry connection)
                                     (column-type-oid column)
                                     cell
                                     :format (column-format-code column)))))
    decoded))
