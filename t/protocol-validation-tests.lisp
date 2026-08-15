(in-package #:cl-postgresql-kit/test)

(deftest protocol-counts-are-bounded-before-allocation
  (it-signals-each 'protocol-error
      ((:row-description)
       (:data-row)
       (:copy-response)
       (:parameter-description))
    "rejects oversized count field in ~A parser"
    (case-name)
    (case case-name
      (:row-description
       (parse-row-description (octets 255 255)))
      (:data-row
       (parse-data-row (octets 255 255)))
      (:copy-response
       (parse-copy-response (octets 0 255 255)))
      (:parameter-description
       (parse-parameter-description (octets 255 255))))))

(deftest protocol-format-code-validation
  (is (zerop (cl-postgresql-kit::%protocol-format-code 0)))
  (is (= 1 (cl-postgresql-kit::%protocol-format-code 1)))
  (it-signals-each 'parameter-error
      ((-1)
       (2)
       (nil)
       ("0"))
    "rejects invalid protocol format code ~S"
    (value)
    (cl-postgresql-kit::%protocol-format-code value))
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
