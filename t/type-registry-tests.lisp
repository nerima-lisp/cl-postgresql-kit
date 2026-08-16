(in-package #:cl-postgresql-kit/test)

(deftest dynamic-type-registry-loading
  (let* ((registry (make-type-registry))
         (responses
           (vector
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("enumlabel" 25 -1))
             '(("8000" "mood" "sad")
               ("8000" "mood" "happy")))
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("typbasetype" 20 8))
             '(("8001" "positive_int" "23")))
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("rngsubtype" 20 8))
             '(("8002" "int4range" "23")))
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("attnum" 23 4)
               ("attname" 25 -1) ("atttypid" 20 8))
             '(("8003" "number_label" "1" "number" "23")
               ("8003" "number_label" "2" "label" "25")))
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("typelem" 20 8))
             '(("8004" "mood[]" "8000")))))
         (query-index 0)
         (connection
           (ready-memory-connection
            :on-write
            (lambda (transport octets)
              (when (and (plusp (length octets))
                         (= (aref octets 0) (char-code #\Q)))
                (is (< query-index (length responses)))
                (memory-transport-append-input
                 transport (aref responses query-index))
                (incf query-index))))))
    (unwind-protect
         (progn
           (is (eq registry (load-type-registry connection
                                                :registry registry)))
           (is (= 5 query-index))
           (is (string= "happy"
                        (decode-value
                         registry 8000
                         (cl-codec-kit:string-to-octets "happy"
                                                        :encoding :utf-8))))
           (is (= 42
                  (decode-value registry 8001
                                (octets 0 0 0 42) :format 1)))
           (let ((value
                   (decode-value
                    registry 8002
                    (cl-codec-kit:string-to-octets "[1,10)"
                                                   :encoding :utf-8))))
             (is (postgres-range-p value))
             (is (= 1 (postgres-range-lower value)))
             (is (= 10 (postgres-range-upper value))))
           (let ((value
                   (decode-value
                    registry 8003
                    (cl-codec-kit:string-to-octets "(42,hello)"
                                                   :encoding :utf-8))))
             (is (postgres-composite-p value))
             (is (= 42 (aref (postgres-composite-fields value) 0)))
             (is (string= "hello"
                          (aref (postgres-composite-fields value) 1))))
           (let ((value
                   (decode-value
                    registry 8004
                    (cl-codec-kit:string-to-octets "{sad,happy}"
                                                   :encoding :utf-8))))
             (is (equalp #("sad" "happy")
                         (postgres-array-elements value)))))
      (disconnect connection))))

(deftest dynamic-type-registry-catalog-row-limit
  (let* ((response
           (catalog-query-input
            '(("oid" 20 8) ("typname" 25 -1) ("enumlabel" 25 -1))
            '(("8000" "mood" "sad")
              ("8000" "mood" "happy"))))
         (query-index 0)
         (connection
           (ready-memory-connection
            :on-write
            (lambda (transport octets)
              (when (and (plusp (length octets))
                         (= (aref octets 0) (char-code #\Q)))
                (is (= query-index 0))
                (memory-transport-append-input transport response)
                (incf query-index))))))
    (unwind-protect
         (progn
           (let ((cl-postgresql-kit::*maximum-type-registry-catalog-rows* 1))
             (assert-signals
              'query-error
              (lambda () (load-type-registry connection))))
           (is (= 1 query-index))
           (is (not (connection-open connection))))
      (disconnect connection))))

(deftest dynamic-multirange-type-registry-loading
  (let* ((registry (make-type-registry))
         (responses
           (vector
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("enumlabel" 25 -1))
             '(("8000" "mood" "sad")
               ("8000" "mood" "happy")))
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("typbasetype" 20 8))
             '(("8001" "positive_int" "23")))
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("rngsubtype" 20 8))
             '(("8002" "int4range" "23")))
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("rngsubtype" 20 8))
             '(("8011" "int4multirange" "23")))
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("attnum" 23 4)
               ("attname" 25 -1) ("atttypid" 20 8))
             '(("8003" "number_label" "1" "number" "23")
               ("8003" "number_label" "2" "label" "25")))
            (catalog-query-input
             '(("oid" 20 8) ("typname" 25 -1) ("typelem" 20 8))
             '(("8004" "mood[]" "8000")))))
         (query-index 0)
         (connection
           (ready-memory-connection
            :on-write
            (lambda (transport octets)
              (when (and (plusp (length octets))
                         (= (aref octets 0) (char-code #\Q)))
                (is (< query-index (length responses)))
                (memory-transport-append-input
                 transport (aref responses query-index))
                (incf query-index))))))
    (setf (gethash "server_version_num"
                   (connection-parameters connection))
          "140000")
    (unwind-protect
         (progn
           (is (eq registry (load-type-registry connection
                                                :registry registry)))
           (is (= 6 query-index))
           (let ((value
                   (decode-value
                    registry
                    8011
                    (cl-codec-kit:string-to-octets "{[1,10)}"
                                                   :encoding :utf-8))))
             (is (postgres-multirange-p value))
             (is (= 1 (length (postgres-multirange-ranges value))))
             (is (= 1 (postgres-range-lower
                       (aref (postgres-multirange-ranges value) 0))))
             (is (= 10 (postgres-range-upper
                        (aref (postgres-multirange-ranges value) 0))))))
      (disconnect connection))))

(deftest type-registry-catalog-validation
  (flet ((row (&rest values)
           (coerce values 'vector)))
    (is (eq :value
            (cl-postgresql-kit::%type-registry-catalog-value
             :value :field)))
    (assert-signals 'protocol-error
                    (lambda ()
                      (cl-postgresql-kit::%type-registry-catalog-value
                       +sql-null+ :field)))

    (is-each (value '(0 #xffffffff))
      (is (= value
             (cl-postgresql-kit::%type-registry-catalog-oid
              value :oid))))
    (it-signals-each 'protocol-error
        ((:negative-oid -1)
         (:oid-overflow #x100000000)
         (:non-integer-oid "23"))
      "type registry catalog rejects invalid OID case ~A"
      (label value)
      (declare (ignore label))
      (cl-postgresql-kit::%type-registry-catalog-oid value :oid))

    (is (= 0
           (cl-postgresql-kit::%type-registry-catalog-integer
            0 :integer)))
    (assert-signals
     'protocol-error
     (lambda ()
       (cl-postgresql-kit::%type-registry-catalog-integer
        "1" :integer)))

    (is (string= "name"
                 (cl-postgresql-kit::%type-registry-catalog-name
                  "name" :name)))
    (it-signals-each 'protocol-error
        ((:empty-name "")
         (:embedded-null
          #.(format nil "bad~Cname" #\Null))
         (:non-string-name 42)
         (:sql-null-name +sql-null+))
      "type registry catalog rejects invalid name case ~A"
      (label value)
      (declare (ignore label))
      (cl-postgresql-kit::%type-registry-catalog-name value :name))

    (let ((definitions
            (cl-postgresql-kit::%type-registry-enum-definitions
             (list (row 8000 "mood" "sad")
                   (row 8000 "mood" "happy")
                   (row 8001 "state" "ready")))))
      (is (= 2 (length definitions)))
      (is (= 8000 (getf (first definitions) :oid)))
      (is (string= "mood" (getf (first definitions) :name)))
      (is (equal '(
                 "sad" "happy")
                 (getf (first definitions) :labels))))
    (assert-signals
     'protocol-error
     (lambda ()
       (cl-postgresql-kit::%type-registry-enum-definitions
        (list (row 8000 "mood"))))
     )
    (assert-signals
     'protocol-error
     (lambda ()
       (cl-postgresql-kit::%type-registry-enum-definitions
        (list (row 8000 "mood" "sad")
              (row 8000 "state" "happy")))))

    (let ((definitions
            (cl-postgresql-kit::%type-registry-simple-definitions
             (list (row 8002 "int4range" 3904))
             :range)))
      (is (equal '(:oid 8002 :name "int4range" :base-oid 3904)
                 (first definitions))))
    (assert-signals
     'protocol-error
     (lambda ()
       (cl-postgresql-kit::%type-registry-simple-definitions
        (list (row 8002 "int4range"))
        :range)))

    (let ((definitions
            (cl-postgresql-kit::%type-registry-composite-definitions
             (list (row 8003 "number_label" 1 "number" 23)
                   (row 8003 "number_label" 2 "label" 25)))))
      (is (= 1 (length definitions)))
      (is (equalp #("number" "label")
                  (getf (first definitions) :field-names)))
      (is (equalp #(23 25)
                  (getf (first definitions) :field-oids))))
    (let ((definitions
            (cl-postgresql-kit::%type-registry-composite-definitions
             (list (row 8004 "empty" +sql-null+ +sql-null+ +sql-null+)))))
      (is (equalp #() (getf (first definitions) :field-names)))
      (is (equalp #() (getf (first definitions) :field-oids))))
    (it-signals-each 'protocol-error
        ((:truncated-row
          (list (row 8003 "composite" 1 "field")))
         (:mixed-type-name
          (list (row 8003 "composite" 1 "field" 23)
                (row 8003 "other" 2 "next" 25)))
         (:partial-empty-row
          (list (row 8003 "composite" +sql-null+ "field" +sql-null+)))
         (:zero-attnum
          (list (row 8003 "composite" 0 "field" 23)))
         (:non-integer-attnum
          (list (row 8003 "composite" "1" "field" 23)))
         (:empty-field-name
          (list (row 8003 "composite" 1 "" 23)))
         (:negative-field-oid
          (list (row 8003 "composite" 1 "field" -1)))
         (:empty-row-before-fields
          (list (row 8003 "composite"
                     +sql-null+ +sql-null+ +sql-null+)
                (row 8003 "composite" 1 "field" 23)))
         (:empty-row-after-fields
          (list (row 8003 "composite" 1 "field" 23)
                (row 8003 "composite"
                     +sql-null+ +sql-null+ +sql-null+)))
         (:attnum-regresses
          (list (row 8003 "composite" 2 "field" 23)
                (row 8003 "composite" 1 "next" 25)))
         (:duplicate-attnum
          (list (row 8003 "composite" 1 "field" 23)
                (row 8003 "composite" 1 "next" 25))))
      "type registry composite definitions reject invalid case ~A"
      (label value)
      (declare (ignore label))
      (cl-postgresql-kit::%type-registry-composite-definitions value))

    (let ((definitions
            (cl-postgresql-kit::%type-registry-array-definitions
             (list (row 8005 "mood[]" 8000)))))
      (is (equal '(:oid 8005 :name "mood[]" :element-oid 8000)
                 (first definitions))))
    (it-signals-each 'protocol-error
        ((:truncated-array-row
          (list (row 8005 "mood[]")))
         (:negative-element-oid
          (list (row 8005 "mood[]" -1))))
      "type registry array definitions reject invalid case ~A"
      (label value)
      (declare (ignore label))
      (cl-postgresql-kit::%type-registry-array-definitions value))))
