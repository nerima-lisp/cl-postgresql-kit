(in-package #:cl-postgresql-kit/test)

(deftest copy-in-and-out
  (let ((copy-in-input
          (join-octets
           (make-frame #\G (copy-response-payload))
           (make-frame #\C (cstring "COPY 1"))
           (make-frame #\Z (octets (char-code #\I))))))
    (with-ready-memory-test-connection (connection :input copy-in-input)
      (let ((operation (copy-in connection "copy target from stdin")))
        (assert-signals 'copy-error
                        (lambda () (query connection "select 1")))
        (is (copy-in-write operation "one-row"))
        (is (string= (copy-in-finish operation) "COPY 1"))
        (is (equalp
             (memory-transport-output (connection-transport connection))
             (join-octets
              (encode-query-message "copy target from stdin")
              (encode-copy-data-message "one-row")
              (encode-copy-done-message)))))))
  (let ((copy-out-input
          (join-octets
           (make-frame #\H (copy-response-payload))
           (make-frame #\d (octets 102 111 111))
           (make-frame #\c #())
           (make-frame #\C (cstring "COPY 1"))
           (make-frame #\Z (octets (char-code #\I))))))
    (with-ready-memory-test-connection (connection :input copy-out-input)
      (let ((operation (copy-out connection "copy source to stdout")))
        (is (equalp (copy-out-read operation) (octets 102 111 111)))
        (is (null (copy-out-read operation)))
        (is (equalp
             (memory-transport-output (connection-transport connection))
             (encode-query-message "copy source to stdout")))))))

(deftest copy-in-abort-keeps-connection-ready
  (let* ((error-payload
           (join-octets
            (octets (char-code #\S)) (cstring "ERROR")
            (octets (char-code #\C)) (cstring "57014")
            (octets (char-code #\M)) (cstring "COPY cancelled")
            (octets 0)))
         (input
           (join-octets
           (make-frame #\G (copy-response-payload))
           (make-frame #\E error-payload)
           (make-frame #\Z (octets (char-code #\I))))))
    (with-ready-memory-test-connection (connection :input input)
      (let ((operation (copy-in connection "copy target from stdin")))
        (is (copy-in-abort operation "stop now"))
        (is (connection-open connection))
        (is (eq (connection-state connection) :ready))
        (is (equalp
             (memory-transport-output (connection-transport connection))
             (join-octets
              (encode-query-message "copy target from stdin")
              (encode-copy-fail-message "stop now"))))
        (assert-signals 'copy-error
                        (lambda () (copy-in-finish operation)))))))

(deftest copy-both-round-trip
  (let ((copy-both-input
          (join-octets
           (make-frame #\W (copy-response-payload))
           (make-frame #\d (octets 115 101 114 118 101 114 45 114 111 119))
           (make-frame #\c #())
           (make-frame #\C (cstring "COPY 1"))
           (make-frame #\Z (octets (char-code #\I))))))
    (with-ready-memory-test-connection (connection :input copy-both-input)
      (let ((operation (copy-both-start connection "copy both target")))
        (is (copy-both-write operation "client-row"))
        (is (equalp (copy-both-read operation)
                    (octets 115 101 114 118 101 114 45 114 111 119)))
        (is (null (copy-both-read operation)))
        (is (string= (copy-both-finish operation) "COPY 1"))
        (is (equalp
             (memory-transport-output (connection-transport connection))
             (join-octets
              (encode-query-message "copy both target")
              (encode-copy-data-message "client-row")
              (encode-copy-done-message)))))))

(deftest copy-codec-session-helpers
  (let ((input
          (join-octets
           (make-frame #\G (copy-response-payload))
           (make-frame #\C (cstring "COPY 1"))
           (make-frame #\Z (octets (char-code #\I))))))
    (with-ready-memory-test-connection (connection :input input)
      (let ((operation (copy-in-start connection "copy target from stdin")))
        (is (copy-in-write-row operation '(7 "hello") '(23 25)))
        (is (copy-in-write-binary-stream operation '((8) (9)) '(23)))
        (is (string= (copy-in-finish operation) "COPY 1"))
        (is (equalp
             (memory-transport-output (connection-transport connection))
             (join-octets
              (encode-query-message "copy target from stdin")
              (encode-copy-data-message
               (encode-copy-row '(7 "hello") '(23 25)))
              (encode-copy-data-message
               (encode-copy-binary-stream '((8) (9)) '(23)))
              (encode-copy-done-message)))))))
  (let* ((stream (encode-copy-binary-stream '((1) (2)) '(23)))
         (split (floor (length stream) 2))
         (input
           (join-octets
            (make-frame #\H (copy-response-payload))
            (make-frame #\d (subseq stream 0 split))
            (make-frame #\d (subseq stream split))
            (make-frame #\c #())
            (make-frame #\C (cstring "COPY 2"))
            (make-frame #\Z (octets (char-code #\I))))))
    (with-ready-memory-test-connection (connection :input input)
      (let ((operation (copy-out-start connection "copy source to stdout")))
        (multiple-value-bind (rows flags extension)
            (decode-copy-binary-stream
             (copy-out-read-all operation) '(23))
          (is (equal rows '((1) (2))))
          (is (= flags 0))
          (is (zerop (length extension)))))))
  (let* ((stream (encode-copy-binary-stream '((11) (12)) '(23)))
         (input
           (join-octets
           (make-frame #\W (copy-response-payload))
           (make-frame #\d stream)
           (make-frame #\c #())
           (make-frame #\C (cstring "COPY 2"))
           (make-frame #\Z (octets (char-code #\I))))))
    (with-ready-memory-test-connection (connection :input input)
      (let ((operation (copy-both-start connection "copy both target")))
        (is (copy-both-write-row operation '(10) '(23)))
        (is (equalp (copy-both-read-all operation) stream))
        (is (string= (copy-both-finish operation) "COPY 2"))
        (is (equalp
             (memory-transport-output (connection-transport connection))
             (join-octets
             (encode-query-message "copy both target")
             (encode-copy-data-message
              (encode-copy-row '(10) '(23)))
             (encode-copy-done-message)))))))))

(deftest copy-text-stream-codec-session-helpers
  (let* ((rows
           (list (list 7 (format nil "hello~Cworld" #\Newline))
                 (list 8 (format nil "back~Cslash~Cvalue" #\\ #\Tab))))
         (stream (encode-copy-text-stream rows '(23 25))))
    (is (equal rows (decode-copy-text-stream stream '(23 25))))
    (assert-signals 'protocol-error
                    (lambda ()
                      (decode-copy-text-stream
                       (subseq stream 0 (1- (length stream)))
                       '(23 25)))))
  (let* ((rows
           (list (list 7 (format nil "hello~Cworld" #\Newline))
                 (list 8 (format nil "back~Cslash~Cvalue" #\\ #\Tab))))
         (stream (encode-copy-text-stream rows '(23 25)))
         (input
          (join-octets
           (make-frame #\G (copy-response-payload))
           (make-frame #\C (cstring "COPY 2"))
           (make-frame #\Z (octets (char-code #\I))))))
    (with-ready-memory-test-connection (connection :input input)
      (let ((operation (copy-in-start connection "copy target from stdin")))
        (is (copy-in-write-text-stream operation rows '(23 25)))
        (is (string= (copy-in-finish operation) "COPY 2"))
        (is (equalp
             (memory-transport-output (connection-transport connection))
             (join-octets
              (encode-query-message "copy target from stdin")
              (encode-copy-data-message stream)
              (encode-copy-done-message)))))))
  (let* ((rows
           (list (list 1 (format nil "one~Cline" #\Newline))
                 (list 2 (format nil "two~Cslash" #\\))))
         (stream (encode-copy-text-stream rows '(23 25)))
         (split (floor (length stream) 2))
         (input
           (join-octets
            (make-frame #\H (copy-response-payload))
            (make-frame #\d (subseq stream 0 split))
            (make-frame #\d (subseq stream split))
            (make-frame #\c #())
            (make-frame #\C (cstring "COPY 2"))
            (make-frame #\Z (octets (char-code #\I))))))
    (with-ready-memory-test-connection (connection :input input)
      (let ((operation (copy-out-start connection "copy source to stdout")))
        (is (equal rows (copy-out-read-rows operation '(23 25)))))))
  (let* ((server-rows
           (list (list 11 (format nil "server~Cline" #\Newline))
                 (list 12 (format nil "server~Cslash" #\\))))
         (client-rows
           (list (list 10 (format nil "client~Cline" #\Newline))))
         (server-stream (encode-copy-text-stream server-rows '(23 25)))
         (client-stream (encode-copy-text-stream client-rows '(23 25)))
         (input
           (join-octets
           (make-frame #\W (copy-response-payload))
           (make-frame #\d server-stream)
           (make-frame #\c #())
           (make-frame #\C (cstring "COPY 2"))
           (make-frame #\Z (octets (char-code #\I))))))
    (with-ready-memory-test-connection (connection :input input)
      (let ((operation (copy-both-start connection "copy both target")))
        (is (copy-both-write-text-stream operation client-rows '(23 25)))
        (is (equal server-rows
                   (copy-both-read-rows operation '(23 25))))
        (is (string= (copy-both-finish operation) "COPY 2"))
        (is (equalp
             (memory-transport-output (connection-transport connection))
             (join-octets
              (encode-query-message "copy both target")
              (encode-copy-data-message client-stream)
              (encode-copy-done-message))))))))

(deftest copy-formats-are-validated
  (it-signals-each 'protocol-error
      ((:empty #())
       (:invalid-format-code #(2 0 0))
       (:truncated-format-vector #(0 0 1 0 2)))
    "copy response parser rejects malformed payload case ~A"
    (label payload)
    (declare (ignore label))
    (parse-copy-response payload)))
