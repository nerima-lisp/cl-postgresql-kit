(in-package #:cl-postgresql-kit/test)

(deftest frame-round-trip
  (let* ((payload (cl-codec-kit:string-to-octets "select 1" :encoding :utf-8))
         (frame (make-frame #\Q payload))
         (message (parse-frame frame)))
    (is (= (backend-message-type message) (char-code #\Q)))
    (is (equalp (backend-message-payload message) payload))))
