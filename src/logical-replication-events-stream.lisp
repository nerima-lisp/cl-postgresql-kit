(in-package #:cl-postgresql-kit)

(defstruct (logical-replication-stream-event
             (:constructor %make-logical-replication-stream-event
                 (&key kind replication-message logical-message logical-event)))
  "One decoded event from a physical PostgreSQL replication stream.

KIND is the physical replication envelope kind.  XLOG-DATA records also have
LOGICAL-MESSAGE and LOGICAL-EVENT slots populated; other envelope kinds such
as PRIMARY-KEEPALIVE leave those slots NIL."
  kind
  replication-message
  logical-message
  logical-event)

(defmacro %logical-replication-stream-event
    ((kind replication-message)
     &key logical-message logical-event)
  `(%make-logical-replication-stream-event
    :kind ,kind
    :replication-message ,replication-message
    ,@(when logical-message
        `(:logical-message ,logical-message))
    ,@(when logical-event
        `(:logical-event ,logical-event))))

(defun %decode-logical-replication-xlog-data
    (decoder replication-message
     &key protocol-version streamed-p parallel-streaming-p)
  (let* ((logical-message
           (parse-logical-replication-message
            (replication-message-data replication-message)
            :protocol-version protocol-version
            :streamed-p streamed-p
            :parallel-streaming-p parallel-streaming-p))
         (logical-event
           (decode-logical-replication-message decoder logical-message)))
    (%logical-replication-stream-event
        (:xlog-data replication-message)
      :logical-message logical-message
      :logical-event logical-event)))

(defmacro %logical-replication-read-or-abort ((operation) &body body)
  `(handler-case
       (progn ,@body)
     (error (condition)
       (%copy-abort ,operation)
       (error condition))))

(defun decode-logical-replication-stream-message
    (decoder replication-message
     &key (protocol-version 1) streamed-p parallel-streaming-p)
  "Decode one physical replication envelope into a typed stream event.

For an XLogData envelope, parse its pgoutput payload using PROTOCOL-VERSION
and decode the resulting message through DECODER.  Keepalive and standby
feedback envelopes are returned as physical events without a logical payload.
The caller owns the COPY BOTH lifecycle; this function only decodes one
already-read envelope."
  (check-type decoder logical-replication-decoder)
  (check-type replication-message replication-message)
  (if (eq :xlog-data (replication-message-kind replication-message))
      (%decode-logical-replication-xlog-data
       decoder replication-message
       :protocol-version protocol-version
       :streamed-p streamed-p
       :parallel-streaming-p parallel-streaming-p)
      (%logical-replication-stream-event
          ((replication-message-kind replication-message)
           replication-message))))

(defun logical-replication-read
    (operation decoder
     &key (protocol-version 1) streamed-p parallel-streaming-p)
  "Read and decode the next event from a logical replication operation.

The operation must be a COPY BOTH handle returned by REPLICATION-START or
one of the logical replication start helpers.  Return NIL after the server's
CopyDone.  A malformed pgoutput message or a value-decoding error retires the
connection, matching REPLICATION-READ's error behavior."
  (check-type decoder logical-replication-decoder)
  (let ((replication-message (replication-read operation)))
    (when replication-message
      (%logical-replication-read-or-abort (operation)
        (decode-logical-replication-stream-message
         decoder replication-message
         :protocol-version protocol-version
         :streamed-p streamed-p
         :parallel-streaming-p parallel-streaming-p)))))
