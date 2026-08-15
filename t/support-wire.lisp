(in-package #:cl-postgresql-kit/test)

(defun octets (&rest values)
  (make-array (length values)
              :element-type '(unsigned-byte 8)
              :initial-contents values))

(defun octets-as-hex (value)
  (with-output-to-string (stream)
    (loop for octet across value
          do (format stream "~2,'0X" octet))))

(defun cstring (value)
  (let* ((payload (cl-codec-kit:string-to-octets value :encoding :utf-8))
         (result (make-array (1+ (length payload))
                             :element-type '(unsigned-byte 8))))
    (replace result payload)
    (setf (aref result (length payload)) 0)
    result))

(defun join-octets (&rest sequences)
  (apply #'concatenate '(vector (unsigned-byte 8)) sequences))

(defun binary-array-wire (dimensions-count has-null element-oid
                          dimensions lower-bounds &optional (body #()))
  (let ((builder (make-octet-builder)))
    (append-i32 builder dimensions-count)
    (append-i32 builder has-null)
    (append-i32 builder element-oid)
    (dolist (dimension dimensions)
      (append-i32 builder dimension))
    (dolist (lower-bound lower-bounds)
      (append-i32 builder lower-bound))
    (append-octets builder body)
    (builder-octets builder)))

(defun binary-i16-vector-wire (element-oid values
                               &key (lower-bound 0) (has-null 0))
  (let ((body-builder (make-octet-builder)))
    (loop for value across values
          do (append-i32 body-builder 2)
             (append-i16 body-builder value))
    (binary-array-wire 1 has-null element-oid
                       (list (length values))
                       (list lower-bound)
                       (builder-octets body-builder))))

(defun binary-u32-vector-wire (element-oid values
                               &key (lower-bound 0) (has-null 0))
  (let ((body-builder (make-octet-builder)))
    (loop for value across values
          do (append-i32 body-builder 4)
             (append-u32 body-builder value))
    (binary-array-wire 1 has-null element-oid
                       (list (length values))
                       (list lower-bound)
                       (builder-octets body-builder))))

(defun memory-transport-frame-types (transport)
  (loop with wire = (memory-transport-output transport)
        with position = 0
        while (< position (length wire))
        for frame-length =
          (+ 1 (logior (ash (aref wire (+ position 1)) 24)
                       (ash (aref wire (+ position 2)) 16)
                       (ash (aref wire (+ position 3)) 8)
                       (aref wire (+ position 4))))
        for frame = (parse-frame (subseq wire position (+ position frame-length)))
        collect (code-char (backend-message-type frame))
        do (incf position frame-length)))

(defun copy-response-payload ()
  (octets 0 0 1 0 0))

(defun append-ready-command (transport tag)
  (memory-transport-append-input
   transport
   (join-octets (make-frame #\C (cstring tag))
                (make-frame #\Z (octets (char-code #\I))))))

(defun append-startup-parameter (transport name value)
  (memory-transport-append-input
   transport
   (make-frame #\S (join-octets (cstring name) (cstring value))))
  (append-ready-command transport "STARTUP"))

(defun make-target-session-transport (read-only-p)
  (make-memory-transport
   :on-write (lambda (transport octets)
               (declare (ignore octets))
               (append-startup-parameter
                transport
                "transaction_read_only"
                (if read-only-p "on" "off")))))

(defun large-object-scalar-result-input (type-oid value tag)
  (let ((description (make-octet-builder))
        (row (make-octet-builder)))
    (append-u16 description 1)
    (append-cstring description "value")
    (append-u32 description 0)
    (append-i16 description 0)
    (append-u32 description type-oid)
    (append-i16 description (if (= type-oid 17) -1 4))
    (append-i32 description -1)
    (append-i16 description 1)
    (append-u16 row 1)
    (append-i32 row (length value))
    (append-octets row value)
    (join-octets
     (make-frame #\1 #())
     (make-frame #\2 #())
     (make-frame #\T (builder-octets description))
     (make-frame #\D (builder-octets row))
     (make-frame #\C (cstring tag))
     (make-frame #\Z (octets (char-code #\T))))))

(defun catalog-query-input (columns rows &optional (tag "SELECT 1"))
  (labels ((wire-value (value)
             (cond ((stringp value)
                    (cl-codec-kit:string-to-octets value :encoding :utf-8))
                   ((integerp value)
                    (cl-codec-kit:string-to-octets (format nil "~D" value)
                                                   :encoding :utf-8))
                   ((and (vectorp value)
                         (equalp (array-element-type value)
                                 '(unsigned-byte 8)))
                    value)
                   (t
                    (error "Unsupported catalog test value: ~S" value))))
           (row-frame (row)
             (let ((payload (make-octet-builder)))
               (append-u16 payload (length columns))
               (dolist (value row)
                 (if (null value)
                     (append-i32 payload -1)
                     (let ((wire (wire-value value)))
                       (append-i32 payload (length wire))
                       (append-octets payload wire))))
               (make-frame #\D (builder-octets payload)))))
    (let ((description (make-octet-builder)))
      (append-u16 description (length columns))
      (dolist (column columns)
        (destructuring-bind (name type-oid type-size) column
          (append-cstring description name)
          (append-u32 description 0)
          (append-i16 description 0)
          (append-u32 description type-oid)
          (append-i16 description type-size)
          (append-i32 description -1)
          (append-i16 description 0)))
      (apply #'join-octets
             (append (list (make-frame #\T (builder-octets description)))
                     (mapcar #'row-frame rows)
                     (list (make-frame #\C (cstring tag))
                           (make-frame #\Z (octets (char-code #\I)))))))))
