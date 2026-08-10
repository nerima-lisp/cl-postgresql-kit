(in-package #:cl-postgresql-kit)

(defun %tls-stream-options (tls-options)
  (loop for (key value) on tls-options by #'cddr
        unless (eq key :verify-location)
          append (list key value)))

(defmethod transport-start-tls ((transport socket-transport) &key hostname verify)
  "Upgrade an open socket transport with CL+SSL.

The optional TLS system keeps the core transport independent of CL+SSL while
preserving the transport API used by CONNECTION."
  (let ((stream (socket-transport-stream transport))
        (tls-options (socket-transport-tls-options transport)))
    (unless stream
      (error 'tls-error :message "The socket transport is not open."))
    (handler-case
        (let ((verify-location (getf tls-options :verify-location)))
          (setf (socket-transport-stream transport)
                (if verify-location
                    (let ((context
                            (cl+ssl:make-context
                             :verify-location verify-location
                             :verify-mode (if verify
                                              cl+ssl:+ssl-verify-peer+
                                              cl+ssl:+ssl-verify-none+))))
                      (cl+ssl:with-global-context (context :auto-free-p t)
                        (apply #'cl+ssl:make-ssl-client-stream
                               stream
                               :hostname hostname
                               :verify (if verify :required :optional)
                               :external-format nil
                               (%tls-stream-options tls-options))))
                    (apply #'cl+ssl:make-ssl-client-stream
                           stream
                           :hostname hostname
                           :verify (if verify :required :optional)
                           :external-format nil
                           tls-options))))
      (error (condition)
        (ignore-errors (transport-close transport))
        (error 'tls-error
               :cause condition
               :message "Unable to establish TLS with the PostgreSQL server.")))
    transport))
