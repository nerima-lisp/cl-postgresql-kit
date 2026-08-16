(in-package #:cl-postgresql-kit)

(defun %tls-stream-options (tls-options)
  (loop for (key value) on tls-options by #'cddr
        unless (member key '(:verify-location :min-proto-version) :test #'eq)
          append (list key value)))

(defun %tls-protocol-version (version)
  (ecase version
    (:tlsv1 #x0301)
    (:tlsv1-1 #x0302)
    (:tlsv1-2 #x0303)
    (:tlsv1-3 #x0304)))

(defmethod transport-start-tls ((transport socket-transport) &key hostname verify)
  "Upgrade an open socket transport with CL+SSL.

The optional TLS system keeps the core transport independent of CL+SSL while
preserving the transport API used by CONNECTION."
  (let ((stream (socket-transport-stream transport))
        (tls-options (socket-transport-tls-options transport)))
    (unless stream
      (error 'tls-error :message "The socket transport is not open."))
    (handler-case
        (let ((verify-location (getf tls-options :verify-location))
              (min-proto-version (getf tls-options :min-proto-version)))
          (setf (socket-transport-stream transport)
                (if (or verify-location min-proto-version)
                    (let ((context
                            (apply #'cl+ssl:make-context
                                   (append
                                    (when verify-location
                                      (list :verify-location verify-location))
                                    (when min-proto-version
                                      (list :min-proto-version
                                            (%tls-protocol-version
                                             min-proto-version)))
                                    (list :verify-mode
                                          (if verify
                                              cl+ssl:+ssl-verify-peer+
                                              cl+ssl:+ssl-verify-none+))))))
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
                           (%tls-stream-options tls-options)))))
      (error (condition)
        (ignore-errors (transport-close transport))
        (error 'tls-error
               :cause condition
               :message "Unable to establish TLS with the PostgreSQL server.")))
    transport))
