(in-package #:cl-postgresql-kit)

(defun %oauthbearer-initial-response (connection)
  (unless (connection-tls-established-p connection)
    (error 'authentication-error
           :message "OAUTHBEARER authentication requires TLS."))
  (let ((provider (connection-oauth-token-provider connection)))
    (unless (functionp provider)
      (error 'authentication-error
             :message "OAUTHBEARER authentication requires a token provider."))
    (let ((token (funcall provider connection)))
      (unless (stringp token)
        (error 'authentication-error
               :message "OAUTHBEARER token provider must return a string."))
      (format nil "n,a=,~Cauth=Bearer ~A~C~C"
              (code-char 1) token (code-char 1) (code-char 1)))))

(defun %oauthbearer-handle-authentication-message (message)
  (let ((kind (backend-message-kind (backend-message-type message))))
    (unless (eq kind :authentication)
      (error 'authentication-error
             :message "Unexpected message during OAUTHBEARER authentication."))
    (let ((authentication
            (parse-authentication (backend-message-payload message))))
      (case (getf authentication :type)
        (:sasl-continue
         (unless (zerop (length (getf authentication :data)))
           (error 'authentication-error
                  :message "OAUTHBEARER server returned an unsupported challenge.")))
        (:sasl-final
         (unless (zerop (length (getf authentication :data)))
           (error 'authentication-error
                  :message "OAUTHBEARER server returned an error."))
         :done)
        (otherwise
         (error 'authentication-error
                :message "Unexpected authentication response for OAUTHBEARER."))))))

(defun %authenticate-oauthbearer (connection)
  (%send-frontend-message
   connection
   (encode-sasl-initial-response
    "OAUTHBEARER" (%oauthbearer-initial-response connection)))
  (loop
    for message = (%read-backend-message connection)
    do (when (eq (%oauthbearer-handle-authentication-message message) :done)
         (return nil))))
