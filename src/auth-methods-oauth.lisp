(in-package #:cl-postgresql-kit)

(defun %oauthbearer-token-response (token)
  (format nil "n,a=,~Cauth=Bearer ~A~C~C"
          (code-char 1) token (code-char 1) (code-char 1)))

(defun %oauthbearer-initial-response (connection)
  (unless (connection-tls-established-p connection)
    (error 'authentication-error
           :message "OAUTHBEARER authentication requires TLS."))
  (setf (connection--oauth-discovery-active-p connection) nil
        (connection--oauth-discovery-response connection) nil)
  (cond
    ((stringp (connection--oauth-discovery-token connection))
     (%oauthbearer-token-response
      (connection--oauth-discovery-token connection)))
    ((functionp (connection-oauth-token-provider connection))
     (let ((token (funcall (connection-oauth-token-provider connection)
                           connection)))
       (unless (stringp token)
         (error 'authentication-error
                :message "OAUTHBEARER token provider must return a string."))
       (%oauthbearer-token-response token)))
    ((functionp (connection-oauth-discovery-provider connection))
     (setf (connection--oauth-discovery-active-p connection) t)
     (format nil "n,,~Cauth=~C~C"
             (code-char 1) (code-char 1) (code-char 1)))
    (t
     (error 'authentication-error
            :message "OAUTHBEARER authentication requires a token provider or OAuth discovery provider."))))

(defun %oauthbearer-handle-authentication-message (connection message)
  (let ((kind (backend-message-kind (backend-message-type message))))
    (unless (eq kind :authentication)
      (error 'authentication-error
             :message "Unexpected message during OAUTHBEARER authentication."))
    (let ((authentication
            (parse-authentication (backend-message-payload message))))
      (case (getf authentication :type)
        (:sasl-continue
         (if (connection--oauth-discovery-active-p connection)
             (let ((response (getf authentication :data)))
               (unless (and (stringp response) (plusp (length response)))
                 (error 'authentication-error
                        :message "OAUTHBEARER discovery response is empty."))
               (setf (connection--oauth-discovery-response connection) response)
               (%send-frontend-message
                connection
                (encode-sasl-response (string (code-char 1))))
               :discovery-response)
             (unless (zerop (length (getf authentication :data)))
               (error 'authentication-error
                      :message "OAUTHBEARER server returned an unsupported challenge."))))
        (:sasl-final
         (when (connection--oauth-discovery-active-p connection)
           (error 'authentication-error
                  :message "OAUTHBEARER discovery ended without an error response."))
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
    for kind = (backend-message-kind (backend-message-type message))
    do (cond
         ((eq kind :error-response)
          (let ((fields (parse-error-response
                         (backend-message-payload message))))
            (if (connection--oauth-discovery-active-p connection)
                (let ((response
                        (connection--oauth-discovery-response connection)))
                  (unless (stringp response)
                    (%signal-server-fields fields))
                  (error 'oauth-discovery-required
                         :message (or (%field fields :message)
                                      "OAuth discovery requires a token.")
                         :response response
                         :server-fields fields))
                (%signal-server-fields fields))))
         ((eq (%oauthbearer-handle-authentication-message connection message)
              :done)
          (return nil)))))
