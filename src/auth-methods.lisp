(in-package #:cl-postgresql-kit)

(defun %authenticate-md5 (connection salt)
  (unless (connection-password connection)
    (error 'authentication-error :message "A password is required for MD5 authentication."))
  (%send-frontend-message
   connection
   (encode-password-message
    (%md5-password-response
     (connection-password connection)
     (connection-user connection)
     salt))))

(defun %authenticate-cleartext (connection)
  (unless (and (connection-tls-established-p connection)
               (member (connection-ssl-mode connection)
                       '(:verify-ca :verify-full)))
    (error 'authentication-error
           :message "Cleartext password authentication requires verified TLS."))
  (unless (connection-password connection)
    (error 'authentication-error
           :message "A password is required for cleartext authentication."))
  (%send-frontend-message
   connection
   (encode-password-message (connection-password connection))))

(defun %authenticate-gss-or-sspi (connection type data)
  (when (member type '(:gss :sspi))
    (setf (connection--authentication-method connection) type))
  (let* ((method (or (connection--authentication-method connection)
                     (if (eq type :sspi) :sspi :gss)))
         (provider (ecase method
                     (:gss (connection-gss-token-provider connection))
                     (:sspi (connection-sspi-token-provider connection)))))
    (unless (functionp provider)
      (error 'authentication-error
             :message (format nil
                              "~A authentication requires a token provider."
                              (string-upcase (symbol-name method)))))
    (let ((token (funcall provider connection type data)))
      (%send-frontend-message connection (encode-gss-response token)))))

(defun %handle-authentication (connection authentication)
  (case (getf authentication :type)
    (:ok nil)
    (:cleartext-password (%authenticate-cleartext connection))
    (:md5-password (%authenticate-md5 connection (getf authentication :salt)))
    ((:gss :gss-continue :sspi)
     (%authenticate-gss-or-sspi connection
                                (getf authentication :type)
                                (getf authentication :data)))
    (:sasl
     (let* ((mechanisms (getf authentication :mechanisms))
            (channel-binding-mode
              (connection-channel-binding connection))
            (channel-binding-data
              (transport-channel-binding-data
               (connection-transport connection)))
            (plus-offered-p
              (member "SCRAM-SHA-256-PLUS" mechanisms :test #'string=))
            (scram-offered-p
              (member "SCRAM-SHA-256" mechanisms :test #'string=))
            (oauth-offered-p
              (member "OAUTHBEARER" mechanisms :test #'string=)))
       (when (and plus-offered-p channel-binding-data
                  (not (%transport-octet-vector-p channel-binding-data)))
         (error 'authentication-error
                :message "Transport returned invalid SCRAM channel binding data."))
       (cond
         ((and (eq channel-binding-mode :require)
               (not (and plus-offered-p
                         (connection-tls-established-p connection)
                         channel-binding-data)))
          (error 'authentication-error
                 :message
                 "CHANNEL-BINDING=REQUIRE needs SCRAM-SHA-256-PLUS with TLS channel binding."))
         ((and (not (eq channel-binding-mode :disable))
               plus-offered-p (connection-tls-established-p connection)
               channel-binding-data)
          (%authenticate-scram connection
                               :mechanism "SCRAM-SHA-256-PLUS"
                               :channel-binding-data channel-binding-data))
         ((and (not (eq channel-binding-mode :require))
               scram-offered-p)
          (%authenticate-scram connection))
         ((and oauth-offered-p (connection-oauth-token-provider connection))
          (%authenticate-oauthbearer connection))
         (t
          (error 'authentication-error
                 :message "The server offers no supported SASL mechanism.")))))
    (otherwise
     (error 'authentication-error
            :message (format nil "Unsupported PostgreSQL authentication method: ~S"
                             (getf authentication :type))))))
