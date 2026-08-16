(in-package #:cl-postgresql-kit)

(defun %require-authentication-method-allowed-p (connection method)
  (let ((policy (connection-require-auth connection)))
    (or (null policy)
        (let ((methods (getf policy :methods)))
          (if (eq (getf policy :mode) :allow)
              (member method methods :test #'eq)
              (not (member method methods :test #'eq)))))))

(defun %note-authentication-method (connection method)
  (unless (%require-authentication-method-allowed-p connection method)
    (error 'authentication-error
           :message
           (format nil
                   "Server authentication method ~A does not satisfy REQUIRE_AUTH policy ~S."
                   method
                   (connection-require-auth connection))))
  (setf (connection--authentication-requested-p connection) t
        (connection--authentication-observed-method connection) method)
  method)

(defun %require-authentication-satisfied-p (connection)
  (let ((policy (connection-require-auth connection)))
    (or (null policy)
        (let* ((mode (getf policy :mode))
               (methods (getf policy :methods))
               (observed (connection--authentication-observed-method connection))
               (gss-established-p (connection-gss-established-p connection))
               (no-auth-p (not (connection--authentication-requested-p connection)))
               (positive-match-p
                 (or (and no-auth-p
                          (member :none methods :test #'eq))
                     (and observed
                          (member observed methods :test #'eq))
                     (and gss-established-p
                          (member :gss methods :test #'eq))))
               (negative-match-p
                 (or (and observed
                          (member observed methods :test #'eq))
                     (and gss-established-p
                          (member :gss methods :test #'eq))
                     (and no-auth-p
                          (not gss-established-p)
                          (member :none methods :test #'eq)))))
          (unless (if (eq mode :allow)
                      positive-match-p
                      (not negative-match-p))
            (error 'authentication-error
                   :message
                   (format nil
                           "Server authentication did not satisfy REQUIRE_AUTH policy ~S."
                           policy)))
          t))))

(defun %authenticate-md5 (connection salt)
  (%note-authentication-method connection :md5)
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
  (%note-authentication-method connection :password)
  (unless (connection-tls-established-p connection)
    (error 'authentication-error
           :message "Cleartext password authentication requires encrypted TLS."))
  (unless (connection-password connection)
    (error 'authentication-error
           :message "A password is required for cleartext authentication."))
  (%send-frontend-message
   connection
   (encode-password-message (connection-password connection))))

(defun %authenticate-gss-or-sspi (connection type data)
  (when (member type '(:gss :kerberos-v5 :sspi))
    (setf (connection--authentication-method connection) type)
    (%note-authentication-method
     connection
     (if (eq type :sspi) :sspi :gss)))
  (let* ((method (or (connection--authentication-method connection)
                     (if (eq type :sspi) :sspi :gss)))
         (method-name (if (eq method :sspi) :sspi :gss))
         (provider (ecase method
                     ((:gss :kerberos-v5) (connection-gss-token-provider connection))
                     (:sspi (connection-sspi-token-provider connection)))))
    (unless (connection--authentication-requested-p connection)
      (%note-authentication-method connection method-name))
    (unless (functionp provider)
      (error 'authentication-error
             :message (format nil
                              "~A authentication requires a token provider."
                              (string-upcase (symbol-name method)))))
    (let ((token (funcall provider connection type data)))
      (%send-frontend-message connection (encode-gss-response token)))))

(defun %handle-authentication (connection authentication)
  (case (getf authentication :type)
    (:ok (%require-authentication-satisfied-p connection))
    (:cleartext-password (%authenticate-cleartext connection))
    (:md5-password (%authenticate-md5 connection (getf authentication :salt)))
    ((:kerberos-v5 :gss :gss-continue :sspi)
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
              (member "OAUTHBEARER" mechanisms :test #'string=))
            (scram-allowed-p
              (%require-authentication-method-allowed-p
               connection :scram-sha-256))
            (oauth-allowed-p
              (%require-authentication-method-allowed-p connection :oauth)))
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
               plus-offered-p scram-allowed-p
               (connection-tls-established-p connection)
               channel-binding-data)
          (%note-authentication-method connection :scram-sha-256)
          (%authenticate-scram connection
                               :mechanism "SCRAM-SHA-256-PLUS"
                               :channel-binding-data channel-binding-data))
         ((and (not (eq channel-binding-mode :require))
               scram-offered-p scram-allowed-p)
          (%note-authentication-method connection :scram-sha-256)
          (%authenticate-scram connection))
         ((and oauth-offered-p oauth-allowed-p
               (or (connection-oauth-token-provider connection)
                   (connection-oauth-discovery-provider connection)))
          (%note-authentication-method connection :oauth)
          (%authenticate-oauthbearer connection))
         (t
          (error 'authentication-error
                 :message "The server offers no supported SASL mechanism.")))))
    (otherwise
     (error 'authentication-error
            :message (format nil "Unsupported PostgreSQL authentication method: ~S"
                             (getf authentication :type))))))
