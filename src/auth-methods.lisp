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

(defun %authenticate-scram (connection &key (mechanism "SCRAM-SHA-256")
                                      channel-binding-data)
  (unless (connection-password connection)
    (error 'authentication-error :message "A password is required for SCRAM authentication."))
  (let* ((plus-p (string= mechanism "SCRAM-SHA-256-PLUS"))
         (gs2-header (if plus-p "p=tls-server-end-point,," "n,,"))
         (channel-binding
           (if plus-p
               (progn
                 (unless (connection-tls-established-p connection)
                   (error 'authentication-error
                          :message "SCRAM channel binding requires TLS."))
                 (unless (%transport-octet-vector-p channel-binding-data)
                   (error 'authentication-error
                          :message "SCRAM channel binding data is unavailable."))
                 (octets-to-base64
                  (concatenate '(vector (unsigned-byte 8))
                               (%string-to-octets gs2-header)
                               channel-binding-data)))
               "biws"))
         (nonce (%random-scram-nonce))
         (client-first-bare
           (format nil "n=~A,r=~A"
                   (%scram-username (connection-user connection)) nonce))
         (client-first (format nil "~A~A" gs2-header client-first-bare)))
    (%send-frontend-message
     connection
     (encode-sasl-initial-response mechanism (%string-to-octets client-first)))
    (let* ((continue (%read-backend-message connection))
           (continue-kind (backend-message-kind (backend-message-type continue))))
      (unless (eq continue-kind :authentication)
        (error 'authentication-error
               :message "The server did not send a SCRAM continuation."))
      (let* ((authentication (parse-authentication
                              (backend-message-payload continue)))
             (server-first (getf authentication :data))
             (attributes (%scram-attributes
                          server-first
                          :allowed-names '("r" "s" "i")))
             (server-nonce (%scram-attribute attributes "r"))
             (salt-text (%scram-attribute attributes "s"))
             (iteration-text (%scram-attribute attributes "i")))
        (unless (and server-nonce salt-text iteration-text
                     (>= (length server-nonce) (length nonce))
                     (string= nonce server-nonce :end2 (length nonce)))
          (error 'authentication-error :message "The SCRAM server nonce is invalid."))
        (let* ((salt (base64-to-octets salt-text))
               (iterations (handler-case
                               (parse-integer iteration-text :junk-allowed nil)
                             (error () nil))))
          (unless (and (integerp iterations)
                       (plusp iterations)
                       (<= iterations *maximum-scram-iterations*))
            (error 'authentication-error
                   :message "The SCRAM iteration count is invalid or exceeds the configured maximum."))
          (let* ((client-final-without-proof
                   (format nil "c=~A,r=~A" channel-binding server-nonce))
                 (auth-message
                   (format nil "~A,~A,~A"
                           client-first-bare server-first client-final-without-proof))
                 (salted-password
                   (scram-hi (connection-password connection) salt iterations))
                 (client-key (hmac-sha256 salted-password
                                          (%string-to-octets "Client Key")))
                 (stored-key (sha256-digest client-key))
                 (client-signature (hmac-sha256 stored-key
                                                (%string-to-octets auth-message)))
                 (client-proof (%xor-octet-vectors client-key client-signature))
                 (server-key (hmac-sha256 salted-password
                                          (%string-to-octets "Server Key")))
                 (server-signature (hmac-sha256 server-key
                                                (%string-to-octets auth-message)))
                 (client-final
                   (format nil "~A,p=~A"
                           client-final-without-proof
                           (octets-to-base64 client-proof))))
            (%send-frontend-message
             connection
             (encode-sasl-response (%string-to-octets client-final)))
            (let* ((final (%read-backend-message connection))
                   (final-payload (backend-message-payload final))
                   (final-kind (backend-message-kind
                                (backend-message-type final))))
              (unless (eq final-kind :authentication)
                (error 'authentication-error
                       :message "The server did not send a SCRAM final message."))
              (let* ((final-auth (parse-authentication final-payload))
                     (server-final (getf final-auth :data))
                     (server-final-attributes
                       (%scram-attributes server-final
                                          :allowed-names '("v" "e")))
                     (server-error (%scram-attribute server-final-attributes "e"))
                     (server-proof (%scram-attribute server-final-attributes "v")))
                (when server-error
                  (error 'authentication-error
                         :message "The PostgreSQL server rejected SCRAM authentication."))
                (unless (and server-proof
                             (%constant-time-string=
                              server-proof (octets-to-base64 server-signature)))
                  (error 'authentication-error
                         :message "The SCRAM server signature is invalid."))))))))))

(defun %authenticate-oauthbearer (connection)
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
      (let ((initial-response
              (format nil "n,a=,~Cauth=Bearer ~A~C~C"
                      (code-char 1) token (code-char 1) (code-char 1))))
        (%send-frontend-message
         connection
         (encode-sasl-initial-response
          "OAUTHBEARER" (%string-to-octets initial-response))))))
  (loop
    for message = (%read-backend-message connection)
    do (let ((kind (backend-message-kind (backend-message-type message))))
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
              (return nil))
             (otherwise
             (error 'authentication-error
                     :message "Unexpected authentication response for OAUTHBEARER.")))))))

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
