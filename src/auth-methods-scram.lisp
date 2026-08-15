(in-package #:cl-postgresql-kit)

(defun %scram-client-initial-message (connection mechanism channel-binding-data)
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
    (values nonce client-first-bare channel-binding)))

(defun %scram-read-server-first (connection nonce)
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
      (let ((iterations (handler-case
                            (parse-integer iteration-text :junk-allowed nil)
                          (error () nil))))
        (unless (and (integerp iterations)
                     (plusp iterations)
                     (<= iterations *maximum-scram-iterations*))
          (error 'authentication-error
                 :message "The SCRAM iteration count is invalid or exceeds the configured maximum."))
        (values server-first
                server-nonce
                (base64-to-octets salt-text)
                iterations)))))

(defun %scram-client-final-message (connection client-first-bare server-first
                                    channel-binding server-nonce salt iterations)
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
    (values client-final server-signature)))

(defun %scram-verify-server-final (connection server-signature)
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
               :message "The SCRAM server signature is invalid.")))))

(defun %authenticate-scram (connection &key (mechanism "SCRAM-SHA-256")
                                      channel-binding-data)
  (unless (connection-password connection)
    (error 'authentication-error :message "A password is required for SCRAM authentication."))
  (multiple-value-bind (nonce client-first-bare channel-binding)
      (%scram-client-initial-message connection mechanism channel-binding-data)
    (multiple-value-bind (server-first server-nonce salt iterations)
        (%scram-read-server-first connection nonce)
      (multiple-value-bind (client-final server-signature)
          (%scram-client-final-message connection
                                       client-first-bare
                                       server-first
                                       channel-binding
                                       server-nonce
                                       salt
                                       iterations)
        (%send-frontend-message
         connection
         (encode-sasl-response (%string-to-octets client-final)))
        (%scram-verify-server-final connection server-signature)))))
