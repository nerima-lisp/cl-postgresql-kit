(in-package #:cl-postgresql-kit)

(defun %connection-unix-socket-p (connection)
  (let ((host (connection-host connection)))
    (and (stringp host)
         (plusp (length host))
         (char= (char host 0) #\/))))

(defun %ssl-request (connection)
  (transport-write-all (connection-transport connection)
                       (encode-ssl-request))
  (transport-flush (connection-transport connection))
  (let ((response (transport-read-exactly (connection-transport connection) 1)))
    (case (code-char (aref response 0))
      (#\S
       (%start-tls connection))
      (#\N
       (setf (connection-tls-established-p connection) nil)
       (when (member (connection-ssl-mode connection)
                     '(:require :verify-ca :verify-full))
         (error 'tls-error :message "The server refused SSL negotiation."))
       :rejected)
      (otherwise
       (error 'protocol-error :message "Invalid SSL negotiation response.")))))

(defun %ssl-mode-requests-tls-p (mode)
  (member mode '(:prefer :require :verify-ca :verify-full)))

(defun %connection-direct-tls-p (connection)
  (and (eq (connection-ssl-negotiation connection) :direct)
       (%ssl-mode-requests-tls-p (connection-ssl-mode connection))
       (not (%connection-unix-socket-p connection))))

(defun %connection-gss-mode-requests-p (connection)
  (and (member (connection-gssenc-mode connection) '(:prefer :require)
               :test #'eq)
       (not (%connection-unix-socket-p connection))))

(defun %start-tls (connection)
  (transport-start-tls (connection-transport connection)
                       :hostname (unless (eq (connection-ssl-mode connection)
                                             :verify-ca)
                                  (connection-host connection))
                       :verify (not (null (member (connection-ssl-mode connection)
                                                  '(:verify-ca :verify-full)))))
  (setf (connection-tls-established-p connection) t)
  :accepted)

(defun %ssl-required-server-error-p (condition)
  (when (typep condition 'server-error)
    (let ((message (string-downcase (or (server-error-message condition) "")))
          (sqlstate (server-error-sqlstate condition)))
      (or (search "ssl required" message)
          (search "ssl connection is required" message)
          (search "ssl is required" message)
          (search "tls required" message)
          (search "tls connection is required" message)
          (search "must use ssl" message)
          (search "no encryption" message)
          (search "ssl off" message)
          (and (member sqlstate '("08004" "28000") :test #'string=)
               (or (search "ssl" message)
                   (search "tls" message)
                   (search "encrypt" message)))))))

(defun %connect-request-ssl-p (connection)
  (and (%ssl-mode-requests-tls-p (connection-ssl-mode connection))
       (not (%connection-direct-tls-p connection))))

(defun %gss-request (connection)
  (let ((mode (connection-gssenc-mode connection)))
    (unless (transport-gss-available-p (connection-transport connection))
      (when (eq mode :require)
        (error 'unsupported-feature
               :feature :gss-encryption
               :message "GSS encryption is required but the active transport does not provide GSS support."))
      (return-from %gss-request :unavailable))
    (transport-write-all (connection-transport connection)
                         (encode-gssenc-request))
    (transport-flush (connection-transport connection))
    (let ((response (transport-read-exactly (connection-transport connection) 1)))
      (case (code-char (aref response 0))
        (#\G
         (transport-start-gss
          (connection-transport connection)
          :hostname (connection-host connection)
          :service (connection-gss-service-name connection))
         (setf (connection-gss-established-p connection) t)
         :accepted)
        (#\N
         (when (eq mode :require)
           (error 'unsupported-feature
                  :feature :gss-encryption
                  :message "The server refused GSS encryption negotiation."))
         :rejected)
        (otherwise
         (error 'protocol-error
                :message "Invalid GSS encryption negotiation response."))))))

(defun %connect-ssl-fallback-p (connection condition request-ssl-p
                                 ssl-server-rejected-p
                                 authentication-started-p)
  (or (and (eq (connection-ssl-mode connection) :allow)
           (not request-ssl-p)
           (not authentication-started-p)
           (%ssl-required-server-error-p condition))
      (and (eq (connection-ssl-mode connection) :prefer)
           request-ssl-p
           ssl-server-rejected-p
           (not authentication-started-p))))
