(in-package #:cl-postgresql-kit)

(defclass connection ()
  ((host :initarg :host :accessor connection-host)
   (hostaddr :initarg :hostaddr :accessor connection-hostaddr :initform nil)
   (port :initarg :port :accessor connection-port)
   (user :initarg :user :reader connection-user)
   (password :initarg :password :accessor connection-password)
   (passfile :initarg :passfile :reader connection-passfile :initform nil)
    (oauth-token-provider :initarg :oauth-token-provider
                          :accessor connection-oauth-token-provider
                          :initform nil)
    (oauth-discovery-provider :initarg :oauth-discovery-provider
                              :accessor connection-oauth-discovery-provider
                              :initform nil)
    (oauth-discovery-response :initform nil
                              :accessor connection--oauth-discovery-response)
    (oauth-discovery-active-p :initform nil
                              :accessor connection--oauth-discovery-active-p)
    (oauth-discovery-token :initform nil
                           :accessor connection--oauth-discovery-token)
    (database :initarg :database :reader connection-database)
   (application-name :initarg :application-name :reader connection-application-name)
   (startup-parameters :initarg :startup-parameters
                       :reader connection-startup-parameters
                       :initform nil)
   (protocol-version :initarg :protocol-version
                     :reader connection-protocol-version
                     :initform +protocol-version-3.0+)
   (negotiated-protocol-version :initform nil
                                :accessor connection-negotiated-protocol-version)
   (ssl-mode :initarg :ssl-mode :reader connection-ssl-mode)
   (ssl-negotiation :initarg :ssl-negotiation
                    :reader connection-ssl-negotiation
                    :initform :postgres)
   (tls-options :initarg :tls-options
                :reader connection-tls-options
                :initform nil)
   (gssenc-mode :initarg :gssenc-mode
                :reader connection-gssenc-mode
                :initform :prefer)
   (gss-service-name :initarg :gss-service-name
                     :reader connection-gss-service-name
                     :initform "postgres")
   (channel-binding :initarg :channel-binding
                    :reader connection-channel-binding
                    :initform :prefer)
   (require-auth :initarg :require-auth
                 :reader connection-require-auth
                 :initform nil)
   (target-session-attrs :initarg :target-session-attrs
                         :reader connection-target-session-attrs
                         :initform :any)
   (load-balance-hosts :initarg :load-balance-hosts
                       :reader connection-load-balance-hosts
                       :initform :disable)
   (gss-token-provider :initarg :gss-token-provider
                       :reader connection-gss-token-provider
                       :initform nil)
   (sspi-token-provider :initarg :sspi-token-provider
                        :reader connection-sspi-token-provider
                        :initform nil)
   (authentication-method :initform nil
                          :accessor connection--authentication-method)
   (authentication-requested-p :initform nil
                               :accessor connection--authentication-requested-p)
   (authentication-observed-method :initform nil
                                   :accessor connection--authentication-observed-method)
   (tls-established-p :initform nil :accessor connection-tls-established-p)
   (gss-established-p :initform nil :accessor connection-gss-established-p)
   (transport :initarg :transport :accessor connection-transport)
   (endpoints :initarg :endpoints :reader connection--endpoints)
   (initial-transport :initarg :initial-transport
                      :reader connection--initial-transport
                      :initform nil)
   (initial-transport-used-p :initform nil
                             :accessor connection--initial-transport-used-p)
   (transport-factory :initarg :transport-factory
                      :reader connection--transport-factory
                      :initform nil)
   (logger :initarg :logger :reader connection-logger)
   (metrics :initarg :metrics :reader connection--metrics :initform nil)
   (type-registry :initarg :type-registry :accessor connection-type-registry)
   (query-timeout :initarg :query-timeout :reader connection-query-timeout)
   (connect-timeout :initarg :connect-timeout :reader connection-connect-timeout)
   (cancel-transport-factory :initarg :cancel-transport-factory
                             :reader connection-cancel-transport-factory
                             :initform nil)
   (open :initform nil :accessor connection-open)
   (state :initform :new :accessor connection-state)
   (parameters :initform (make-hash-table :test #'equal)
               :reader connection-parameters)
   (backend-process-id :initform nil :accessor connection-backend-process-id)
   (backend-secret-key :initform nil :accessor connection-backend-secret-key)
   (transaction-status :initform :idle :accessor connection-transaction-status)
   (notice-handler :initarg :notice-handler :accessor connection-notice-handler)
   (notification-handler :initarg :notification-handler
                         :accessor connection-notification-handler)
   (max-notifications :initarg :max-notifications
                      :reader connection-max-notifications
                      :initform 10000)
   (notifications :initform nil :accessor connection--notifications)
   (notifications-lock
    :initform (cl-concurrent-kit:make-lock :name "postgresql-notifications")
    :reader connection--notifications-lock)
   (prepared-statements :initform (make-hash-table :test #'equal)
                        :reader connection--prepared-statements)
   (lock :initform (cl-concurrent-kit:make-lock :name "postgresql-connection")
         :reader connection--lock)
   (read-buffer :initform (make-array 0 :element-type '(unsigned-byte 8)
                                      :adjustable t :fill-pointer 0)
                :accessor connection--read-buffer)
   (pending-backend-messages :initform nil
                             :accessor connection--pending-backend-messages)
   (pending-error :initform nil :accessor connection--pending-error)
   (last-result :initform nil :accessor connection-last-result)
   (active-cursors :initform nil :accessor connection--active-cursors)
   (active-copy :initform nil :accessor connection--active-copy)))

(defstruct (query-result
             (:constructor %make-query-result
                 (&key columns rows command-tag row-count transaction-status notices
                       portal-suspended-p)))
  columns
  rows
  command-tag
  row-count
  transaction-status
  notices
  portal-suspended-p)

(defstruct (pipeline-request
             (:constructor %make-pipeline-request
                 (&key sql parameters parameter-type-oids parameter-formats
                       result-formats statement-name portal-name)))
  sql
  parameters
  parameter-type-oids
  parameter-formats
  result-formats
  statement-name
  portal-name)

(defstruct (pipeline-entry
             (:constructor %make-pipeline-entry
                 (&key request statement-name portal-name close-statement-p
                       columns raw-rows command-tag portal-suspended-p notices
                       results current-result-p pending-error)))
  request
  statement-name
  portal-name
  close-statement-p
  columns
  raw-rows
  command-tag
  portal-suspended-p
  notices
  results
  current-result-p
  pending-error)

(defstruct (prepared-statement
             (:constructor %make-prepared-statement
                 (&key name sql parameter-type-oids columns connection)))
  name
  sql
  parameter-type-oids
  columns
  connection)

(defclass cursor ()
  ((connection :initarg :connection :reader cursor-connection)
   (statement-name :initarg :statement-name :reader cursor-statement-name)
   (close-statement-p :initarg :close-statement-p :initform t
                      :reader cursor--close-statement-p)
   (portal-name :initarg :portal-name :reader cursor-portal-name)
   (fetch-size :initarg :fetch-size :reader cursor-fetch-size)
   (max-result-rows :initarg :max-result-rows :initform nil
                    :reader cursor-max-result-rows)
   (max-result-bytes :initarg :max-result-bytes :initform nil
                     :reader cursor-max-result-bytes)
   (result-row-count :initform 0 :accessor cursor--result-row-count)
   (result-bytes :initform 0 :accessor cursor--result-bytes)
   (columns :initform #() :accessor cursor-columns)
   (pending-rows :initform #() :accessor cursor--pending-rows)
   (suspended-p :initform nil :accessor cursor--suspended-p)
   (done-p :initform nil :accessor cursor-done-p)
   (closed-p :initform nil :accessor cursor-closed-p)
   (command-tag :initform nil :accessor cursor-command-tag)))

(defun cursor-p (object)
  (typep object 'cursor))

(defun cursor-suspended-p (cursor)
  (check-type cursor cursor)
  (cursor--suspended-p cursor))

(defparameter *statement-counter* 0)

(defparameter *maximum-scram-message-length* (* 16 1024))

(defparameter *maximum-scram-attribute-count* 32)

(defparameter *maximum-scram-attribute-length* (* 8 1024))
