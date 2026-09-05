(in-package #:cl-postgresql-kit/test)

(deftest transport-validation-and-defaults
  (let ((transport (make-instance 'cl-postgresql-kit::transport)))
    (is (not (transport-alive-p transport)))
    (is (eq transport (transport-open transport)))
    (is (transport-alive-p transport))
    (is (eq transport (transport-close transport)))
    (is (not (transport-alive-p transport)))
    (is (equalp #() (transport-read-available transport)))
    (is (not (transport-wait-readable transport 0)))
    (is (null (transport-channel-binding-data transport)))
    (assert-signals 'unsupported-feature
                    (lambda ()
                      (transport-start-tls transport)))))

(deftest transport-tls-options-normalize
  (is (null (cl-postgresql-kit::%normalize-tls-options nil)))
  (is-funcall-results
   #'cl-postgresql-kit::%normalize-tls-options
   '(((:alpn-protocols nil
       :certificate nil
       :key nil
       :password nil
       :cipher-list nil
       :method :default
       :verify-location :default
       :min-proto-version nil
       :max-proto-version nil)
      (:alpn-protocols nil :certificate nil :key nil
       :password nil :cipher-list nil :method :default
       :verify-location :default :min-proto-version nil
       :max-proto-version nil))
     ((:alpn-protocols ("postgres")
       :certificate #P"/tmp/cert.pem"
       :key #P"/tmp/key.pem"
       :verify-location (#P"/tmp/ca.pem")
       :min-proto-version "TLSv1.2"
       :max-proto-version "TLSv1.3")
      (:alpn-protocols ("postgres")
       :certificate "/tmp/cert.pem"
       :key "/tmp/key.pem"
       :verify-location ("/tmp/ca.pem")
       :min-proto-version :tlsv1-2
       :max-proto-version :tlsv1-3))))
  (is-funcall-results
   #'cl-postgresql-kit::%normalize-tls-verify-location
   '((:default :default)
     (:default-file :default-file)
     (:default-dir :default-dir)
     (nil nil)))
  (let ((circular (list :method :default)))
    (setf (cdr (last circular)) circular)
    (assert-signals 'parameter-error
                    (lambda ()
                      (cl-postgresql-kit::%normalize-tls-options circular))))
  (assert-funcall-signals-each
   'parameter-error
   '((:unknown t)
     (:certificate 42)
     (:alpn-protocols ("ok" 42))
     (:verify-location (42))
     (:password 42)
     (:min-proto-version "SSLv3")
     (:max-proto-version "SSLv3")
     (:min-proto-version "TLSv1.3"
      :max-proto-version "TLSv1.2")
     (:method :x :method :y))
   #'cl-postgresql-kit::%normalize-tls-options))

(deftest socket-transport-construction
  (let ((transport (make-socket-transport
                     :host "localhost"
                     :port 5433
                     :timeout 5
                     :tls-options '(:method :default))))
    (is (equal "localhost"
               (cl-postgresql-kit::socket-transport-host transport)))
    (is (= 5433 (cl-postgresql-kit::socket-transport-port transport)))
    (is (= 5 (cl-postgresql-kit::socket-transport-timeout transport)))
    (is (equal '(:method :default)
               (cl-postgresql-kit::socket-transport-tls-options transport)))
    (is (eq transport
            (cl-postgresql-kit::%clear-transport-tls-options transport)))
    (is (null (cl-postgresql-kit::socket-transport-tls-options transport))))
  (let ((transport (make-socket-transport)))
    (is (equal "127.0.0.1"
               (cl-postgresql-kit::socket-transport-host transport)))
    (is (= 5432 (cl-postgresql-kit::socket-transport-port transport)))
    (is (= 30 (cl-postgresql-kit::socket-transport-timeout transport)))))

(deftest socket-transport-unix-socket-path
  (let ((transport (make-socket-transport
                     :host "/var/run/postgresql"
                     :port 5433)))
    (is (cl-postgresql-kit::%socket-transport-local-p transport))
    (is (equal "/var/run/postgresql/.s.PGSQL.5433"
               (cl-postgresql-kit::%socket-transport-local-path transport))))
  (let ((transport (make-socket-transport :host "/" :port 5432)))
    (is (equal "/.s.PGSQL.5432"
               (cl-postgresql-kit::%socket-transport-local-path transport)))))

(deftest transport-octet-validation
  (is (cl-postgresql-kit::%transport-octet-vector-p #(0 1 255)))
  (is (not (cl-postgresql-kit::%transport-octet-vector-p #(256))))
  (is (not (cl-postgresql-kit::%transport-octet-vector-p '(1 2))))
  (is (equalp #(1 2)
              (cl-postgresql-kit::%transport-check-octets #(1 2))))
  (assert-signals 'parameter-error
                    (lambda ()
                      (cl-postgresql-kit::%transport-check-octets #(1 256)))))

#+sbcl
(deftest socket-transport-requires-open-stream
  (let ((transport (make-socket-transport)))
    (assert-signals 'parameter-error
                   (lambda ()
                     (transport-read-exactly transport -1)))
    (it-signals-each 'transport-error
        ((read-exactly)
          (read-available)
          (wait-readable)
          (write-all))
      "signals transport-error for @label without an open stream"
      (label)
      (ecase label
        (read-exactly
         (transport-read-exactly transport 1))
        (read-available
         (transport-read-available transport))
        (wait-readable
         (transport-wait-readable transport 0))
        (write-all
         (transport-write-all transport (octets 1)))))
    (is (transport-flush transport))
    (is (not (transport-alive-p transport)))))

#+sbcl
(deftest socket-transport-open-wraps-resolution-errors
  (let ((transport (make-socket-transport
                     :host "256.256.256.256"
                     :timeout 1)))
    (assert-signals 'transport-error
                   (lambda ()
                     (transport-open transport)))
    (is (null (cl-postgresql-kit::socket-transport-stream transport)))
    (is (null (cl-postgresql-kit::socket-transport-socket transport)))
    (is (not (transport-alive-p transport)))))
