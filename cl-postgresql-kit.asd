(in-package #:asdf-user)

(asdf:defsystem "cl-postgresql-kit"
  :description "A PostgreSQL wire-protocol client for Common Lisp."
  :version "1.0.0"
  :license "MIT"
  :depends-on ((:version "cl-codec-kit" "0.5.0")
               (:version "cl-json-kit" "1.2.0")
               (:version "cl-date-kit" "1.0.0")
               (:version "cl-concurrent-kit" "0.6.1")
               (:version "cl-log-kit" "2.2.0")
               (:version "cl-observability-kit" "1.0.0")
               (:version "cl-resilience-kit" "1.0.0"))
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "wire")
               (:file "crypto")
               (:file "transport")
               (:file "transport-memory")
               (:file "protocol-encode")
               (:file "protocol-response")
               (:file "protocol-replication")
               (:file "types-data")
               (:file "types")
               (:file "codecs-primitives")
               (:file "codecs-numeric")
               (:file "codecs-builtin")
               (:file "codecs-binary")
               (:file "codecs-json-uuid")
               (:file "types-temporal")
               (:file "types-collections")
               (:file "types-structured-text")
               (:file "types-structured-range")
               (:file "types-structured-composite")
               (:file "types-registration")
               (:file "connection-data")
               (:file "observability")
               (:file "connection-build")
               (:file "connection-string-libpq")
               (:file "connection-string-uri")
               (:file "auth-crypto")
               (:file "auth-protocol")
               (:file "auth-methods")
               (:file "connection-startup")
               (:file "connection")
               (:file "query-parameters")
               (:file "query-execution-frames")
               (:file "query-execution-data")
               (:file "query-execution-results")
               (:file "query-execution")
               (:file "cursor")
               (:file "query-timeout")
               (:file "operation")
               (:file "query")
               (:file "query-pipeline-build")
               (:file "query-pipeline-results")
               (:file "query-pipeline")
               (:file "type-registry")
               (:file "advanced-operations")
               (:file "large-object")
               (:file "pool")
               (:file "copy-codecs")
               (:file "copy-session")
               (:file "replication"))
  :in-order-to ((test-op (test-op "cl-postgresql-kit/test"))))

(asdf:defsystem "cl-postgresql-kit/test"
  :description "Protocol and client tests for cl-postgresql-kit."
  :depends-on ("cl-postgresql-kit"
               (:version "cl-weave" "1.3.0"))
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "support")
               (:file "protocol-tests")
               (:file "connection-tests")
               (:file "query-tests")
               (:file "pool-tests")
               (:file "copy-tests")
               (:file "large-object-tests")
               (:file "runner"))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:cl-postgresql-kit/test '#:run-tests)))

;; TLS stays optional because the portable/core client has no dependency on a
;; foreign SSL library.  Loading this system adds the CL+SSL transport method.
(asdf:defsystem "cl-postgresql-kit/tls"
  :description "TLS transport support for cl-postgresql-kit."
  :depends-on ("cl-postgresql-kit" "cl+ssl")
  :pathname "src"
  :components ((:file "tls")))
