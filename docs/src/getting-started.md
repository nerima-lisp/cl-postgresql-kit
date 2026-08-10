# Getting started

## Load the system

Make the checkout visible to ASDF, then load the core system:

```lisp
(asdf:load-system "cl-postgresql-kit")
```

The core system depends on `cl-codec-kit`, `cl-json-kit`, `cl-date-kit`,
`cl-concurrent-kit`, `cl-log-kit`, `cl-observability-kit`, and
`cl-resilience-kit`. TLS is an optional
system dependency:

```lisp
(asdf:load-system "cl-postgresql-kit/tls")
```

Load the TLS system only when the `cl+ssl` implementation and its trust-store
configuration are available.

## Open a connection

Use the fully qualified package name in applications that do not import the
client symbols:

```lisp
(let ((connection
        (cl-postgresql-kit:make-connection
         :host (or (uiop:getenv "PGHOST") "127.0.0.1")
         :port (parse-integer (or (uiop:getenv "PGPORT") "5432"))
         :database (uiop:getenv "PGDATABASE")
         :user (uiop:getenv "PGUSER")
         :password (uiop:getenv "PGPASSWORD"))))
  (unwind-protect
       (progn
         (cl-postgresql-kit:connect connection)
         (let ((result (cl-postgresql-kit:query connection "select 1")))
           (format t "Rows: ~S~%"
                   (cl-postgresql-kit:result-rows result))))
    (cl-postgresql-kit:disconnect connection)))
```

`make-connection` accepts connection properties directly and also supports
`cl-postgresql-kit:make-connection-from-string` and
`cl-postgresql-kit:make-connection-from-uri` for PostgreSQL connection
strings and URIs.

For failover or read-scaling, pass multiple hosts with `:hosts` (and matching
`:hostaddrs` or `:ports` when needed). Use `:target-session-attrs` to select a
session role, or `:load-balance-hosts :random` to randomize the candidate order
on each connection attempt.

## Choose TLS behavior

The `:ssl-mode` option accepts `:disable`, `:allow`, `:prefer`, `:require`,
`:verify-ca`, and `:verify-full`:

```lisp
(cl-postgresql-kit:make-connection
 :ssl-mode :verify-full
 :host (or (uiop:getenv "PGHOST") "127.0.0.1"))
```

The core `make-connection` default is `:ssl-mode :disable` because TLS is an
optional system dependency. For production connections that require encryption,
select `:verify-full` (or `:verify-ca`) explicitly.

`:require` requests TLS without certificate verification. `:verify-ca`
verifies the certificate chain, and `:verify-full` also verifies the host
name. Cleartext password authentication is accepted only after verified TLS
has been established.

`:channel-binding` accepts `:disable`, `:prefer` (the default), and `:require`.
With `:prefer`, SCRAM-SHA-256-PLUS is selected when the TLS transport supplies
channel-binding data; `:require` rejects authentication unless that data and
the PLUS mechanism are available.
The native socket transport with `cl+ssl` integration does not expose
channel-binding bytes; use a custom transport implementing
`transport-channel-binding-data` when SCRAM-PLUS is required.

Options accepted by the optional `cl+ssl` integration can be supplied through
`:tls-options`:

```lisp
(cl-postgresql-kit:make-connection
 :ssl-mode :verify-full
 :tls-options '(:certificate "client.crt"
                :key "client.key"
                :password (uiop:getenv "PGSSLKEYPASSWORD")
                :alpn-protocols ("postgresql")
                :verify-location "root.crt"))
```

Supported keys are `:certificate`, `:key`, `:password`, `:alpn-protocols`,
`:cipher-list`, `:method`, and `:verify-location`. The last accepts a
pathname/string, `:default`, `:default-file`, `:default-dir`, or a list of
pathnames. The connection-string form `sslrootcert=system` selects `:default`
and implies `sslmode=verify-full` when no sslmode is supplied.

## Read results safely

`query` returns a `query-result` object. Use
`cl-postgresql-kit:result-columns`, `cl-postgresql-kit:result-rows`, and
`cl-postgresql-kit:result-command-tag` to inspect its metadata, rows, and
command tag. A SQL `NULL` value is represented by the exported
`cl-postgresql-kit:+sql-null+` singleton rather than by `NIL`.

For bounded-memory request handling, query and prepared-statement operations
accept `:max-result-rows` and `:max-result-bytes`. These limits apply to the
aggregate result stream for a cursor as well. Exceeding a client-side limit
retires the connection because unread protocol data cannot safely be reused.
Use a named portal cursor for incremental fetching:

```lisp
(let ((cursor (cl-postgresql-kit:open-cursor connection
                                             "select * from events"
                                             :fetch-size 1000)))
  (unwind-protect
       (loop
         (multiple-value-bind (rows done-p)
             (cl-postgresql-kit:cursor-fetch cursor)
           (map nil #'process-event-row rows)
           (when done-p (return))))
    (cl-postgresql-kit:cursor-close cursor)))
```

Only one active cursor is allowed per connection. Close it before issuing
another operation on that connection.

## Bound operations

Set `:query-timeout` on a connection for a client-side query limit. A positive
timeout is measured in seconds. For an operation-wide deadline, use
`cl-resilience-kit` directly:

```lisp
(cl-resilience-kit:with-deadline (:timeout 5)
  (cl-postgresql-kit:query connection "select expensive_operation()"))
```

An active resilience deadline caps the configured query timeout. When the
limit expires, the client sends PostgreSQL's cancel request and retires the
connection because its protocol stream is no longer safe to reuse.

## Run the self-contained tests

The test system uses the in-memory transport and does not require a running
PostgreSQL server:

```lisp
(asdf:test-system "cl-postgresql-kit/test")
```

The suite covers protocol framing and state-machine behavior. Use a separate
integration environment for live-server authentication, sockets, TLS, and
server-specific SQL semantics.
