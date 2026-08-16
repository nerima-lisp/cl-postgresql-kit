# API reference

The public API is in the `cl-postgresql-kit` package. The groups below follow
the ownership boundaries used by the implementation; the source package
definition remains the authoritative list of exported symbols.

## Connections

### Create and close connections

- `make-connection` creates a connection object from host, port, database,
  user, password, timeout, TLS, OAuth, and transport options.
- `make-connection-from-string ""` additionally applies `PG*` environment
  defaults, `PGSERVICE`/`PGSERVICEFILE`, and `PGPASSFILE`/`.pgpass`; direct
  `make-connection` remains an explicit builder.
- `parse-connection-string` and `parse-connection-uri` parse PostgreSQL
  connection descriptions.
- Connection descriptions support endpoint, authentication, TLS/security,
  startup-parameter, and session-selection options. Environment values are
  overridden by a service profile and then by explicit description values;
  passfiles fill in only an absent password.
- `:hosts`, `:hostaddrs`, and `:ports` can describe multiple candidate
  endpoints. `:target-session-attrs` filters candidates by their session role;
  `:load-balance-hosts :random` shuffles the candidate order for each connect.
- Empty entries in those endpoint lists select the platform default host or
  port 5432, matching PostgreSQL connection-description semantics.
- `:oauth-token-provider` receives a connection and returns an OAuth token
  string. `:oauth-discovery-provider` receives a connection and the server's
  discovery response string, returns a token string, and is used for a fresh
  startup retry when the server requests OAuth discovery.
- `:channel-binding` controls SCRAM channel binding with `:disable`, `:prefer`
  (default), or `:require`.
- `:ssl-negotiation` selects PostgreSQL or direct TLS negotiation;
  `:gssenc-mode`, `:gss-service-name`, and `:require-auth` control GSS
  encryption and authentication policy.
- `:tls-options` passes per-stream `cl+ssl` options such as `:certificate`,
  `:key`, `:password`, `:alpn-protocols`, `:cipher-list`, and `:method`; its
  `:min-proto-version` and `:max-proto-version` limit the TLS protocol range,
  while `:verify-location` configures a connection-local CA context.
- `make-connection-from-string` and `make-connection-from-uri` construct a
  connection from those descriptions.
- `connect` opens the transport and performs startup and authentication.
- `disconnect` performs normal connection cleanup.
- `connection-state`, `connection-healthy-p`, and the connection accessors
  expose lifecycle and configuration state.

### Observability

- `:metric-registry` optionally attaches a `cl-observability-kit` registry to
  `make-connection`.
- `connection-metric-registry` returns the attached registry, or `nil` when
  instrumentation is disabled.
- Instrumented operations publish `postgresql_operations_total` with
  `operation` and `status` labels, plus
  `postgresql_operation_duration_seconds` with an `operation` label. Metrics
  are emitted at the shared operation boundary and instrumentation failures do
  not replace the database operation's condition.

### Notifications and cancellation

- `listen`, `unlisten`, `unlisten-all`, and `notify` manage PostgreSQL
  notification channels.
- `poll-notification` and `wait-for-notification` consume queued
  notifications.
- `cancel-request` sends an out-of-band cancel packet through the configured
  cancel transport.

## Queries and results

### Execute SQL

- `query` and `query-all` execute SQL with optional parameters and result
  limits.
- `query-async` starts an asynchronous request flow.
- `query-cps` composes the asynchronous query Promise with success and error
  continuations through `cl-concurrent-kit`; both continuations are validated
  before the underlying asynchronous operation is started.
- `flush` flushes pending frontend messages when an asynchronous sequence
  needs explicit control.
- `function-call` invokes a PostgreSQL function through the function-call
  protocol message.

### Deadlines and query timeouts

- `:query-timeout` on `make-connection` sets a positive client-side timeout in
  seconds. A missing or zero value leaves the query unbounded by that setting.
- `cl-resilience-kit:with-deadline` can bound a larger operation. The active
  deadline caps the query timeout, and an expired query is cancelled before
  its connection is retired.

### Inspect results

- `query-result` is the result object type; `query-result-p` recognizes it.
- `result-columns`, `result-rows`, `result-row-count`, and
  `result-command-tag` expose the main result data.
- `result-transaction-status`, `result-notices`, and
  `result-portal-suspended-p` expose associated protocol state.
- `result-row`, `result-column`, `row-value`, `result-row-alist`, and
  `result-rows-as-alists` provide row and column access helpers.

### Pipelines

- `make-pipeline-request` creates a request with explicit parameter types,
  formats, statement names, or portal names.
- `query-pipeline` sends batches and returns results in request order.
- `query-pipeline-async` exposes the asynchronous pipeline variant.
- `query-pipeline-cps` composes that Promise with success and error
  continuations through `cl-concurrent-kit`, with the same preflight
  validation as `query-cps`.

## Prepared statements and cursors

### Prepared statements

- `prepare` creates a `prepared-statement` for a connection.
- `execute-prepared` executes it with parameters, formats, and result limits.
- `close-prepared` releases the server-side statement.
- `prepared-statement-name`, `prepared-statement-sql`,
  `prepared-statement-connection`, `prepared-statement-parameter-type-oids`,
  and `prepared-statement-columns` inspect the statement.

### Cursors

- `open-cursor` creates a named portal cursor with a configurable fetch size
  and aggregate result limits.
- `cursor-fetch` retrieves the next batch; `cursor-close` closes the portal.
- `cursor-columns`, `cursor-command-tag`, `cursor-suspended-p`,
  `cursor-done-p`, and `cursor-closed-p` expose cursor state.

## Transactions

- `begin-transaction`, `commit-transaction`, and `rollback-transaction`
  manage transaction boundaries.
- `savepoint`, `release-savepoint`, and `rollback-to-savepoint` manage nested
  transaction boundaries.
- `with-transaction` wraps a body with commit-on-success and rollback-on-error
  behavior.

## Connection pools

- `make-pool` creates a pool from a connection factory.
- `pool-acquire` and `pool-release` borrow and return connections;
  `with-connection` manages that pair around a body.
- `pool-close` closes the pool and its managed sessions.
- `pool-size`, `pool-available-count`, and `pool-in-use-count` report pool
  state.
- `pool-reap` removes expired or unhealthy idle connections, and
  `pool-refill` restores the configured minimum size.
- `pool-max-size`, `pool-min-size`, `pool-max-idle-time`,
  `pool-max-connection-age`, `pool-validation-query`, and
  `pool-connection-factory` expose pool configuration.

## COPY and replication

### COPY

- `copy-in` and `copy-out` start their respective COPY operations.
- `copy-in-start`, `copy-in-write`, `copy-in-abort`, and `copy-in-finish`
  manage `COPY FROM STDIN` incrementally. `copy-in-abort` sends PostgreSQL's
  `CopyFail` message and consumes the expected error before returning the
  connection to the ready state.
- `copy-in-write-row` encodes one logical row with the registered codecs,
  while `copy-in-write-text-stream` and `copy-in-write-binary-stream` send
  complete text or binary COPY streams.
- `copy-out-start` and `copy-out-read` manage `COPY TO STDOUT` incrementally.
- `copy-out-read-all` concatenates the raw server stream for decoding or
  persistence.
- `copy-out-read-rows` reads and decodes a complete text or binary stream with
  the registered codecs.
- `copy-both-start`, `copy-both-write`, `copy-both-read`, and
  `copy-both-finish` expose the bidirectional COPY flow. `copy-both-finish`
  sends the client `CopyDone` message and drains the server side; there is no
  `CopyFail` abort message for `COPY BOTH`.
- `copy-both-write-row`, `copy-both-write-text-stream`, and
  `copy-both-write-binary-stream` provide the same
  encoding helpers for the client-to-server side of COPY BOTH, and
  `copy-both-read-all` and `copy-both-read-rows` concatenate or decode its
  server-to-client chunks.
- `encode-copy-row`, `decode-copy-row`, `encode-copy-text-stream`, and
  `decode-copy-text-stream` convert logical rows using a type registry.
- Internally, `copy-codecs` owns text row codecs plus the shared row format
  boundary, `copy-codecs-row-binary` owns binary row payload codecs,
  `copy-codecs-text-stream` owns text stream framing, and
  `copy-codecs-binary` owns binary COPY stream framing.

`COPY TO STDOUT` has no frontend abort message in PostgreSQL's wire protocol.
When abandoning it, use `cancel-request` or close/retire the connection rather
than leaving the operation's backend messages unread.

### Streaming replication

- `replication-start` and `replication-finish` manage a replication stream.
- `replication-read` decodes XLogData and primary keepalive messages.
- `replication-send-status` and `replication-send-hot-standby-feedback`
  send standby feedback.
- `replication-identify-system`, `replication-create-slot`,
  `replication-read-slot`, `replication-drop-slot`, and
  `replication-alter-slot` expose slot and system discovery/management
  operations. `replication-alter-slot` supports explicitly enabling or
  disabling `TWO_PHASE` and `FAILOVER`, as well as additional server options.
- `replication-start-physical` and `replication-start-logical` construct the
  corresponding `START_REPLICATION` flows.

#### Logical output decoding

- `make-logical-replication-decoder` creates a stateful `pgoutput` decoder
  backed by the package's type registry and a concurrent relation metadata
  cache.
- Pass each parsed `logical-replication-message` to
  `decode-logical-replication-message`. Relation messages populate the cache;
  insert, update, and delete messages return events with decoded values.
- `decode-logical-replication-tuple` can decode a tuple directly with
  `:tuple-kind :new`, `:key`, or `:old`, so replica-identity subsets are
  mapped to the correct relation columns.
- Text and binary fields use the registered codec for their PostgreSQL type
  OID. Unknown OIDs remain raw octet vectors. SQL NULL is `+sql-null+`, while
  an unchanged TOAST field is `+logical-replication-unchanged-toast+`.
- `logical-replication-read` combines the physical replication envelope,
  `pgoutput` parsing, relation-cache updates, and type decoding in one read.
  Its result exposes the physical `replication-message`, parsed logical
  message, and typed logical event; keepalives have no logical event.
- `decode-logical-replication-stream-message` performs the same composition
  for an already parsed physical envelope, which is useful when the caller
  owns the COPY BOTH read loop.
- Use `register-logical-replication-relation`,
  `find-logical-replication-relation`, `forget-logical-replication-relation`,
  and `clear-logical-replication-relations` when relation metadata must be
  managed explicitly.

## Type codecs

### Registries and codecs

- `make-type-codec` creates a codec from text and/or binary encoder and decoder
  functions.
- `make-type-registry`, `default-type-registry`, `register-type`, and
  `find-type-codec` manage OID-to-codec lookup.
- `register-array-type`, `register-enum-type`, `register-domain-type`,
  `register-range-type`, and `register-composite-type` register structured
  PostgreSQL types.
- `encode-value` and `decode-value` use a registry to translate parameters
  and result fields.

### Value wrappers

The exported wrappers make wire-level intent explicit for values that need
special treatment:

- `typed-value` for an explicit type OID and format;
- `json-value`, `bytea-value`, `date-value`, `time-value`,
  `timestamp-value`, `timestamptz-value`, `interval-value`, `uuid-value`,
  `postgres-bit-string`, and `postgres-mac-address`;
- `postgres-array`, `postgres-range`, and `postgres-composite` for structured
  values; and
- `sql-null` and `+sql-null+` for SQL `NULL`.

`bytea-value` affects parameter encoding. Result decoding for built-in `bytea`
text codecs expects PostgreSQL's hex output form (`\\x...`).

## Large objects

`large-object-create`, `large-object-open`, `large-object-close`,
`large-object-read`, `large-object-write`, `large-object-seek`,
`large-object-tell`, `large-object-truncate`, and `large-object-unlink`
provide descriptor-based PostgreSQL large-object operations.  The
server-side helpers `large-object-read-all`, `large-object-read-range`,
`large-object-from-bytea`, and `large-object-write-at` cover bytea-backed
creation, range reads, and offset writes without exposing a descriptor.

`large-object-import-server-file` and `large-object-export-server-file`
operate on the PostgreSQL server's filesystem, not the client filesystem.
They require the corresponding server privileges and should be treated as
privileged operations.

## Wire and transport primitives

### Transports

`transport` defines the transport protocol. `make-socket-transport` creates the
native socket transport, while `make-memory-transport` creates an in-memory
transport for protocol tests. The transport protocol includes open/close,
exact reads, available reads, readiness waits, writes, flushing, TLS startup,
channel binding, and health checks.

### Wire messages

The low-level API includes frame builders and parsers such as
`make-frame`, `parse-frame`, `read-backend-message`,
`encode-startup-message`, `encode-query-message`, `encode-bind-message`, and
`encode-sync-message`. Authentication, error, notification, COPY, and
replication payloads have corresponding `parse-*` and `encode-*` functions.
