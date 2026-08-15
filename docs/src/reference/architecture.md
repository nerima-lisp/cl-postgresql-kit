# Architecture

`cl-postgresql-kit` is layered around the PostgreSQL wire protocol. The
public connection, query, pool, and COPY APIs sit above a transport boundary,
so protocol tests can run without opening a real socket.

## ASDF layers

The core system is loaded serially in this order:

1. `package` and `conditions` define the package surface and condition types.
2. `wire`, `crypto`, and `transport` provide framing, authentication helpers,
   and byte-stream operations.
3. `protocol` parses backend messages and constructs frontend messages.
   `protocol-response` keeps shared response framing plus row, copy, and
   error-response decoders, while `protocol-response-startup` keeps the
   connection-startup/authentication response parsers. The physical/base-backup
   replication envelope stays in `protocol-replication`, while pgoutput
   tuple/message records and parser helpers live in
   `protocol-replication-logical-data`, relation/type/row message parsers
   live in `protocol-replication-logical-row`, and transaction, stream,
   prepare, and public entrypoint logic live in
   `protocol-replication-logical`. This keeps wire-level data definitions
   stable while message decoding logic stays localized by message family.
4. `types-data` defines value records and codec/registry data, while `types`,
   the `codecs-*` files, the temporal/collection/structured files,
   `types-registration`, and `types-registration-builtins` implement type
   behavior.
5. `connection-data`, `observability`, the connection-string files,
   `connection-build`, the `auth-*` files, and the `connection-startup*`
   files build and authenticate connections. `connection-string-libpq` now
   centralizes shared connection-option normalization used by both string
   parsing and `make-connection`, while `connection-build` assembles the
   runtime connection object. `auth-methods-scram` and
   `auth-methods-oauth` keep the mechanism-specific SASL exchanges, while
   `auth-methods` keeps MD5, cleartext, GSS/SSPI, and the
   authentication-method dispatcher. `connection-startup-tls` isolates SSL/TLS
   negotiation and downgrade policy, `connection-startup-session` isolates
   startup-parameter assembly and target-session evaluation, and
   `connection-startup` keeps endpoint retry/orchestration. The optional
   `cl-observability-kit` registry is attached here without changing the
   wire-protocol path when it is not configured.
6. `connection`, `query-parameters`, `query-execution-frames`,
   `query-execution-data`, `query-execution-results`, `query-execution`,
   `cursor`, and `query-timeout` implement session state and protocol
   operations.
7. `operation` centralizes the shared timeout and failed-exchange retirement
   boundary; `query`, `query-prepared`, and `query-pipeline` provide the
   synchronous request APIs and their asynchronous/CPS counterparts.
8. `type-registry`, `advanced-operations-data`, `advanced-operations`,
   `large-object-data`, and `large-object` contain focused protocol operation
   families. `advanced-operations-data` keeps the internal CPS continuation
   boundary, connection-operation macro, and ReadyForQuery drain helper, while
   `advanced-operations` keeps the public async and CPS entry points. The
   `large-object-data` layer keeps descriptor state, validation, and shared
   scalar-query helpers, while `large-object` keeps the public API entry
   points.
9. `pool-data`, `pool`, and the `copy-*` files implement pooling and COPY data flows.
   `copy-codecs` owns text row codecs plus the shared row format boundary,
   `copy-codecs-row-binary` owns binary row payload codecs,
   `copy-codecs-text-stream` owns text stream framing,
   `copy-codecs-binary` owns binary stream framing, `copy-session` owns COPY
   startup, client writes, and shared operation state, and
   `copy-session-read` owns server stream reads, row decoding helpers, and
   protocol completion.
10. `replication-base-backup-results` owns BASE_BACKUP result-state and
    result-set aggregation, while `replication-base-backup` owns the
    BASE_BACKUP start/read/finish stream lifecycle. `replication` owns the
    physical replication and slot operations, while `logical-replication`
    owns the logical decoder lifecycle and relation-cache state. The focused
    `logical-replication-events` unit decodes pgoutput tuples and logical
    row/message events on top of that cached metadata, while
    `logical-replication-events-stream` maps physical replication envelopes
    into those logical events and owns the COPY BOTH read boundary.

The optional `cl-postgresql-kit/tls` system is separate from the core system.
It depends on `cl+ssl` and supplies the TLS transport method without making a
foreign SSL library a core dependency.

## Boundaries

### Wire and transport

The wire layer owns PostgreSQL frame construction and parsing. The transport
layer owns opening, reading, writing, flushing, and closing a byte stream,
plus readiness waiting and TLS negotiation. The native socket transport is
implemented separately from the in-memory transport used by protocol tests.

### Protocol and types

The protocol layer turns backend frames into typed protocol messages and
serializes frontend messages. The type registry maps PostgreSQL type OIDs to
codecs. Built-in codecs cover the common scalar, temporal, JSON, binary,
array, range, composite, and UUID families; the scalar codecs stay in
`codecs-builtin`, `types-collections` keeps the generic value entry points,
`types-collections-array` keeps shared array shape validation,
`types-collections-array-text` and `types-collections-array-binary` split the
array text and binary/vector codec mechanics, `types-temporal` keeps shared
temporal epoch/constants and value coercion, `types-temporal-text` and
`types-temporal-binary` split temporal text and binary codecs,
`types-structured-text` keeps shared structured text tokenization and quoting,
`types-structured-range` owns range and multirange codecs,
`types-structured-composite` owns composite codecs,
`types-registration-builtins` seeds the default registry and array OIDs, and
`codecs-network` / `codecs-network-address` divide LSN/bit/MAC handling from
inet/cidr parsing and formatting. Applications can register their own codecs
or wrappers.

The data/logic split is deliberate: record definitions and constructors remain
small and stable, while codecs and protocol operations can evolve independently
without hiding state transitions inside transport or protocol integrations.

### Connection and operations

A connection owns one protocol stream and its session state. Query, prepared
statement, cursor, transaction, notification, and COPY operations use that
state and must follow the server's message ordering. A cursor or COPY
operation therefore remains tied to its connection until it is closed or
finished.

Query execution uses `cl-concurrent-kit` for preemptive client timeouts and
caps those timeouts with the active `cl-resilience-kit` deadline. A timed-out
stream is cancelled and retired before the connection can return to a pool.
The shared operation boundary records optional success/error counters and
duration histograms through `cl-observability-kit`; metric updates are
best-effort and never mask the operation's original condition.
The asynchronous query APIs return `cl-concurrent-kit` Promises directly;
`query-cps` and `query-pipeline-cps` use Promise fulfillment and rejection as
their continuation boundary. `query` keeps direct SQL and `FunctionCall`
entry points, while `query-prepared` owns prepared-statement lifecycle and
execution on top of the shared `%query` exchange path.

### Pool and cancellation

`pool-data` owns the pool state model plus the internal acquisition, reset,
and refill helpers. `pool` keeps the public acquisition, release, sizing, and
`with-connection` entry points on top of that internal layer. A connection
returned to the pool is reset or retired according to its transaction and
health state. Cancellation uses a separate control connection so the query
stream can receive and process the server's cancel response independently.

See [Core concepts](../guide/core-concepts.md) for operation lifecycles and
the [API reference](api.md) for the public entry points.
