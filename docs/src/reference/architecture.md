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
4. `types-data` defines value records and codec/registry data, while `types`,
   the `codecs-*` files, the temporal/collection/structured files, and
   `types-registration` implement type behavior.
5. `connection-data`, `observability`, `connection-build`, the connection-string
   files, the `auth-*` files, and `connection-startup` build and authenticate
   connections. The optional `cl-observability-kit` registry is attached here
   without changing the wire-protocol path when it is not configured.
6. `connection`, `query-parameters`, `query-execution`, `cursor`, and
   `query-timeout` implement session state and protocol operations.
7. `operation` centralizes the shared timeout and failed-exchange retirement
   boundary; `query` and `query-pipeline` provide the synchronous request
   APIs and their asynchronous/CPS counterparts.
8. `type-registry`, `advanced-operations`, and `large-object` contain focused
   protocol operation families.
9. `pool`, the `copy-*` files, and `replication` implement pooling, COPY, and
   replication data flows.

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
array, range, composite, and UUID families; applications can register their
own codecs or wrappers.

The data/logic split is deliberate: record definitions and constructors remain
small and stable, while codecs and protocol operations can evolve independently
without hiding state transitions inside adapters.

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
their continuation boundary.

### Pool and cancellation

The pool owns acquisition, release, validation, lazy eviction, and minimum
size refill. A connection returned to the pool is reset or retired according
to its transaction and health state. Cancellation uses a separate control
connection so the query stream can receive and process the server's cancel
response independently.

See [Core concepts](../guide/core-concepts.md) for operation lifecycles and
the [API reference](api.md) for the public entry points.
