# cl-postgresql-kit

`cl-postgresql-kit` is a Common Lisp client for the PostgreSQL wire protocol.
It provides connection management, typed query results, pooling, COPY flows,
and low-level streaming replication helpers while sharing codecs and
concurrency primitives with the nerima-lisp package family.

## Start here

- [Getting started](getting-started.md) explains installation, connection
  setup, result handling, and the self-contained test system.
- [Core concepts](guide/core-concepts.md) describes connection ownership,
  result limits, transactions, pooling, and protocol-sensitive operations.
- [API reference](reference/api.md) groups the exported functions and data
  structures by responsibility.
- [Compatibility](reference/compatibility.md) records authentication, TLS,
  transport, and test-boundary details.

## Scope

The core ASDF system has no foreign TLS dependency. Load
`cl-postgresql-kit/tls` separately when `cl+ssl` support is available. The
default test system uses an in-memory transport, so protocol framing and
state-machine behavior can be tested without a running PostgreSQL server.

The self-contained suite is not a substitute for a live-server integration
environment. Authentication against a real server, native socket and TLS
interoperability, and server-specific SQL behavior need separately controlled
integration coverage.
