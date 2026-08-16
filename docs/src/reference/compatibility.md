# Compatibility

This page describes the protocol and transport boundary covered by the
current implementation. It is not a PostgreSQL server-version support
matrix.

## Wire protocol

The core client supports PostgreSQL wire-protocol versions 3.0 and 3.2.
`make-connection` defaults to 3.0; pass `:protocol-version` to select the
other supported startup version. When a server sends
`NegotiateProtocolVersion`, the connection records the negotiated version and
uses it for subsequent protocol parsing. This describes the wire-protocol
boundary, not support for every PostgreSQL server release or extension.

For built-in `bytea` text decoding, the client accepts PostgreSQL's hex form
(`\\x...`) and rejects the legacy escape form. Applications that still depend
on escape-form `bytea` text results should switch the query or server setting
to hex output before relying on the built-in codec.

## Connection descriptions and defaults

Connection strings and URIs cover endpoint selection (`host`, `hostaddr`, and
`port`), authentication (`user`, `password`, `passfile`, and `database`), TLS
(`sslmode`, `sslcert`, `sslkey`, `sslpassword`, `sslrootcert`,
`ssl_min_protocol_version`, and `ssl_max_protocol_version`), security negotiation (`sslnegotiation`,
`gssencmode`, `krbsrvname`, `channel_binding`, and `require_auth`), startup
parameters (`application_name`, `client_encoding`, `options`, and
`replication`), and session selection (`target_session_attrs` and
`load_balance_hosts`). Unknown parameters signal `unsupported-feature` rather
than being silently ignored.

An empty connection string applies `PG*` environment defaults, then a
`PGSERVICE` profile from `PGSERVICEFILE`, then explicit connection-string
properties. `PGPASSFILE` or the default passfile can supply a password only
when no explicit password was provided. On Unix-like systems the default host
is the PostgreSQL Unix-socket directory; on Windows it is the loopback host.
`hostaddr` can be used to separate the address used for transport from the
hostname used for TLS verification. Empty items in comma-separated host,
hostaddr, and port lists retain libpq's default semantics: the platform
default host and port 5432. The same rule applies to empty URI host-list
items.

## Authentication

The connection startup path supports these authentication mechanisms when the
server offers them:

- cleartext password, restricted to verified TLS;
- MD5 password;
- SCRAM-SHA-256;
- SCRAM-SHA-256-PLUS with TLS channel binding; and
- OAUTHBEARER with TLS and either an OAuth token provider or an OAuth
  discovery provider.

OAuth providers are supplied through the connection options. The token
provider receives the connection and returns an OAuth token string. The
discovery provider receives the connection and the server's discovery
response string, then returns a token string; the client retries startup on a
new connection with that token. If no suitable provider is configured, the
client signals an authentication error. The client rejects cleartext
authentication without `:verify-ca` or `:verify-full` and requires TLS for
OAUTHBEARER. The `:channel-binding` option accepts `:disable`, `:prefer` (the
default), and `:require`; `:prefer` uses SCRAM-SHA-256-PLUS when TLS
channel-binding data is available, while `:require` fails when it is
unavailable.

## TLS modes

The `:ssl-mode` connection option accepts the following values:

| Mode | Behavior |
| --- | --- |
| `:disable` | Do not negotiate TLS. |
| `:allow` | Start without TLS and retry with TLS when the server explicitly requires it. |
| `:prefer` | Request TLS first, then continue without TLS only when the server declines the request. |
| `:require` | Require encrypted transport, without certificate verification. |
| `:verify-ca` | Require TLS and verify the certificate chain. |
| `:verify-full` | Require TLS, verify the certificate chain, and verify the server hostname. |

TLS support is optional. Load `cl-postgresql-kit/tls` when `cl+ssl` is
available; the core system remains usable with a non-TLS transport.
The native socket transport passes the per-stream `cl+ssl` options `:certificate`,
`:key`, `:password`, `:alpn-protocols`, `:cipher-list`, and `:method` from
`:tls-options`. `:verify-location` additionally creates a connection-local
`cl+ssl` context from a pathname, a CL+SSL default location keyword, or a list
of pathnames. The libpq value `sslrootcert=system` maps to the CL+SSL default
location and implies `:verify-full` when `sslmode` is omitted.
The TLS minimum and maximum versions can be set with `:min-proto-version` and
`:max-proto-version`, or the `ssl_min_protocol_version` and
`ssl_max_protocol_version` connection parameters, using TLS 1.0 through TLS
1.3. The maximum cannot be lower than the minimum. With
`sslnegotiation=direct`, the connection requires an encrypting TLS
mode and advertises PostgreSQL's direct-TLS ALPN protocol. The native socket
transport does not expose TLS channel-binding bytes, so applications that
require SCRAM-SHA-256-PLUS must provide a transport implementation that
implements `transport-channel-binding-data`.

`make-connection` defaults to `:disable` because TLS is an optional system
dependency. Production deployments that require encryption should choose
`:verify-full` (or `:verify-ca`) explicitly.

## Operations and typed values

The public operation surface includes simple and parameterized queries,
prepared statements, named cursors, transactions and savepoints, pipelines,
notifications, cancellation, pooling, text and binary COPY, large objects,
physical replication, logical replication and `pgoutput` decoding, and base
backup primitives. The type registry includes built-in scalar, temporal,
JSON, UUID, bit-string, network, array, range, composite, enum, domain, and
SQL `NULL` handling. The API reference describes the operation-specific
limits and transport requirements.

COPY follows PostgreSQL's subprotocol boundaries: `copy-in-abort` sends
`CopyFail` and consumes the expected `ErrorResponse`/`ReadyForQuery` sequence;
`COPY TO STDOUT` is drained or canceled/closed because the frontend has no
abort message; and `copy-both-finish` sends the client `CopyDone` and drains
the remaining server stream. Automatic query cancellation opens a separate
control connection to the active endpoint and prefers the configured
`hostaddr` when one is supplied.

## Transports and platforms

The native socket transport currently provides blocking TCP I/O and readiness
waiting on SBCL. On other implementations, the native socket methods signal
`unsupported-feature`; an implementation-specific transport can be supplied
through the transport protocol. The in-memory transport is intended for
deterministic protocol tests and does not model a live server or network.

## Test boundary

The ASDF test system exercises framing, authentication helpers including the
OAuth discovery exchange, typed values, SQL `NULL`, conditions, pooling,
cancellation, COPY, pipelines, replication, and result limits through
in-memory transport. It does not prove
interoperability with a running PostgreSQL server, a particular server
version, a real DNS/socket environment, or a TLS implementation.

For those cases, run a separate integration suite against the target server
and transport. Keep claims about server extensions or version-specific SQL in
that integration documentation rather than treating the protocol tests as a
compatibility guarantee.
