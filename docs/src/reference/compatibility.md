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

## Authentication

The connection startup path supports these authentication mechanisms when the
server offers them:

- cleartext password, restricted to verified TLS;
- MD5 password;
- SCRAM-SHA-256;
- SCRAM-SHA-256-PLUS with TLS channel binding; and
- OAUTHBEARER with TLS and an OAuth token provider.

The optional OAuth provider is supplied through the connection options. The
client rejects cleartext authentication without `:verify-ca` or
`:verify-full` and rejects OAUTHBEARER without both TLS and a token provider.
The `:channel-binding` option accepts `:disable`, `:prefer` (the default), and
`:require`; `:prefer` uses SCRAM-SHA-256-PLUS when TLS channel-binding data is
available, while `:require` fails when it is unavailable.

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
The native socket transport does not expose TLS channel-binding bytes, so applications
that require SCRAM-SHA-256-PLUS must provide a transport implementation that
implements `transport-channel-binding-data`.

`make-connection` defaults to `:disable` because TLS is an optional system
dependency. Production deployments that require encryption should choose
`:verify-full` (or `:verify-ca`) explicitly.

## Transports and platforms

The native socket transport currently provides blocking TCP I/O and readiness
waiting on SBCL. On other implementations, the native socket methods signal
`unsupported-feature`; an implementation-specific transport can be supplied
through the transport protocol. The in-memory transport is intended for
deterministic protocol tests and does not model a live server or network.

## Test boundary

The ASDF test system exercises framing, authentication helpers, typed values,
SQL `NULL`, conditions, pooling, cancellation, COPY, pipelines, replication,
and result limits through in-memory transport. It does not prove
interoperability with a running PostgreSQL server, a particular server
version, a real DNS/socket environment, or a TLS implementation.

For those cases, run a separate integration suite against the target server
and transport. Keep claims about server extensions or version-specific SQL in
that integration documentation rather than treating the protocol tests as a
compatibility guarantee.
