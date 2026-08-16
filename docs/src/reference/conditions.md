# Conditions

All client conditions inherit from `postgresql-condition`. Errors inherit from
`postgresql-error`, while `notice` represents a non-error server event.

## Condition families

```text
postgresql-condition
├── postgresql-error
│   ├── protocol-error
│   ├── transport-error
│   ├── connection-error
│   │   ├── authentication-error
│   │   │   └── oauth-discovery-required
│   │   └── tls-error
│   ├── query-error
│   │   ├── multiple-results-error
│   │   ├── transaction-error
│   │   ├── copy-error
│   │   └── server-error
│   ├── timeout-error
│   ├── pool-error
│   │   └── pool-exhausted
│   ├── unsupported-feature
│   └── parameter-error
└── notice
```

The exported condition names are the supported handling points. A handler can
target a specific operation, a family such as `query-error`, or the common
`postgresql-error` superclass.

## Common readers

Every condition exposes `postgresql-condition-message` when it carries a
message. The more specific readers are:

- `protocol-error-context`, `protocol-error-expected`, and
  `protocol-error-actual` describe a framing or protocol mismatch.
- `transport-error-operation` and `transport-error-cause` identify the failed
  transport operation and underlying cause.
- `tls-error-cause` exposes the TLS-layer cause.
- `oauth-discovery-response` returns the server's OAuth discovery response
  string, and `oauth-discovery-server-fields` returns its parsed error fields.
- `timeout-error-cancel-sent-p` and
  `timeout-error-connection-retired-p` record timeout cleanup outcomes.
- `pool-exhausted-timeout` records the acquisition timeout that was exceeded.
- `unsupported-feature-name` and `parameter-error-parameter` identify the
  rejected feature or parameter.

## Server errors and notices

`server-error` preserves PostgreSQL error fields through
`server-error-fields`. Common fields have dedicated readers:
`server-error-severity`, `server-error-sqlstate`, `server-error-message`,
`server-error-detail`, `server-error-hint`, `server-error-position`,
`server-error-where`, `server-error-schema`, `server-error-table`,
`server-error-column`, `server-error-datatype`, `server-error-constraint`,
`server-error-file`, `server-error-line`, and `server-error-routine`.

Fields that the client does not have a dedicated reader for remain available
through `server-error-unknown-fields`; this prevents newer server fields from
being silently discarded. `notice-fields` provides the same field structure
for a non-error notice.

## SQL NULL

`sql-null` is the exported marker class for SQL `NULL`. Use `sql-null-p` or
compare with `+sql-null+`; do not use a generic `NIL` check when the difference
between SQL `NULL` and a Lisp value matters.
