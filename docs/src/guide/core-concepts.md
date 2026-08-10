# Core concepts

## One protocol stream per connection

A connection owns one PostgreSQL protocol stream. Ordinary operations are
serialized so startup, authentication, queries, and transaction state cannot
interleave accidentally. A COPY operation additionally owns the connection
until it finishes. `cancel-request` is the exception: it uses a separate
cancel transport so another thread can request cancellation while the query
transport is busy.

The normal lifecycle is:

```text
make-connection -> connect -> query / prepare / COPY -> disconnect
```

Use `disconnect` for normal application cleanup.

## Results and typed values

`query` returns `query-result` objects. A result carries column
metadata, decoded rows, the command tag, transaction status, and notices. Use
`result-rows-as-alists` when column-name keyed rows are more convenient than
positional rows.

The type registry maps PostgreSQL type OIDs to text and binary codecs. The
default registry includes numeric, JSON/JSONB, `bytea`, date/time, interval,
UUID, arrays, ranges, and composite values. Register an application-specific
codec with `register-type` or one of the structured registration helpers.

SQL `NULL` is represented by `+sql-null+`. This keeps a database null distinct
from an ordinary Lisp `NIL` value. Use `typed-value` when a parameter needs an
explicit PostgreSQL type or format.

## Transactions and server-side state

Use `begin-transaction`/`commit-transaction`/`rollback-transaction` for
transaction boundaries.
`with-transaction` commits on normal completion and rolls back when its body
signals a condition. Savepoints are available through `savepoint`,
`release-savepoint`, and `rollback-to-savepoint`.

Prepared statements are created with `prepare`, executed with
`execute-prepared`, and released with `close-prepared`. Named portal cursors
provide bounded fetching through `open-cursor`, `cursor-fetch`, and
`cursor-close`. A connection permits one active cursor at a time.

## Pools reset sessions

`make-pool` creates a pool around a connection factory. Borrow with
`pool-acquire` or `with-connection`, and return with `pool-release`. Returning
a connection rolls back an unfinished transaction and runs `DISCARD ALL` so
session state does not leak to the next borrower. If reset fails, the session
is retired rather than returned to the idle set.

`min-size` eagerly warms the configured floor during pool creation. Idle and
aged sessions are evicted lazily by `pool-reap`; call `pool-refill` when a
maintenance loop should restore the minimum without waiting for a borrower.

## Bound long-running operations

Queries, pipelines, prepared execution, and cursors accept result row and raw
payload byte limits. Cursor limits cover the aggregate stream across all
fetches. A limit violation retires the connection because the unread backend
messages cannot be safely discarded and the protocol stream reused.

The `:query-timeout` option sends PostgreSQL's out-of-band cancel request and
retires the affected session. Manual `cancel-request` does not retire a
connection by itself; the resulting exchange determines whether the session
remains healthy.

## Notifications, COPY, and replication

Use `listen`, `unlisten`, and `notify` for asynchronous notifications. Notices
and notifications can be handled through connection handlers or polled with
`poll-notification` and `wait-for-notification`.

`copy-in-start`/`copy-in-write`/`copy-in-finish` implement `COPY FROM STDIN`,
and `copy-out-start`/`copy-out-read` implement `COPY TO STDOUT`. The
`copy-both-*` operations expose the bidirectional flow used by streaming
replication. `replication-start` and `replication-read` decode XLogData and
primary keepalive messages; standby feedback is sent with the corresponding
replication status functions.
