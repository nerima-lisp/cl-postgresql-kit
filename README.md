# cl-postgresql-kit

[![Documentation](https://img.shields.io/badge/docs-nerima--lisp.github.io-teal)](https://nerima-lisp.github.io/cl-postgresql-kit/)

`cl-postgresql-kit` is a PostgreSQL wire-protocol client for Common Lisp. It
follows the conventions of the nerima-lisp package family and provides
connections, typed results, pooling, COPY, and replication primitives.

The [documentation site](https://nerima-lisp.github.io/cl-postgresql-kit/)
contains the complete getting-started guide, API reference, compatibility
notes, and development instructions.

## Quick Start

```lisp
(asdf:load-system "cl-postgresql-kit")

(let ((connection
        (cl-postgresql-kit:make-connection-from-string "")))
  (unwind-protect
       (progn
         (cl-postgresql-kit:connect connection)
         (let ((result (cl-postgresql-kit:query connection "select 1")))
           (cl-postgresql-kit:result-rows result)))
    (cl-postgresql-kit:disconnect connection)))
```

`make-connection-from-string` applies libpq-style defaults: `PG*` environment
variables, `PGSERVICE`/`PGSERVICEFILE`, and `PGPASSFILE`/`.pgpass` when an
explicit password is not supplied. Direct `make-connection` is the explicit
builder and does not read those environment or service-file defaults.

`query` returns a result object with column metadata, a command tag, and
decoded rows. SQL `NULL` is represented by the exported `+sql-null+` singleton.
Use [Getting started](https://nerima-lisp.github.io/cl-postgresql-kit/getting-started/)
for connection options and bounded result handling.

## Install

Make this repository visible to ASDF, then load the `cl-postgresql-kit` system.
Its direct dependencies are `cl-codec-kit`, `cl-json-kit`, `cl-date-kit`,
`cl-concurrent-kit`, `cl-log-kit`, `cl-observability-kit`, and
`cl-resilience-kit` from the
nerima-lisp package family. TLS is optional; load
`cl-postgresql-kit/tls` when `cl+ssl` support is available.

## Documentation

- [Getting started](https://nerima-lisp.github.io/cl-postgresql-kit/getting-started/)
- [API reference](https://nerima-lisp.github.io/cl-postgresql-kit/reference/api/)
- [Compatibility](https://nerima-lisp.github.io/cl-postgresql-kit/reference/compatibility/)

## Development

The self-contained test system uses an in-memory transport and does not require
a running PostgreSQL server:

```lisp
(asdf:test-system "cl-postgresql-kit/test")
```

Real-server authentication, socket/TLS interoperability, and server-specific
SQL behavior require a separate integration environment. The integration entry
point requires an explicit URI and exits with an error when it is absent; it
never treats an unavailable server as a passing test:

```text
PGKIT_TEST_URI='postgresql://user@127.0.0.1:5432/database' \
  sbcl --script scripts/run-integration.lisp

PGKIT_TEST_TLS=required \
PGKIT_TEST_URI='postgresql://user@127.0.0.1:5432/database?sslmode=verify-full' \
  sbcl --script scripts/run-integration.lisp
```

The URI is not printed by the integration runner. Use `PGKIT_TEST_TLS=required`
to assert that TLS was actually established; the TLS system is loaded only
when the URI or that explicit requirement asks for it.

Run the self-contained coverage measurement with:

```text
sbcl --script scripts/run-coverage.lisp
```

The coverage report measures production `src/` forms. Enforce the 100% project
target through cl-weave's native thresholds:

```text
PGKIT_COVERAGE_MINIMUM_EXPRESSION=100 \
PGKIT_COVERAGE_MINIMUM_BRANCH=100 \
  sbcl --script scripts/run-coverage.lisp
```

The strict command exits non-zero when either threshold is not met. The plain
command is useful for measuring progress, while live-server authentication,
TLS interoperability, and server-specific SQL behavior remain separate
integration gates.

Build the docs with the flake's reproducible MkDocs Material output:

```text
nix build .#docs
```

For a direct local build outside the flake, use a Python environment containing
both MkDocs and its Material theme:

```text
nix-shell --impure -p python3Packages.mkdocs python3Packages.mkdocs-material \
  python3Packages.pymdown-extensions \
  --run 'mkdocs build --strict -f docs/mkdocs.yml'
```

The flake uses `cl-nix-forge` for reproducible package, test, development-shell,
and coverage outputs:

```text
nix run .#test
nix build .#coverage
```

When concurrent Common Lisp sessions share the default SBCL FASL cache, use the
repository wrapper to give one verification run its own temporary cache root:

```text
sh scripts/with-isolated-cache.sh nix run .#test
sh scripts/with-isolated-cache.sh nix develop --command cl-weave list cl-postgresql-kit/test --filter protocol
```

The wrapper preserves an explicitly-set `XDG_CACHE_HOME`; otherwise it creates
and removes a temporary cache directory for the wrapped command.

## Contributing

Please keep public API changes, protocol behavior, and documentation in sync.
Add or update focused tests when changing protocol or state-machine behavior.

## Support

Use the [GitHub issue tracker](https://github.com/nerima-lisp/cl-postgresql-kit/issues)
for reproducible bugs, protocol questions, and documentation corrections.

## License

MIT. See the `:license` declaration in `cl-postgresql-kit.asd`.
