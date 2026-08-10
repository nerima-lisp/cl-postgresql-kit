# Development

## Repository layout

- `cl-postgresql-kit.asd` defines the core, test, and optional TLS systems.
- `src/` contains the package, wire protocol, transport, connection, pool,
  type, and COPY implementation. Data records live in focused `*-data.lisp`
  files, while protocol and state-machine logic stays in the corresponding
  operation files.
- `t/` contains the self-contained protocol and client tests.
- `docs/mkdocs.yml` and `docs/src/` contain this documentation site.
- `run-tests.lisp` is the reproducible local ASDF test entry point; the flake
  exposes the same test boundary as `.#test`.

## Load and test

Make the repository visible to ASDF, then load or test the systems from a
Common Lisp image:

```lisp
(asdf:load-system "cl-postgresql-kit")
(asdf:test-system "cl-postgresql-kit/test")
```

The test system registers examples with `cl-weave` and asserts that the
registered set is non-empty. Its protocol tests use the in-memory transport,
so a running PostgreSQL server is not required. Use a separate integration
environment for live authentication, sockets, TLS, and server-specific SQL
behavior. The live integration runner requires `PGKIT_TEST_URI`; an unset URI
is an error and is never reported as a skipped or passing test:

```text
PGKIT_TEST_URI='postgresql://user@127.0.0.1:5432/database' \
  sbcl --script scripts/run-integration.lisp
```

To make TLS a required assertion, use a TLS-enabled URI and set:

```text
PGKIT_TEST_TLS=required \
PGKIT_TEST_URI='postgresql://user@127.0.0.1:5432/database?sslmode=verify-full' \
  sbcl --script scripts/run-integration.lisp
```

The runner does not print the URI. It loads the optional
`cl-postgresql-kit/tls` system when the URI requests TLS and checks
`connection-tls-established-p` when TLS is required.

The flake provides the same test boundary and a coverage artifact:

```text
nix run .#test
nix build .#coverage
```

Coverage is measured by `scripts/run-coverage.lisp` through `cl-weave` for
production code. Enforce the 100% target with cl-weave's native thresholds:

```text
PGKIT_COVERAGE_MINIMUM_EXPRESSION=100 \
PGKIT_COVERAGE_MINIMUM_BRANCH=100 \
  sbcl --script scripts/run-coverage.lisp
```

The strict command exits non-zero when either threshold is not met. The
report does not claim integration coverage for optional TLS or live-server
behavior.

## Build the documentation

From the repository root, run:

```text
nix build .#docs
```

The strict build catches broken internal links, missing navigation targets,
and configuration warnings. For a direct local build outside the flake, use:

```text
nix-shell --impure -p python3Packages.mkdocs python3Packages.mkdocs-material \
  python3Packages.pymdown-extensions \
  --run 'mkdocs build --strict -f docs/mkdocs.yml'
```

The generated site is written to `site/`; it is a build artifact and should
not be committed.

## Keeping docs in sync

When the public surface changes, update the corresponding reference page and
the examples in Getting Started. Use `src/package.lisp` as the source for
exports, `src/conditions.lisp` for the condition hierarchy,
`cl-postgresql-kit.asd` for system boundaries, and the connection and
transport sources for authentication and platform claims.

Keep examples package-qualified and keep the root README as a short entry
point. Add a project page only when it describes behavior that exists in the
repository; do not turn an absent roadmap into a promise.
