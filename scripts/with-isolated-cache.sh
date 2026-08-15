#!/bin/sh

set -eu

if [ "$#" -eq 0 ]; then
  echo "usage: sh scripts/with-isolated-cache.sh COMMAND [ARG ...]" >&2
  exit 64
fi

if [ -n "${XDG_CACHE_HOME:-}" ]; then
  "$@"
  exit $?
fi

cache_dir=$(mktemp -d "${TMPDIR:-/tmp}/cl-postgresql-kit-cache.XXXXXX")

cleanup() {
  rm -rf "$cache_dir"
}

trap cleanup EXIT HUP INT TERM

XDG_CACHE_HOME="$cache_dir" "$@"
