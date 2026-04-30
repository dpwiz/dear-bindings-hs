#!/usr/bin/env bash
# Wipe FFI generation artifacts so the next `scripts/generate-all-ffi.sh`
# (or `scripts/dogfood-ffi.sh`, which delegates to it) runs from a clean
# slate. Does NOT touch the repo-root .stack-work (dear-bindings-ffi
# itself stays built — rebuilding it isn't part of the artifact lifecycle).
#
# generated-out/ is fully derived from generated-in/ + package-templates/,
# so a flat rm is safe.
#
# Usage: scripts/clean-ffi.sh

set -euo pipefail

REPO="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO"

echo "==> wiping generated-out/"
rm -rf generated-out

for tdir in test-ffi-scaffold/vanilla test-ffi-scaffold/docking; do
  [ -d "$tdir" ] || continue
  echo "==> cleaning $tdir"
  rm -rf "$tdir/.stack-work"
  rm -f  "$tdir"/*.cabal
done

echo "==> clean."
