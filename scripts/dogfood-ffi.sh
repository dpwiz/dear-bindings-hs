#!/usr/bin/env bash
# Regenerate FFI bindings into the per-flavor stack packages plus
# the flavor-neutral backend package, then build the consumer test
# projects that exercise both flavors against the same backend.
# This is the multi-package verification for dear-bindings-ffi.
#
# Usage:
#   scripts/dogfood-ffi.sh                 -- both vanilla + docking
#   scripts/dogfood-ffi.sh vanilla         -- just vanilla
#   scripts/dogfood-ffi.sh docking         -- just docking
#
# What runs each invocation:
#   1. (re)build dear-bindings-ffi
#   2. regenerate the per-flavor core package's src/ tree
#   3. stack-build the core package (standalone)
#   4. regenerate the (single, shared) backend package once
#   5. for each chosen flavor: stack-build the consumer test project
#      under dist-ffi/test-<flavor>/, which transitively exercises the
#      backend package against the matching core via cabal flags
#
# Prerequisite (one-off): imgui sources vendored into each core's
# cbits/imgui/, plus imgui_impl_*.cpp variants vendored into the
# backend's cbits/. If any are missing the script tells you and stops.

set -euo pipefail

REPO="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO"

flavors=()
case "${1:-}" in
  vanilla|docking)
    flavors=("$1")
    ;;
  ""|all)
    flavors=(vanilla docking)
    ;;
  *)
    echo "usage: $0 [vanilla|docking|all]" >&2
    exit 2
    ;;
esac

backend_dir="generated/backends/dear-imgui-raw-impl-opengl3"
backend_json="generated/backends/dcimgui_impl_opengl3.json"

echo "==> Building dear-bindings-ffi"
stack build --flag dear-bindings-aeson:executables >/dev/null

# Regenerate every requested core, build it standalone.
for flavor in "${flavors[@]}"; do
  core_dir="generated/${flavor}/dear-imgui-raw-${flavor}"
  core_json="dear_bindings/${flavor}/dcimgui_nodefaultargfunctions.json"

  if [ ! -f "${core_dir}/package.yaml" ]; then
    echo "==> ${flavor}: ${core_dir}/package.yaml missing — set up the package first" >&2
    exit 1
  fi
  if [ ! -d "${core_dir}/cbits/imgui" ] || [ -z "$(ls -A "${core_dir}/cbits/imgui" 2>/dev/null)" ]; then
    echo "==> ${flavor}: ${core_dir}/cbits/imgui is empty — vendor imgui sources first" >&2
    exit 1
  fi
  if [ ! -f "$core_json" ]; then
    echo "==> ${flavor}: core JSON missing at $core_json" >&2
    exit 1
  fi

  echo "==> ${flavor}: regenerating core into ${core_dir}/src"
  rm -rf "${core_dir}/src" "${core_dir}/cbits/DearImGuiWrappers.cpp" "${core_dir}/cbits/DearImGuiWrappers.h"
  stack exec -- dear-bindings-ffi \
    --input "$core_json" \
    --module-root DearImGui.Raw \
    -o "${core_dir}"

  echo "==> ${flavor}: stack build ${core_dir}"
  ( cd "${core_dir}" && stack build )
  echo "==> ${flavor}: core OK"
done

# Regenerate the (single) backend package — flavor-neutral; the
# Haskell modules and shim cpp are identical across cores. The
# imgui-side .cpp variants for each flavor are vendored separately
# and selected via cabal flag at consume time.
if [ ! -f "${backend_dir}/package.yaml" ]; then
  echo "==> backend: ${backend_dir}/package.yaml missing — set up the package first" >&2
  exit 1
fi
if [ ! -f "${backend_dir}/cbits/imgui_impl_opengl3_vanilla.cpp" ] || \
   [ ! -f "${backend_dir}/cbits/imgui_impl_opengl3_docking.cpp" ]; then
  echo "==> backend: vendor imgui_impl_opengl3_{vanilla,docking}.cpp into ${backend_dir}/cbits/ first" >&2
  exit 1
fi
if [ ! -f "$backend_json" ]; then
  echo "==> backend: input JSON missing at $backend_json" >&2
  exit 1
fi

echo "==> backend: regenerating impl-opengl3 into ${backend_dir}/src"
rm -rf "${backend_dir}/src" "${backend_dir}/cbits/DearImGuiWrappers.cpp" "${backend_dir}/cbits/DearImGuiWrappers.h"
stack exec -- dear-bindings-ffi \
  --input "$backend_json" \
  --module-root DearImGui.Raw.Impl.OpenGL3 \
  --header dcimgui_impl_opengl3.h \
  --external-types-module DearImGui.Raw.Types \
  -o "${backend_dir}"

# Build each requested flavor's consumer test. This transitively
# builds the backend against the chosen core (via cabal flags) and
# proves cross-package type identity holds — Test.hs assigns
# core.ImDrawData to the impl's foreign-import expectation.
for flavor in "${flavors[@]}"; do
  test_dir="dist-ffi/test-${flavor}"

  if [ ! -f "${test_dir}/package.yaml" ]; then
    echo "==> ${flavor}: ${test_dir}/package.yaml missing — scaffold the consumer first" >&2
    exit 1
  fi

  echo "==> ${flavor}: stack build ${test_dir} (exercises backend)"
  ( cd "${test_dir}" && stack build )
  echo "==> ${flavor}: consumer OK"
done

echo "==> All requested flavors built."
