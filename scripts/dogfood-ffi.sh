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

opengl3_dir="generated/backends/dear-imgui-raw-impl-opengl3"
opengl3_json="generated/backends/dcimgui_impl_opengl3.json"

glfw_dir="generated/backends/dear-imgui-raw-impl-glfw"
glfw_json="generated/backends/dcimgui_impl_glfw.json"

vulkan_dir="generated/backends/dear-imgui-raw-impl-vulkan"
vulkan_aliases="generated/backends/vulkan_type_aliases.json"

# Common --external-types-json source for all impl regens. Vanilla
# and docking core JSONs declare the same set of struct/typedef/enum
# names (only the function lists and field counts differ), so either
# works as the "external names" reference for impl-mode drops.
external_core_json="dear_bindings/vanilla/dcimgui_nodefaultargfunctions.json"

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

# Regenerate flavor-neutral backend packages. Each ships one
# Haskell module tree (output of the generator on the single shared
# JSON) plus per-flavor imgui_impl_*.cpp variants, selected via
# cabal flag at consume time. impl-{opengl3,glfw} both follow this
# pattern: dear-bindings emits identical .cpp/.h for vanilla and
# docking, so we generate once.

# impl-opengl3
if [ ! -f "${opengl3_dir}/package.yaml" ]; then
  echo "==> opengl3: ${opengl3_dir}/package.yaml missing — set up the package first" >&2
  exit 1
fi
if [ ! -f "${opengl3_dir}/cbits/imgui_impl_opengl3_vanilla.cpp" ] || \
   [ ! -f "${opengl3_dir}/cbits/imgui_impl_opengl3_docking.cpp" ]; then
  echo "==> opengl3: vendor imgui_impl_opengl3_{vanilla,docking}.cpp into ${opengl3_dir}/cbits/ first" >&2
  exit 1
fi
if [ ! -f "$opengl3_json" ]; then
  echo "==> opengl3: input JSON missing at $opengl3_json" >&2
  exit 1
fi

echo "==> opengl3: regenerating impl into ${opengl3_dir}/src"
rm -rf "${opengl3_dir}/src" "${opengl3_dir}/cbits/DearImGuiWrappers.cpp" "${opengl3_dir}/cbits/DearImGuiWrappers.h"
stack exec -- dear-bindings-ffi \
  --input "$opengl3_json" \
  --module-root DearImGui.Raw.Impl.OpenGL3 \
  --header dcimgui_impl_opengl3.h \
  --external-types-module DearImGui.Raw.Types \
  --external-types-json "$external_core_json" \
  -o "${opengl3_dir}"

# impl-glfw
if [ ! -f "${glfw_dir}/package.yaml" ]; then
  echo "==> glfw: ${glfw_dir}/package.yaml missing — set up the package first" >&2
  exit 1
fi
if [ ! -f "${glfw_dir}/cbits/imgui_impl_glfw_vanilla.cpp" ] || \
   [ ! -f "${glfw_dir}/cbits/imgui_impl_glfw_docking.cpp" ]; then
  echo "==> glfw: vendor imgui_impl_glfw_{vanilla,docking}.cpp into ${glfw_dir}/cbits/ first" >&2
  exit 1
fi
if [ ! -f "$glfw_json" ]; then
  echo "==> glfw: input JSON missing at $glfw_json" >&2
  exit 1
fi

echo "==> glfw: regenerating impl into ${glfw_dir}/src"
rm -rf "${glfw_dir}/src" "${glfw_dir}/cbits/DearImGuiWrappers.cpp" "${glfw_dir}/cbits/DearImGuiWrappers.h"
stack exec -- dear-bindings-ffi \
  --input "$glfw_json" \
  --module-root DearImGui.Raw.Impl.GLFW \
  --header dcimgui_impl_glfw.h \
  --external-types-module DearImGui.Raw.Types \
  --external-types-json "$external_core_json" \
  -o "${glfw_dir}"

# impl-vulkan is NOT flavor-neutral: docking adds multi-viewport
# fields/functions, and the vendored imgui_impl_vulkan.{cpp,h} differs
# between flavors. Generate per-flavor subtrees and let cabal flags
# select one at consume time.
if [ ! -f "${vulkan_dir}/package.yaml" ]; then
  echo "==> vulkan: ${vulkan_dir}/package.yaml missing — set up the package first" >&2
  exit 1
fi
if [ ! -f "${vulkan_dir}/imgui-impl/imgui_impl_vulkan_vanilla.cpp" ] || \
   [ ! -f "${vulkan_dir}/imgui-impl/imgui_impl_vulkan_docking.cpp" ]; then
  echo "==> vulkan: vendor imgui_impl_vulkan_{vanilla,docking}.cpp into ${vulkan_dir}/imgui-impl/ first" >&2
  exit 1
fi
if [ ! -f "$vulkan_aliases" ]; then
  echo "==> vulkan: type-aliases JSON missing at $vulkan_aliases" >&2
  exit 1
fi

for vfl in vanilla docking; do
  v_subdir="${vulkan_dir}/flavor-${vfl}"
  v_json="dear_bindings/${vfl}/dcimgui_impl_vulkan.json"
  v_external="dear_bindings/${vfl}/dcimgui_nodefaultargfunctions.json"

  if [ ! -f "$v_json" ]; then
    echo "==> vulkan: ${vfl} input JSON missing at $v_json" >&2
    exit 1
  fi
  if [ ! -f "${v_subdir}/cbits/dcimgui_impl_vulkan.cpp" ] || \
     [ ! -f "${v_subdir}/cbits/imgui_impl_vulkan.h" ]; then
    echo "==> vulkan: ${vfl} cbits not vendored under ${v_subdir}/cbits/" >&2
    exit 1
  fi

  echo "==> vulkan/${vfl}: regenerating impl into ${v_subdir}/src"
  rm -rf "${v_subdir}/src" "${v_subdir}/cbits/DearImGuiWrappers.cpp" "${v_subdir}/cbits/DearImGuiWrappers.h"
  stack exec -- dear-bindings-ffi \
    --input "$v_json" \
    --module-root DearImGui.Raw.Impl.Vulkan \
    --header dcimgui_impl_vulkan.h \
    --external-types-module DearImGui.Raw.Types \
    --external-types-json "$v_external" \
    --type-aliases-json "$vulkan_aliases" \
    -o "${v_subdir}"
done

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
