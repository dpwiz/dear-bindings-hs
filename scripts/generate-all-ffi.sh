#!/usr/bin/env bash
# Wipe-and-rebuild every FFI binding package under generated-out/ from the
# committed inputs in generated-in/ + scaffolding in package-templates/.
# This is the "fresh checkout, get me to a buildable state" entry point.
#
# Invariant: rm -rf generated-out/ && scripts/generate-all-ffi.sh leaves
# generated-out/ in a state where every package builds.
#
# Usage:
#   scripts/generate-all-ffi.sh                 -- both vanilla + docking
#   scripts/generate-all-ffi.sh vanilla         -- just vanilla core + backends
#   scripts/generate-all-ffi.sh docking         -- just docking core + backends
#
# What runs each invocation:
#   1. (re)build dear-bindings-ffi
#   2. wipe generated-out/
#   3. for each requested flavor: scaffold core package (template + cbits +
#      FFI gen)
#   4. scaffold each flavor-neutral backend whose imgui-side cbits are
#      vendored. Backends listed under FLAVOR_NEUTRAL_BACKENDS that lack
#      vendored imgui_impl_*.cpp variants are skipped with a warning;
#      drop the variants into generated-in/backends/ to light them up.
#   5. scaffold the flavor-specific vulkan backend (per-flavor sub-trees).

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

echo "==> Building dear-bindings-ffi"
stack build --flag dear-bindings-aeson:executables >/dev/null

echo "==> Wiping generated-out/"
rm -rf generated-out
mkdir -p generated-out

# external-types reference for impl-mode generation. Vanilla and docking
# core JSONs declare the same set of struct/typedef/enum names (only the
# function lists and field counts differ), so either works as the
# "external names" reference. Use vanilla.
external_core_json="generated-in/vanilla/dcimgui_nodefaultargfunctions.json"

# ----- core flavors -----
for flavor in "${flavors[@]}"; do
  core_dir="generated-out/${flavor}/dear-imgui-raw-${flavor}"
  core_template="package-templates/${flavor}/dear-imgui-raw-${flavor}"
  core_input="generated-in/${flavor}"
  core_json="${core_input}/dcimgui_nodefaultargfunctions.json"

  echo "==> ${flavor}: scaffolding ${core_dir}"
  mkdir -p "${core_dir}/cbits"
  cp -r "${core_template}/." "${core_dir}/"
  cp "${core_input}/dcimgui_nodefaultargfunctions.cpp" "${core_dir}/cbits/"
  cp "${core_input}/dcimgui_nodefaultargfunctions.h"   "${core_dir}/cbits/"
  # Backend impl headers (dcimgui_impl_<b>.h) #include "dcimgui.h"
  # verbatim — that's the dear_bindings emitter's hardcoded filename.
  # We don't generate the default-args variant; the no-default-args
  # header has the same type definitions (only function-list differs)
  # so it satisfies the include cleanly.
  cp "${core_input}/dcimgui_nodefaultargfunctions.h"   "${core_dir}/cbits/dcimgui.h"
  cp -r "${core_input}/imgui"                          "${core_dir}/cbits/imgui"

  echo "==> ${flavor}: FFI generation -> ${core_dir}/src"
  stack exec -- dear-bindings-ffi \
    --input "$core_json" \
    --module-root DearImGui.Raw \
    -o "${core_dir}"
done

# ----- flavor-neutral backends -----
#
# Per-backend metadata: short-name | hpack module suffix | extra cbits files
# (whitespace-separated, "" for none). Templates must exist at
# package-templates/backends/dear-imgui-raw-impl-<short-name>/.
# Linux-buildable backends only — Windows-only (dx9-12, win32) and the
# more involved ones (wgpu, glut, null, android) are intentionally
# omitted; add them when their cbits + system deps are sorted.
FLAVOR_NEUTRAL_BACKENDS=(
  "glfw|GLFW|"
  "opengl3|OpenGL3|imgui_impl_opengl3_loader.h"
  "opengl2|OpenGL2|"
  "sdl2|SDL2|"
  "sdl3|SDL3|"
  "sdlrenderer2|SDLRenderer2|"
  "sdlrenderer3|SDLRenderer3|"
  "sdlgpu3|SDLGPU3|imgui_impl_sdlgpu3_shaders.h"
  "allegro5|Allegro5|"
)

scaffold_neutral_backend() {
  # $1 = backend short name (e.g. glfw)
  # $2 = module suffix (e.g. GLFW)
  # rest = extra cbits files
  local backend="$1"
  local module_suffix="$2"
  shift 2
  local extras=("$@")

  local pkg="dear-imgui-raw-impl-${backend}"
  local out="generated-out/backends/${pkg}"
  local tmpl="package-templates/backends/${pkg}"
  local in="generated-in/backends"

  if [ ! -d "$tmpl" ]; then
    echo "!! ${backend}: template missing at ${tmpl}; skipping"
    return
  fi

  # Cbits gate: imgui-side cpp/h come from the upstream imgui repo,
  # not dear_bindings, so they're vendored separately into
  # generated-in/backends/. Two layouts:
  #   - split: imgui_impl_<b>_{vanilla,docking}.cpp + imgui_impl_<b>.h
  #     (used by glfw/opengl3 because the multi-viewport/docking diff
  #     between branches is large enough to keep them separate)
  #   - single: imgui_impl_<b>.cpp + imgui_impl_<b>.h (used by every
  #     other backend, where the file is identical across flavors)
  local pattern
  if [ -f "${in}/imgui_impl_${backend}_vanilla.cpp" ] \
     && [ -f "${in}/imgui_impl_${backend}_docking.cpp" ]; then
    pattern="split"
  elif [ -f "${in}/imgui_impl_${backend}.cpp" ]; then
    pattern="single"
  else
    echo "!! ${backend}: skipping — no imgui_impl_${backend}.cpp under ${in}/"
    return
  fi
  if [ ! -f "${in}/imgui_impl_${backend}.h" ]; then
    echo "!! ${backend}: skipping — missing ${in}/imgui_impl_${backend}.h"
    return
  fi

  echo "==> ${backend}: scaffolding ${out} (${pattern})"
  mkdir -p "${out}/cbits"
  cp -r "${tmpl}/." "${out}/"
  cp "${in}/dcimgui_impl_${backend}.cpp" "${out}/cbits/"
  cp "${in}/dcimgui_impl_${backend}.h"   "${out}/cbits/"
  cp "${in}/imgui_impl_${backend}.h"     "${out}/cbits/"

  # Workaround for an upstream dear_bindings quirk: it emits lines of
  # the form `typedef struct _Foo _Foo;` for opaque externs whose tag
  # uses an underscore-uppercase name (a C++ reserved-identifier shape
  # that SDL2 et al. use, e.g. _SDL_GameController). When the wrapped
  # header is re-#include'd inside `namespace cimgui` from the .cpp,
  # transitive system headers from imgui_impl_<backend>.cpp have
  # already declared the same tag in the global namespace; the
  # cimgui-scoped typedef then aliases to that global tag instead of
  # introducing a fresh one, and subsequent `struct cimgui::_Foo` uses
  # fail with "using typedef-name after 'struct'". Replacing the
  # typedef with a bare forward declaration keeps the cimgui-scoped
  # tag without the typedef alias, which is all the rest of the
  # generated code actually uses.
  sed -i -E 's|^typedef struct (_[A-Za-z_][A-Za-z0-9_]*) \1;$|struct \1;|' \
    "${out}/cbits/dcimgui_impl_${backend}.h"
  case "$pattern" in
    split)
      cp "${in}/imgui_impl_${backend}_vanilla.cpp" "${out}/cbits/"
      cp "${in}/imgui_impl_${backend}_docking.cpp" "${out}/cbits/"
      ;;
    single)
      cp "${in}/imgui_impl_${backend}.cpp" "${out}/cbits/"
      ;;
  esac
  for extra in "${extras[@]}"; do
    [ -n "$extra" ] && cp "${in}/${extra}" "${out}/cbits/"
  done

  echo "==> ${backend}: FFI generation -> ${out}/src"
  stack exec -- dear-bindings-ffi \
    --input "${in}/dcimgui_impl_${backend}.json" \
    --module-root "DearImGui.Raw.Impl.${module_suffix}" \
    --header "dcimgui_impl_${backend}.h" \
    --external-types-module DearImGui.Raw.Types \
    --external-types-json "$external_core_json" \
    -o "${out}"
}

for entry in "${FLAVOR_NEUTRAL_BACKENDS[@]}"; do
  IFS='|' read -r backend module_suffix extras_str <<< "$entry"
  # shellcheck disable=SC2206
  extras=($extras_str)
  scaffold_neutral_backend "$backend" "$module_suffix" "${extras[@]}"
done

# ----- flavor-specific backend (vulkan) -----
vulkan_pkg="dear-imgui-raw-impl-vulkan"
vulkan_out="generated-out/backends/${vulkan_pkg}"
vulkan_tmpl="package-templates/backends/${vulkan_pkg}"
vulkan_aliases="generated-in/backends/vulkan_type_aliases.json"

echo "==> vulkan: scaffolding ${vulkan_out}"
mkdir -p "${vulkan_out}/imgui-impl"
cp -r "${vulkan_tmpl}/." "${vulkan_out}/"
cp "generated-in/backends/imgui_impl_vulkan_vanilla.cpp" "${vulkan_out}/imgui-impl/"
cp "generated-in/backends/imgui_impl_vulkan_docking.cpp" "${vulkan_out}/imgui-impl/"

for vfl in vanilla docking; do
  v_subdir="${vulkan_out}/flavor-${vfl}"
  v_in="generated-in/${vfl}"

  mkdir -p "${v_subdir}/cbits"
  cp "${v_in}/dcimgui_impl_vulkan.cpp" "${v_subdir}/cbits/"
  cp "${v_in}/dcimgui_impl_vulkan.h"   "${v_subdir}/cbits/"
  cp "${v_in}/imgui_impl_vulkan.h"     "${v_subdir}/cbits/"

  echo "==> vulkan/${vfl}: FFI generation -> ${v_subdir}/src"
  stack exec -- dear-bindings-ffi \
    --input "${v_in}/dcimgui_impl_vulkan.json" \
    --module-root DearImGui.Raw.Impl.Vulkan \
    --header dcimgui_impl_vulkan.h \
    --external-types-module DearImGui.Raw.Types \
    --external-types-json "${v_in}/dcimgui_nodefaultargfunctions.json" \
    --type-aliases-json "$vulkan_aliases" \
    -o "${v_subdir}"
done

echo "==> generated-out/ rebuilt from scratch."
