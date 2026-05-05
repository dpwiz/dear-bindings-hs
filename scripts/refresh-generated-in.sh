#!/usr/bin/env bash
# Refresh generated-in/ from upstream submodules.
#
# Mirrors imgui sources from upstream/imgui-{vanilla,docking}/ and runs
# upstream/dear_bindings/dear_bindings.py to regenerate the dcimgui* shim
# family (cpp/h/json) that downstream tooling consumes.
#
# Inputs:  upstream/imgui-{vanilla,docking}/ (library, tag-pinned)
#          upstream/imnodes/                 (extension library, hash-pinned)
#          upstream/dear_bindings/           (tool, tracks main)
# Outputs: generated-in/{vanilla,docking}/   (per-flavor core + vulkan)
#          generated-in/backends/            (flavor-neutral backends)
#          generated-in/imnodes/             (flavor-neutral extension)
#
# Prerequisites:
#   git submodule update --init --recursive
#   pip install -r upstream/dear_bindings/requirements.txt   # ply==3.11
#
# Usage:
#   scripts/refresh-generated-in.sh
#
# After this completes, run scripts/generate-all-ffi.sh to rebuild the
# Haskell FFI packages from the refreshed inputs.

set -euo pipefail

REPO="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO"

PYTHON="${PYTHON:-python3}"
DB="$REPO/upstream/dear_bindings/dear_bindings.py"

# ----- preflight -----

if [ ! -f "$DB" ]; then
  echo "!! $DB not found — run: git submodule update --init --recursive" >&2
  exit 1
fi

if ! "$PYTHON" -c "import ply" >/dev/null 2>&1; then
  echo "!! python module 'ply' missing. Install via:" >&2
  echo "     pip install -r upstream/dear_bindings/requirements.txt" >&2
  exit 1
fi

# Linux-buildable + Windows-only backend list. metal/osx are .mm
# (Objective-C), which dear_bindings can't process — skipped.
BACKENDS=(
  allegro5 android
  dx9 dx10 dx11 dx12
  glfw glut null
  opengl2 opengl3
  sdl2 sdl3 sdlgpu3 sdlrenderer2 sdlrenderer3
  vulkan
  wgpu win32
)

# Backends whose imgui-side cpp diverges between master (vanilla) and
# docking branches — vendor a per-flavor pair (`_vanilla.cpp` +
# `_docking.cpp`). Everything else is identical between branches and
# vendored as a single shared cpp.
SPLIT_BACKENDS=(glfw opengl3 sdl2 vulkan)

# Extra files to vendor alongside specific backends (loader headers,
# pre-compiled shader blobs, etc.). Keep this in sync with
# scripts/generate-all-ffi.sh's FLAVOR_NEUTRAL_BACKENDS extras column.
backend_extras() {
  case "$1" in
    opengl3) printf '%s\n' imgui_impl_opengl3_loader.h ;;
    sdlgpu3) printf '%s\n' imgui_impl_sdlgpu3_shaders.h ;;
  esac
}

is_split_backend() {
  local b="$1"
  for s in "${SPLIT_BACKENDS[@]}"; do
    [ "$s" = "$b" ] && return 0
  done
  return 1
}

# ----- 1. mirror imgui core sources per flavor -----

CORE_FILES=(
  imconfig.h imgui.cpp imgui.h imgui_demo.cpp imgui_draw.cpp
  imgui_internal.h imgui_tables.cpp imgui_widgets.cpp
  imstb_rectpack.h imstb_textedit.h imstb_truetype.h
)

for flavor in vanilla docking; do
  src="upstream/imgui-${flavor}"
  dst="generated-in/${flavor}/imgui"
  echo "==> ${flavor}: mirroring imgui core -> ${dst}"
  rm -rf "$dst"
  mkdir -p "$dst"
  for f in "${CORE_FILES[@]}"; do
    cp "$src/$f" "$dst/$f"
  done
done

# ----- 2. run dear_bindings on each flavor's core -----
#
# Only the no-default-args variant is generated. The default-args
# variant adds C-side helper functions for default argument values
# (e.g. cImGui_Begin alongside cImGui_BeginEx); Haskell exposes
# defaults at its own level, so those helpers are unused weight.
# Backends include "dcimgui.h" — generate-all-ffi.sh satisfies that
# include by copying dcimgui_nodefaultargfunctions.h under both
# names into the core's cbits/.

for flavor in vanilla docking; do
  src="upstream/imgui-${flavor}"
  dst="generated-in/${flavor}"

  # Primary FFI input.
  echo "==> ${flavor}: dear_bindings imgui.h --nogeneratedefaultargfunctions -> dcimgui_nodefaultargfunctions"
  "$PYTHON" "$DB" \
    --nogeneratedefaultargfunctions \
    -o "$dst/dcimgui_nodefaultargfunctions" \
    "$src/imgui.h"

  # Internal header (no-default-args). Pairs with the above for the
  # HTML doc generator.
  echo "==> ${flavor}: dear_bindings imgui_internal.h --nogeneratedefaultargfunctions -> dcimgui_nodefaultargfunctions_internal"
  "$PYTHON" "$DB" \
    --nogeneratedefaultargfunctions \
    --include "$src/imgui.h" \
    -o "$dst/dcimgui_nodefaultargfunctions_internal" \
    "$src/imgui_internal.h"
done

# ----- 3. flavor-specific vulkan backend (per-flavor JSON + header) -----

for flavor in vanilla docking; do
  src="upstream/imgui-${flavor}"
  dst="generated-in/${flavor}"

  echo "==> ${flavor}: dear_bindings imgui_impl_vulkan.h -> dcimgui_impl_vulkan"
  "$PYTHON" "$DB" \
    --backend \
    --include "$src/imgui.h" \
    --imconfig-path "$src/imconfig.h" \
    -o "$dst/dcimgui_impl_vulkan" \
    "$src/backends/imgui_impl_vulkan.h"

  cp "$src/backends/imgui_impl_vulkan.h" "$dst/imgui_impl_vulkan.h"
done

# ----- 4. mirror imgui-side backend sources to generated-in/backends/ -----

mkdir -p generated-in/backends

for backend in "${BACKENDS[@]}"; do
  echo "==> ${backend}: mirroring imgui-side backend sources"

  # Header: identical between branches; pull from vanilla.
  cp "upstream/imgui-vanilla/backends/imgui_impl_${backend}.h" \
     "generated-in/backends/imgui_impl_${backend}.h"

  if is_split_backend "$backend"; then
    cp "upstream/imgui-vanilla/backends/imgui_impl_${backend}.cpp" \
       "generated-in/backends/imgui_impl_${backend}_vanilla.cpp"
    cp "upstream/imgui-docking/backends/imgui_impl_${backend}.cpp" \
       "generated-in/backends/imgui_impl_${backend}_docking.cpp"
  else
    # Single shared cpp from vanilla.
    cp "upstream/imgui-vanilla/backends/imgui_impl_${backend}.cpp" \
       "generated-in/backends/imgui_impl_${backend}.cpp"
  fi

  # Per-backend extras (loaders, shader blobs).
  while IFS= read -r extra; do
    [ -n "$extra" ] || continue
    cp "upstream/imgui-vanilla/backends/${extra}" \
       "generated-in/backends/${extra}"
  done < <(backend_extras "$backend")
done

# ----- 5. run dear_bindings on each flavor-neutral backend header -----
# Vulkan was handled per-flavor in step 3; skip it here.

for backend in "${BACKENDS[@]}"; do
  [ "$backend" = "vulkan" ] && continue

  src="upstream/imgui-vanilla"
  echo "==> ${backend}: dear_bindings imgui_impl_${backend}.h -> dcimgui_impl_${backend}"
  "$PYTHON" "$DB" \
    --backend \
    --include "$src/imgui.h" \
    --imconfig-path "$src/imconfig.h" \
    -o "generated-in/backends/dcimgui_impl_${backend}" \
    "$src/backends/imgui_impl_${backend}.h"
done

# vulkan_type_aliases.json is hand-maintained, not generated. Leave it
# alone if present.

# ----- 6. imnodes extension (Nelarius/imnodes) -----
#
# imnodes is a node-editor library that targets ImGui's public API,
# not a backend. It's flavor-neutral: vanilla and docking both link
# the same dcimnodes.{cpp,h}, the binding just needs a flavor-pinned
# core dependency to provide the underlying ImGui types/symbols.
#
# Three patches are applied that wouldn't be needed for plain ImGui
# headers; each is isolated and explained at its sed/cp site below:
#   (a) namespace macro expansion before parse
#   (b) C++ <imgui.h> -> C-shim "dcimgui.h" in the public C header
#   (c) field-name repair for nested-struct-with-same-name-field
#       (a known dear_bindings limitation in imnodes' ImNodesIO)

imnodes_src="upstream/imnodes"
imnodes_dst="generated-in/imnodes"
imnodes_tmpl="package-templates/imnodes/dear_bindings_templates"

if [ -d "$imnodes_src" ]; then
  echo "==> imnodes: refreshing $imnodes_dst"
  rm -rf "$imnodes_dst"
  mkdir -p "$imnodes_dst"

  # (a) dear_bindings doesn't expand macros, so `namespace IMNODES_NAMESPACE
  # { ... }` would land in the DOM as a literal namespace named
  # "IMNODES_NAMESPACE". Pre-substitute the macro with its default
  # expansion (ImNodes) and switch the system-style include to a quoted
  # one so the parser resolves it via -t/template-relative paths.
  sed -e 's|^#include <imgui.h>$|#include "imgui.h"|' \
      -e 's|IMNODES_NAMESPACE|ImNodes|g' \
      "$imnodes_src/imnodes.h" \
      > "$imnodes_dst/imnodes.h"

  echo "==> imnodes: dear_bindings imnodes.h -> dcimnodes"
  "$PYTHON" "$DB" \
    --include upstream/imgui-vanilla/imgui.h \
    --imconfig-path upstream/imgui-vanilla/imconfig.h \
    -t "$imnodes_tmpl" \
    -o "$imnodes_dst/dcimnodes" \
    "$imnodes_dst/imnodes.h"

  # (b) dcimnodes.h is consumed both by the C++ wrapper (under the
  # cimgui namespace via the template glue, where the C++ imgui.h is
  # already in scope) and by the Haskell hsc2hs preprocessor (which is
  # plain C). Rewriting "imgui.h" to the C-flavored "dcimgui.h" lets
  # the C consumer see ImVec2/ImGuiContext/etc. without dragging in
  # C++. Mirrors the same patch applied to dcimgui_internal in the
  # internal-binding step above.
  sed -i 's|^#include "imgui.h"$|#include "dcimgui.h"|' \
    "$imnodes_dst/dcimnodes.h"

  # (c) imnodes' ImNodesIO has three nested structs whose field names
  # match the type names (struct EmulateThreeButtonMouse { ... }
  # EmulateThreeButtonMouse;). dear_bindings flattens the inner type
  # to ImNodesIO_EmulateThreeButtonMouse but emits the parent member
  # without a field name, producing a compile error. Re-attach the
  # field names explicitly. If upstream dear_bindings ever fixes this,
  # the sed becomes a no-op.
  sed -i \
    -e 's|^    EmulateThreeButtonMouse$|    ImNodesIO_EmulateThreeButtonMouse EmulateThreeButtonMouse;|' \
    -e 's|^    LinkDetachWithModifierClick$|    ImNodesIO_LinkDetachWithModifierClick LinkDetachWithModifierClick;|' \
    -e 's|^    MultipleSelectModifier$|    ImNodesIO_MultipleSelectModifier MultipleSelectModifier;|' \
    "$imnodes_dst/dcimnodes.h"

  # Drop the side-channel JSONs we don't consume (imgui/imconfig
  # snapshots emitted alongside dcimnodes.json). Keeping
  # generated-in/imnodes/ minimal makes downstream wiring obvious.
  rm -f "$imnodes_dst"/dcimnodes_imgui.json \
        "$imnodes_dst"/dcimnodes_imconfig.json
else
  echo "!! imnodes: $imnodes_src missing — run: git submodule update --init upstream/imnodes" >&2
fi

echo "==> generated-in/ refreshed."
echo "    Next: scripts/generate-all-ffi.sh to rebuild Haskell packages."
