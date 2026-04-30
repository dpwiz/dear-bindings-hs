#!/usr/bin/env bash
# Refresh generated-in/ from upstream submodules.
#
# Mirrors imgui sources from upstream/imgui-{vanilla,docking}/ and runs
# upstream/dear_bindings/dear_bindings.py to regenerate the dcimgui* shim
# family (cpp/h/json) that downstream tooling consumes.
#
# Inputs:  upstream/imgui-{vanilla,docking}/ (library, tag-pinned)
#          upstream/dear_bindings/           (tool, tracks main)
# Outputs: generated-in/{vanilla,docking}/   (per-flavor core + vulkan)
#          generated-in/backends/            (flavor-neutral backends)
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
SPLIT_BACKENDS=(glfw opengl3 vulkan)

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

echo "==> generated-in/ refreshed."
echo "    Next: scripts/generate-all-ffi.sh to rebuild Haskell packages."
