#!/usr/bin/env bash
# Render the dear_bindings JSON catalog (committed under generated-in/)
# into ./output/{vanilla,docking}/ as browsable HTML. Intended to run on CI.
#
# Re-running with no upstream changes is cheap: each variant directory
# carries a `.fingerprint` sentinel summarising the inputs + flags it was
# built from; if the fingerprint matches and every input is older than
# the sentinel, the variant is left untouched.
#
# Environment overrides:
#   OUTPUT_DIR             target directory      (default: ./output)
#   BASE_PATH              URL prefix for hosted pages
#                                                (default: empty → relative)
#                            e.g. /dear-bindings-aeson/  → vanilla URLs become
#                            /dear-bindings-aeson/vanilla/..., docking likewise.
#   FORMAT                 pandoc writer name    (default: html5)
#   FORCE                  1 → ignore cache, regenerate every variant.
#                                                (default: 0)

set -euo pipefail

OUTPUT_DIR="${OUTPUT_DIR:-./output}"
BASE_PATH="${BASE_PATH:-}"
FORMAT="${FORMAT:-html5}"
FORCE="${FORCE:-0}"

cd "$(dirname "$0")/.."

echo "==> Building dear-bindings-doc"
stack build dear-bindings-aeson:exe:dear-bindings-doc

# Path to the freshly built binary, plus a content hash. We use the
# hash (not mtime) in the fingerprint because `stack build` re-touches
# the installed binary on every run, which would otherwise bust the
# cache after a no-op rebuild.
exe_path="$(stack path --local-install-root)/bin/dear-bindings-doc"
exe_hash="$(sha256sum "$exe_path" | cut -d' ' -f1)"

mkdir -p "$OUTPUT_DIR"

core_glob='dcimgui_nodefaultargfunctions.json dcimgui_nodefaultargfunctions_internal.json'

for variant in vanilla docking; do
  src_dir="generated-in/$variant"
  dst_dir="$OUTPUT_DIR/$variant"

  if ! compgen -G "$src_dir/*.json" > /dev/null; then
    echo "!!  $src_dir has no JSON files; skipping"
    continue
  fi

  # Pick exactly one of the two flavours of the core API, plus every
  # backend file (dcimgui_impl_*.json). Non-vulkan backend JSONs are
  # flavor-neutral and live under generated-in/backends/; vulkan is
  # flavor-specific and lives under $src_dir.
  inputs=()
  for f in $core_glob; do
    [ -f "$src_dir/$f" ] && inputs+=("$src_dir/$f")
  done
  for f in generated-in/backends/dcimgui_impl_*.json; do
    case "$f" in
      *_imconfig.json|*_imgui.json) continue ;;
    esac
    [ -f "$f" ] && inputs+=("$f")
  done
  [ -f "$src_dir/dcimgui_impl_vulkan.json" ] && inputs+=("$src_dir/dcimgui_impl_vulkan.json")

  if [ ${#inputs[@]} -eq 0 ]; then
    echo "!!  $src_dir matched no expected JSON files; skipping"
    continue
  fi

  args=(generate "${inputs[@]}" -o "$dst_dir" -t "$FORMAT")
  if [ -n "$BASE_PATH" ]; then
    # ensure exactly one trailing slash on each side of the join
    prefix="${BASE_PATH%/}"
    args+=(--base-path "$prefix/$variant/")
  fi

  # Cache key: anything that materially changes the output. The binary
  # is identified by content hash (not mtime) because `stack build`
  # re-touches its installed copy on every run. Inputs are sorted so
  # reorderings of the file glob don't bust the cache.
  sentinel="$dst_dir/.fingerprint"
  fingerprint=$(printf '%s\n' \
    "format=$FORMAT" \
    "base=$BASE_PATH" \
    "exe=$exe_hash" \
    "inputs=$(printf '%s\n' "${inputs[@]}" | sort)")

  if [ "$FORCE" != "1" ] \
     && [ -d "$dst_dir" ] \
     && [ -f "$sentinel" ] \
     && [ "$(cat "$sentinel")" = "$fingerprint" ] \
     && [ -z "$(find "${inputs[@]}" -newer "$sentinel" 2>/dev/null)" ]; then
    echo "==> $variant up to date, skipping (use FORCE=1 to override)"
    continue
  fi

  echo "==> Generating $variant -> $dst_dir  (${#inputs[@]} files)"
  rm -rf "$dst_dir"
  stack exec dear-bindings-doc -- "${args[@]}"
  printf '%s' "$fingerprint" > "$sentinel"
done

# Tiny landing page that links to both variants. Static HTML; no pandoc.
cat > "$OUTPUT_DIR/index.html" <<EOF
<!DOCTYPE html>
<html lang="en">
<meta charset="utf-8">
<title>dear-imgui API browser</title>
<style>
  body { font-family: system-ui, sans-serif; max-width: 120ex; margin: 4em auto; padding: 0 1em; }
  a { color: #0366d6; }
</style>
<h1>dear-imgui API browser</h1>
<p>Generated from <a href="https://github.com/dearimgui/dear_bindings">dear_bindings</a> JSON.</p>
<ul>
  <li><a href="${BASE_PATH%/}${BASE_PATH:+/}vanilla/index.${FORMAT/html5/html}">vanilla</a></li>
  <li><a href="${BASE_PATH%/}${BASE_PATH:+/}docking/index.${FORMAT/html5/html}">docking</a></li>
</ul>
EOF

echo "==> Done. Open $OUTPUT_DIR/index.html"
