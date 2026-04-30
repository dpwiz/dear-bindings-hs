# dear-bindings-aeson

Toolchain that ingests upstream [dear_bindings] JSON descriptions of
Dear ImGui and produces:

- **Haskell FFI binding packages** (raw `Foreign.Ptr` layer, one core
  package per imgui flavor + one package per backend)
- **Browsable HTML documentation** rendered from the same JSON

## Directory layout

### Hand-rolled source (this repo)

| path | what |
| ---- | ---- |
| `src/DearBindings/` | shared library: JSON parser, catalog, slicing |
| `app/` | documentation generator binary (HTML output) |
| `app-ffi/` | FFI binding generator binary (`dear-bindings-ffi`) |
| `test/` | hspec suite for the parser / catalog |
| `scripts/` | tooling: regenerate, format, clean |
| `package.yaml`, `stack.yaml` | this repo's build config |

### Inputs — committed upstream artifacts

| path | what | status |
| ---- | ---- | ---- |
| `generated-in/{vanilla,docking}/` | per-flavor `dcimgui*.{cpp,h,json}`, vendored `imgui/` tree, flavor-specific vulkan headers | committed |
| `generated-in/backends/` | flavor-neutral `dcimgui_impl_*.{cpp,h,json}` and the imgui-side `imgui_impl_*.{cpp,h}`. Two layouts: per-flavor pairs (`_vanilla.cpp` + `_docking.cpp`) for backends whose imgui-side source diverges across flavors (glfw, opengl3, vulkan); single shared `.cpp` for everything else. Plus `vulkan_type_aliases.json`. | committed |

### Scaffolding — committed hand-rolled package skeletons

| path | what |
| ---- | ---- |
| `package-templates/{vanilla,docking,backends}/<package>/` | `package.yaml`, `stack.yaml` for each output package; copied verbatim into `generated-out/` during regen |

### Outputs — produced by this repo's generators

| path | producer | what |
| ---- | -------- | ---- |
| `output/{vanilla,docking}/` | doc generator (`stack run`) | static HTML guides — gitignored |
| `generated-out/{vanilla,docking}/dear-imgui-raw-<flavor>/` | `scripts/generate-all-ffi.sh` | per-flavor **core** binding package — gitignored |
| `generated-out/backends/dear-imgui-raw-impl-<backend>/` | `scripts/generate-all-ffi.sh` | **backend** binding package — gitignored |

`generated-out/` is fully derived: `rm -rf generated-out/ && scripts/generate-all-ffi.sh`
restores every package to a buildable state from `generated-in/` + `package-templates/`.

### Verification — hand-rolled, not shipped

| path | what |
| ---- | ---- |
| `test-ffi-scaffold/{vanilla,docking}/` | tiny stack consumers that depend on a core + the buildable impl backends (glfw, opengl3, vulkan, sdl2, sdl3) and type-check a function exercising cross-package type unification. Not a deliverable; exists so `dogfood-ffi.sh` can prove the architecture compiles end-to-end. |

### Transient

`tmp/`, `.stack-work/`, `dist-newstyle/`, `dist-ffi/` — all
gitignored build / scratch state.

## Workflow

```
# refresh generated-in/ from upstream submodules (imgui-{vanilla,docking}
# + dear_bindings). Run when bumping either upstream pin.
git submodule update --init --recursive
pip install -r upstream/dear_bindings/requirements.txt   # ply==3.11
scripts/refresh-generated-in.sh

# wipe-and-rebuild every FFI package from generated-in/ + package-templates/
scripts/generate-all-ffi.sh          # both flavors
scripts/generate-all-ffi.sh vanilla  # one flavor only

# fast iteration: regenerate src/ and rebuild + run consumer tests.
# Auto-invokes generate-all-ffi.sh if generated-out/ has been wiped.
scripts/dogfood-ffi.sh

# clean slate
scripts/clean-ffi.sh

# regenerate HTML docs from generated-in/
scripts/generate-all.sh
```

## Backend coverage

`scripts/generate-all-ffi.sh` scaffolds every backend that has a template
under `package-templates/backends/` and emits Haskell `.hsc` for each.
Whether the resulting C++ links against the vendored imgui core is a
separate concern — current state:

A typical app pairs **one platform** backend (windowing + input) with
**one renderer** backend (draw-call submission). `allegro5` is the
only combined backend.

| backend | role | builds end-to-end | notes |
| ---- | ---- | ---- | ---- |
| glfw | platform | yes | per-flavor cpp variants vendored |
| sdl2 | platform | yes | leading-underscore typedef worked around in `scripts/generate-all-ffi.sh` |
| sdl3 | platform | yes | self-ref typedef lifted to opaque struct in catalog layer |
| opengl3 | renderer | yes | per-flavor cpp variants + `_loader.h` |
| vulkan | renderer | yes | per-flavor sub-trees, `vulkan_type_aliases.json` |
| opengl2 | renderer | no | imgui-side cpp newer than v1.92.7 core (uses `platform_io.DrawCallback_*`); needs imgui bump |
| sdlrenderer2 | renderer | no | same imgui version skew as opengl2 |
| sdlrenderer3 | renderer | no | same imgui version skew as opengl2 |
| sdlgpu3 | renderer | no | same imgui version skew as opengl2 |
| allegro5 | both | n/a here | needs `liballegro-5-dev` + `liballegro-main-5-dev` system libs |

Windows-only backends (dx9–12 [renderer], win32 [platform]) and the
more involved ones (wgpu [renderer], glut [platform], null [renderer],
android [platform], metal [renderer], osx [platform]) have
`dcimgui_impl_*` JSON in `generated-in/backends/` but no template —
add them when their system deps + cbits are sorted.

[dear_bindings]: https://github.com/dearimgui/dear_bindings
