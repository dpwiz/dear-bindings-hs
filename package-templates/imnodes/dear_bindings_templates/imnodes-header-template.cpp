#include "%IMGUI_INCLUDE_DIR%imgui.h"
#include "imnodes.h"

// Glue file consumed by dear_bindings at imnodes-binding generation
// time. The cimgui namespace wraps the C-shape header so the
// generator can emit type aliases without colliding with the C++
// API; pulling dcimgui.h into the same namespace promotes ImVec2
// and friends from forward-declared (the dcimnodes.h default) to
// fully defined, which dcimnodes.cpp needs for its by-value
// argument/return shims.
#define DEAR_BINDINGS_INTERNAL_GLUE_CODE
namespace cimgui
{
#include "dcimgui.h"
#include "%OUTPUT_HEADER_NAME%"
}
#undef DEAR_BINDINGS_INTERNAL_GLUE_CODE
