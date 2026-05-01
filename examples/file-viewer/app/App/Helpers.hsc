{-# LANGUAGE CApiFFI #-}

-- Tiny C glue for things the generated FFI doesn't cover:
--   * io.ConfigFlags |= ImGuiConfigFlags_DockingEnable (no field
--     setter is generated for ImGuiIO)
--   * the three OpenGL entry points the OpenGL3 backend doesn't
--     link for us (it dlopens its own loader copy)
-- The DockBuilder API used to live here too via a small C++ shim;
-- it now comes from dear-imgui-raw-docking-internal directly.

module App.Helpers
  ( enableDocking
  , glViewport
  , glClearColor
  , glClear
  , glColorBufferBit
  ) where

import Data.Bits ((.|.))
import Foreign.C.Types (CFloat (..), CInt (..), CUInt (..))
import Foreign.Storable (peekByteOff, pokeByteOff)
import DearImGui.Raw.ImGui (imGui_GetIO)
import DearImGui.Raw.Types (ImGuiConfigFlags)

#include "dcimgui_nodefaultargfunctions.h"
#include <GL/gl.h>

enableDocking :: IO ()
enableDocking = do
  io <- imGui_GetIO
  cur <- (#peek ImGuiIO, ConfigFlags) io :: IO ImGuiConfigFlags
  (#poke ImGuiIO, ConfigFlags) io
    (cur .|. (#const ImGuiConfigFlags_DockingEnable))

foreign import capi unsafe "GL/gl.h glViewport"
  glViewport :: CInt -> CInt -> CInt -> CInt -> IO ()

foreign import capi unsafe "GL/gl.h glClearColor"
  glClearColor :: CFloat -> CFloat -> CFloat -> CFloat -> IO ()

foreign import capi unsafe "GL/gl.h glClear"
  glClear :: CUInt -> IO ()

glColorBufferBit :: CUInt
glColorBufferBit = #const GL_COLOR_BUFFER_BIT
