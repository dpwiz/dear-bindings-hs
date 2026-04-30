module Test (checkUnification) where

import DearImGui.Raw.Impl.GLFW (cImGui_ImplGlfw_NewFrame)
import DearImGui.Raw.Impl.OpenGL3 (cImGui_ImplOpenGL3_RenderDrawData)
import DearImGui.Raw.Impl.SDL2 (cImGui_ImplSDL2_NewFrame)
import DearImGui.Raw.Impl.SDL3 (cImGui_ImplSDL3_NewFrame)
import DearImGui.Raw.Impl.Vulkan (cImGui_ImplVulkan_RenderDrawDataEx)
import DearImGui.Raw.Types (ImDrawData)
import Foreign.Ptr (Ptr)
import Vulkan.Core10 (Pipeline)
import Vulkan.Core10.Handles (CommandBuffer_T)

-- Compiles iff the same Ptr ImDrawData unifies across opengl3 and
-- vulkan impls and the chosen core. The glfw and sdl2 calls also
-- link-test those backends even though they don't share types here.
checkUnification :: Ptr ImDrawData -> Ptr CommandBuffer_T -> Pipeline -> IO ()
checkUnification p cb pl = do
  cImGui_ImplOpenGL3_RenderDrawData p
  cImGui_ImplVulkan_RenderDrawDataEx p cb pl
  cImGui_ImplGlfw_NewFrame
  cImGui_ImplSDL2_NewFrame
  cImGui_ImplSDL3_NewFrame
