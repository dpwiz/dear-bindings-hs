{-# LANGUAGE CApiFFI #-}

-- imnodes-demo: a tiny SDL2 + OpenGL3 + docking-flavor consumer of
-- dear-imgui-raw-imnodes. Companion example to file-viewer (which
-- uses GLFW); the two together prove both platform impl backends
-- work end-to-end.
--
-- We use the high-level `sdl2` package for window creation and
-- lifecycle (initialize/quit), but drop down to `SDL.Raw` for the
-- GL context and the per-frame swap/event-pump because:
--   - the high-level `GLContext` newtype's constructor isn't
--     exported, so we can't get the underlying void* to pass to
--     ImGui_ImplSDL2_InitForOpenGL
--   - SDL.pollEvents decodes events into a high-level ADT and
--     loses the raw SDL_Event* that ImGui_ImplSDL2_ProcessEvent
--     wants

module Main (main) where

import App.Helpers (enableDocking, glClear, glClearColor, glColorBufferBit, glViewport)
import App.NodeEditor (EditorState, drawEditor, drawStatus, newEditorState)
import Control.Exception (bracket, bracket_)
import Control.Monad (unless, when)
import Data.Function (fix)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word32)
import DearImGui.Raw.ImGui qualified as ImGui
import DearImGui.Raw.Impl.OpenGL3 qualified as ImplGL3
import DearImGui.Raw.Impl.SDL2 qualified as ImplSDL2
import DearImGui.Raw.Impl.SDL2.Types (SDL_Event, SDL_Window)
import DearImGui.Raw.ImNodes qualified as Nodes
import DearImGui.Raw.Internal.ImGui qualified as ImGuiI
import DearImGui.Raw.Internal.Types (pattern ImGuiDockNodeFlags_DockSpace)
import DearImGui.Raw.Types (ImGuiID, ImVec2 (..), pattern ImGuiDir_Left)
import Foreign.C.String (withCString)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, peekByteOff)
import SDL qualified
import SDL.Internal.Types (Window (..))
import SDL.Raw qualified as SDLR
import System.Directory qualified as Dir
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  bracket_ (SDL.initialize [SDL.InitVideo, SDL.InitEvents]) SDL.quit do
    let cfg = SDL.defaultWindow
          { SDL.windowInitialSize     = SDL.V2 1280 720
          , SDL.windowResizable       = True
          , SDL.windowGraphicsContext = SDL.OpenGLContext glConfig
          , SDL.windowHighDPI         = True
          }
        glConfig = SDL.defaultOpenGL
          { SDL.glProfile = SDL.Core SDL.Normal 3 3
          }
    bracket (SDL.createWindow "imnodes demo" cfg) SDL.destroyWindow \win -> do
      let Window rawWin = win
          sdlWin        = castPtr rawWin :: Ptr SDL_Window
      bracket (SDLR.glCreateContext rawWin) SDLR.glDeleteContext \rawCtx -> do
        _ <- SDLR.glMakeCurrent rawWin rawCtx
        _ <- SDLR.glSetSwapInterval 1                    -- vsync; ignore failure on no-vsync drivers

        ctx <- ImGui.imGui_CreateContext nullPtr
        nodesCtx <- Nodes.imNodesCreateContext
        enableDocking
        ImGui.imGui_StyleColorsDark nullPtr
        Nodes.imNodesStyleColorsDarkEx nullPtr

        _ <- ImplSDL2.cImGui_ImplSDL2_InitForOpenGL sdlWin (castPtr rawCtx)
        initOk <- withCString "#version 130" ImplGL3.cImGui_ImplOpenGL3_InitEx
        case initOk of
          CBool 0 -> hPutStrLn stderr "ImGui_ImplOpenGL3_Init failed." >> exitFailure
          _       -> pure ()

        iniExists <- Dir.doesFileExist "imgui.ini"
        needsLayout <- newIORef (not iniExists)

        editor <- newEditorState
        runLoop rawWin needsLayout editor

        ImplGL3.cImGui_ImplOpenGL3_Shutdown
        ImplSDL2.cImGui_ImplSDL2_Shutdown
        Nodes.imNodesDestroyContextEx nodesCtx
        ImGui.imGui_DestroyContext ctx

runLoop :: SDLR.Window -> IORef Bool -> EditorState -> IO ()
runLoop rawWin needsLayout editor = do
  closeRef <- newIORef False
  fix \loop -> do
    pumpEvents closeRef
    close <- readIORef closeRef
    unless close do
      ImplGL3.cImGui_ImplOpenGL3_NewFrame
      ImplSDL2.cImGui_ImplSDL2_NewFrame
      ImGui.imGui_NewFrame

      drawUI rawWin needsLayout editor

      ImGui.imGui_Render
      (fbW, fbH) <- glDrawableSize rawWin
      glViewport 0 0 fbW fbH
      glClearColor 0.10 0.10 0.10 1.0
      glClear glColorBufferBit
      drawData <- ImGui.imGui_GetDrawData
      ImplGL3.cImGui_ImplOpenGL3_RenderDrawData drawData
      SDLR.glSwapWindow rawWin
      loop

drawUI :: SDLR.Window -> IORef Bool -> EditorState -> IO ()
drawUI rawWin needsLayout editor = do
  viewport <- ImGui.imGui_GetMainViewport
  dockId <- ImGui.imGui_DockSpaceOverViewport 0 viewport 0 nullPtr
  shouldSeed <- readIORef needsLayout
  when shouldSeed do
    (ww, wh) <- windowSize rawWin
    seedDefaultLayout dockId (fromIntegral ww) (fromIntegral wh)
    writeIORef needsLayout False
  drawEditor editor
  drawStatus editor

-- Tear down whatever's at @dockId@ and rebuild it as a 70/30
-- horizontal split with "Editor" docked on the left and "Status"
-- on the right. Run once per app launch (gated by an imgui.ini
-- absence check).
seedDefaultLayout :: ImGuiID -> Float -> Float -> IO ()
seedDefaultLayout dockId w h = do
  ImGuiI.imGui_DockBuilderRemoveNode dockId
  _ <- ImGuiI.imGui_DockBuilderAddNode dockId ImGuiDockNodeFlags_DockSpace
  ImGuiI.imGui_DockBuilderSetNodeSize dockId (ImVec2 (realToFrac w) (realToFrac h))
  alloca \leftP -> alloca \rightP -> do
    _ <- ImGuiI.imGui_DockBuilderSplitNode dockId ImGuiDir_Left 0.70 leftP rightP
    leftId  <- peek leftP
    rightId <- peek rightP
    withCString "Editor" \s -> ImGuiI.imGui_DockBuilderDockWindow s leftId
    withCString "Status" \s -> ImGuiI.imGui_DockBuilderDockWindow s rightId
  ImGuiI.imGui_DockBuilderFinish dockId

-- Drain SDL's event queue, forwarding every raw event to ImGui's
-- SDL2 backend so it can update mouse/keyboard state, then peek the
-- type field to spot the user closing the window. We bypass
-- SDL.pollEvents (which decodes events into the high-level Event
-- ADT and would lose the raw pointer ImGui needs).
pumpEvents :: IORef Bool -> IO ()
pumpEvents closeRef = allocaBytes sdlEventSize \evt -> fix \drain -> do
  n <- SDLR.pollEvent evt
  when (n /= 0) do
    _ <- ImplSDL2.cImGui_ImplSDL2_ProcessEvent (castPtr evt :: Ptr SDL_Event)
    ty <- peekByteOff evt 0 :: IO Word32
    when (ty == sdlQuitEvent) (writeIORef closeRef True)
    drain
  where
    -- SDL_Event is a union; SDL pads it to 56 bytes on x86_64. Use
    -- a 64-byte buffer for headroom — only the active variant's
    -- bytes are written by SDL.
    sdlEventSize :: Int
    sdlEventSize = 64

    -- SDL_QUIT == 0x100 in <SDL2/SDL_events.h>. SDL.Raw doesn't
    -- expose the constant in a way we can use without hsc2hs, so
    -- spell it out and document the source.
    sdlQuitEvent :: Word32
    sdlQuitEvent = 0x100

-- ----- raw SDL helpers -----------------------------------------------

windowSize :: SDLR.Window -> IO (CInt, CInt)
windowSize rawWin = alloca \wp -> alloca \hp -> do
  SDLR.getWindowSize rawWin wp hp
  (,) <$> peek wp <*> peek hp

glDrawableSize :: SDLR.Window -> IO (CInt, CInt)
glDrawableSize rawWin = alloca \wp -> alloca \hp -> do
  SDLR.glGetDrawableSize rawWin wp hp
  (,) <$> peek wp <*> peek hp
