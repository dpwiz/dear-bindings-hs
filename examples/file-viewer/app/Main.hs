{-# LANGUAGE CApiFFI #-}

module Main (main) where

import App.FileBrowser
  ( BrowserState (..)
  , Entry (..)
  , Preview (..)
  , binaryPreviewLimit
  , goInto
  , goUp
  , listEntries
  , loadPreview
  , newBrowser
  )
import App.Helpers
  ( enableDocking
  , glClear
  , glClearColor
  , glColorBufferBit
  , glViewport
  )
import Control.Exception (bracket_)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (for_, traverse_)
import Data.Function (fix)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextE
import Data.Word (Word8)
import DearImGui.Raw.ImGui qualified as ImGui
import DearImGui.Raw.Impl.GLFW qualified as ImplGlfw
import DearImGui.Raw.Impl.GLFW.Types (GLFWwindow)
import DearImGui.Raw.Impl.OpenGL3 qualified as ImplGL3
import DearImGui.Raw.Internal.ImGui qualified as ImGuiI
import DearImGui.Raw.Internal.Types (pattern ImGuiDockNodeFlags_DockSpace)
import DearImGui.Raw.Types (ImGuiID, ImVec2 (..), pattern ImGuiDir_Left)
import Foreign.C.String (withCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek)
import Bindings.GLFW (C'GLFWwindow)
import Graphics.UI.GLFW qualified as GLFW
import Graphics.UI.GLFW.C qualified as GLFWC
import Numeric (showHex)
import System.Directory qualified as Dir
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  startDir <- case args of
    (p : _) -> pure p
    []      -> pure "."

  GLFW.setErrorCallback $ Just \_ msg ->
    hPutStrLn stderr $ "GLFW error: " <> msg

  ok <- GLFW.init
  unless ok do
    hPutStrLn stderr "Failed to initialize GLFW."
    exitFailure

  bracket_ (pure ()) GLFW.terminate do
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
    GLFW.windowHint (GLFW.WindowHint'Resizable True)

    mwin <- GLFW.createWindow 1280 720 "File Viewer" Nothing Nothing
    win <- case mwin of
      Just w  -> pure w
      Nothing -> hPutStrLn stderr "Failed to create GLFW window." >> exitFailure
    GLFW.makeContextCurrent (Just win)
    GLFW.swapInterval 1

    ctx <- ImGui.imGui_CreateContext nullPtr
    enableDocking
    ImGui.imGui_StyleColorsDark nullPtr

    let glfwHandle = castPtr (GLFWC.toC win :: Ptr C'GLFWwindow) :: Ptr GLFWwindow
    _ <- ImplGlfw.cImGui_ImplGlfw_InitForOpenGL glfwHandle (CBool 1)
    initOk <- withCString "#version 130" ImplGL3.cImGui_ImplOpenGL3_InitEx
    case initOk of
      CBool 0 -> hPutStrLn stderr "ImGui_ImplOpenGL3_Init failed." >> exitFailure
      _       -> pure ()

    -- Seed the default split layout iff imgui.ini hasn't been written
    -- yet. After ImGui creates the file (on first shutdown) any user
    -- arrangement is restored automatically.
    iniExists <- Dir.doesFileExist "imgui.ini"
    needsLayout <- newIORef (not iniExists)

    state <- newBrowser startDir
    runLoop win needsLayout state

    ImplGL3.cImGui_ImplOpenGL3_Shutdown
    ImplGlfw.cImGui_ImplGlfw_Shutdown
    ImGui.imGui_DestroyContext ctx
    GLFW.destroyWindow win

runLoop :: GLFW.Window -> IORef Bool -> BrowserState -> IO ()
runLoop win needsLayout state = fix \loop -> do
  close <- GLFW.windowShouldClose win
  unless close do
    GLFW.pollEvents
    ImplGL3.cImGui_ImplOpenGL3_NewFrame
    ImplGlfw.cImGui_ImplGlfw_NewFrame
    ImGui.imGui_NewFrame

    drawUI win needsLayout state

    ImGui.imGui_Render
    (w, h) <- GLFW.getFramebufferSize win
    glViewport 0 0 (fromIntegral w) (fromIntegral h)
    glClearColor 0.10 0.10 0.10 1.0
    glClear glColorBufferBit
    drawData <- ImGui.imGui_GetDrawData
    ImplGL3.cImGui_ImplOpenGL3_RenderDrawData drawData
    GLFW.swapBuffers win
    loop

drawUI :: GLFW.Window -> IORef Bool -> BrowserState -> IO ()
drawUI win needsLayout state = do
  viewport <- ImGui.imGui_GetMainViewport
  dockId <- ImGui.imGui_DockSpaceOverViewport 0 viewport 0 nullPtr
  shouldSeed <- readIORef needsLayout
  when shouldSeed do
    (ww, wh) <- GLFW.getWindowSize win
    seedDefaultLayout dockId (fromIntegral ww) (fromIntegral wh)
    writeIORef needsLayout False
  drawFiles state
  drawPreview state

-- Tear down whatever's at @dockId@ (left over from a previous frame's
-- DockSpaceOverViewport call) and rebuild it as a 30/70 horizontal
-- split with "Files" docked on the left and "Preview" on the right.
-- Run once per app launch (gated by an imgui.ini absence check).
seedDefaultLayout :: ImGuiID -> Float -> Float -> IO ()
seedDefaultLayout dockId w h = do
  ImGuiI.imGui_DockBuilderRemoveNode dockId
  _ <- ImGuiI.imGui_DockBuilderAddNode dockId ImGuiDockNodeFlags_DockSpace
  ImGuiI.imGui_DockBuilderSetNodeSize dockId (ImVec2 (realToFrac w) (realToFrac h))
  alloca \leftP -> alloca \rightP -> do
    _ <- ImGuiI.imGui_DockBuilderSplitNode dockId ImGuiDir_Left 0.30 leftP rightP
    leftId <- peek leftP
    rightId <- peek rightP
    withCString "Files"   \s -> ImGuiI.imGui_DockBuilderDockWindow s leftId
    withCString "Preview" \s -> ImGuiI.imGui_DockBuilderDockWindow s rightId
  ImGuiI.imGui_DockBuilderFinish dockId

drawFiles :: BrowserState -> IO ()
drawFiles state = window "Files" do
  cwdPath <- readIORef state.cwd
  ImGui.imGui_TextUnformatted `withText` Text.pack cwdPath
  _ <- imGuiButton "Up"
    `onClick` do
      let parent = goUp cwdPath
      when (parent /= cwdPath) (changeDir state parent)
  ImGui.imGui_Separator
  childOk <- withCString "##entries" \nm ->
    ImGui.imGui_BeginChild nm zeroVec2 0 0
  when (toBool childOk) do
    es <- readIORef state.entries
    sel <- readIORef state.selected
    for_ es \e -> do
      let label = (if e.isDir then "[D] " else "    ") <> e.name
          full  = cwdPath </> e.name
          isSel = sel == Just full
      clicked <- withCString label \cs -> do
        let flags = 0
        cb <- ImGui.imGui_Selectable cs (cboolOf isSel) flags zeroVec2
        pure (toBool cb)
      when clicked do
        if e.isDir
          then do
            target <- goInto cwdPath e.name
            changeDir state target
          else selectFile state full
    ImGui.imGui_EndChild

drawPreview :: BrowserState -> IO ()
drawPreview state = window "Preview" do
  p <- readIORef state.preview
  case p of
    NoPreview -> labelText "(select a file in the Files pane)"
    ErrorPreview path err ->
      labelText $ "Error reading " <> Text.pack path <> ": " <> err
    TextPreview path body -> do
      labelText (Text.pack path)
      ImGui.imGui_Separator
      childOk <- withCString "##textview" \nm ->
        ImGui.imGui_BeginChild nm zeroVec2 0 0
      when (toBool childOk) do
        ImGui.imGui_TextUnformatted `withText` body
        ImGui.imGui_EndChild
    BinaryPreview path size header -> do
      labelText $ Text.pack path <> "  (" <> Text.pack (show size) <> " bytes)"
      ImGui.imGui_Separator
      labelText $ "First " <> Text.pack (show (BS.length header)) <> " of "
        <> Text.pack (show binaryPreviewLimit) <> " requested:"
      childOk <- withCString "##hexview" \nm ->
        ImGui.imGui_BeginChild nm zeroVec2 0 0
      when (toBool childOk) do
        traverse_ labelText (hexLines header)
        ImGui.imGui_EndChild

changeDir :: BrowserState -> FilePath -> IO ()
changeDir state path = do
  es <- listEntries path
  atomicWriteIORef state.cwd path
  atomicWriteIORef state.entries es

selectFile :: BrowserState -> FilePath -> IO ()
selectFile state path = do
  pv <- loadPreview path
  writeIORef state.selected (Just path)
  writeIORef state.preview pv

window :: String -> IO () -> IO ()
window title body = do
  open <- withCString title \t -> ImGui.imGui_Begin t nullPtr 0
  when (toBool open) body
  ImGui.imGui_End

labelText :: Text -> IO ()
labelText = withText ImGui.imGui_TextUnformatted

withText :: (Ptr a -> Ptr a -> IO b) -> Text -> IO b
withText k t = do
  let bs = TextE.encodeUtf8 t
  BS.useAsCStringLen bs \(p, len) ->
    k (castPtr p) (castPtr (p `plusPtr` len))

imGuiButton :: String -> IO Bool
imGuiButton label =
  withCString label \cs -> toBool <$> ImGui.imGui_Button cs zeroVec2

onClick :: IO Bool -> IO () -> IO ()
onClick action eff = action >>= \b -> when b eff

zeroVec2 :: ImVec2
zeroVec2 = ImVec2 0 0

cboolOf :: Bool -> CBool
cboolOf b = CBool (if b then 1 else 0)

toBool :: CBool -> Bool
toBool (CBool 0) = False
toBool _         = True

hexLines :: ByteString -> [Text]
hexLines bs = go 0 (BS.unpack bs)
  where
    go _ [] = []
    go off xs =
      let (chunk, rest) = splitAt 16 xs
          off'  = off + length chunk
          hex   = Text.intercalate " " [hex2 b | b <- chunk]
          ascii = Text.pack [if b >= 32 && b < 127 then toEnum (fromIntegral b) else '.' | b <- chunk]
          padded = hex <> Text.replicate (16 * 3 - 1 - Text.length hex) " "
          line  = Text.justifyRight 6 '0' (Text.pack (showHex off ""))
                  <> "  " <> padded <> "  " <> ascii
       in line : go off' rest

    hex2 :: Word8 -> Text
    hex2 b =
      let s = Text.pack (showHex b "")
       in if Text.length s == 1 then "0" <> s else s

