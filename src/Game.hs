{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Game where

import           Control.Monad.State.Strict

import qualified Graphics.Rendering.OpenGL  as GL
import           Linear
import           SDL                        (($=))
import qualified SDL
import           SDL.Video.OpenGL           (Mode (Normal))

import           Camera
import           Chunk
import           Mesh
import           Program
import           States

screenWidth, screenHeight :: Int
(screenWidth, screenHeight) = (800, 600)

game :: Game ()
game = do
  window <- create
  play window
  destroy window

create :: Game SDL.Window
create = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  SDL.HintMouseRelativeModeWarp $= SDL.MouseWarping
  SDL.setMouseLocationMode SDL.RelativeLocation

  let
    openGLConfig = SDL.OpenGLConfig
      { SDL.glColorPrecision = V4 8 8 8 0
      , SDL.glDepthPrecision = 24
      , SDL.glStencilPrecision = 8
      , SDL.glMultisampleSamples = 1
      , SDL.glProfile = SDL.Core Normal 4 1
      }
  window <- SDL.createWindow
    "SDL + OpenGL"
    SDL.defaultWindow
      { SDL.windowInitialSize = V2 (fromIntegral screenWidth) (fromIntegral screenHeight)
      , SDL.windowOpenGL = Just openGLConfig
      }
  SDL.showWindow window
  SDL.glCreateContext window

  GL.depthFunc $= Just GL.Less -- which is glEnable(GL_DEPTH_TEST)
  GL.viewport $=
    ( GL.Position 0 0
    , GL.Size (fromIntegral screenWidth) (fromIntegral screenHeight))

  -- build mesh and programs beforehand so it can be reused
  mesh <- buildMesh
  programs <- liftIO buildPrograms

  gameState <- get
  put $ gameState { gamePrograms = programs
                  , gameMesh = Just mesh
                  }

  return window

destroy :: SDL.Window -> Game ()
destroy window = do
  SDL.destroyWindow window
  SDL.quit

play :: SDL.Window -> Game ()
play window = SDL.time >>= loop window

loop :: SDL.Window -> Float -> Game ()
loop window lastFrame = do
  gameState@GameState{..} <- get

  -- get actions from user
  actions <- liftIO $ parseEvents <$> SDL.pollEvents

  -- update camera
  (camera, currentFrame) <- liftIO $ do
    currentFrame <- SDL.time
    let
      dt = currentFrame - lastFrame
      camera = updateCamera actions gameCamera dt
    return (camera, currentFrame)

  -- update game state
  put $ gameState {
    gameCamera = camera
  }

  makeChunks
  renderChunks

  -- clear frame
  liftIO $ do
    SDL.glSwapWindow window
    GL.clearColor $= GL.Color4 0.1 0.1 0.1 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  unless (QuitProgram `elem` actions) (loop window currentFrame)
