{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.ByteString           as BS
import qualified Data.Vector.Storable      as V
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           System.Exit               (exitFailure)
import           System.IO

import           Control.Lens              ((&), (.~))
import qualified Data.Foldable             as Foldable
import qualified Graphics.Rendering.OpenGL as GL
import           Linear                    ((^+^))
import qualified Linear
import           SDL                       (($=))
import qualified SDL
import           SDL.Vect
import           SDL.Video.OpenGL          (Mode (Normal))

import           LoadShaders
import Camera

data Uniforms = Uniforms
  { timeLocation       :: GL.UniformLocation
  , modelLocation      :: GL.UniformLocation
  , viewLocation       :: GL.UniformLocation
  , projectionLocation :: GL.UniformLocation
  }

data Descriptor =
  Descriptor GL.VertexArrayObject
             GL.ArrayIndex
             GL.NumArrayIndices
             Uniforms

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

openGLConfig :: SDL.OpenGLConfig
openGLConfig =
  SDL.OpenGLConfig
    { SDL.glColorPrecision = V4 8 8 8 0
    , SDL.glDepthPrecision = 24
    , SDL.glStencilPrecision = 8
    , SDL.glMultisampleSamples = 1
    , SDL.glProfile = SDL.Core Normal 4 1
    }

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"
  window <-
    SDL.createWindow
      "SDL + OpenGL"
      SDL.defaultWindow
        { SDL.windowInitialSize = V2 screenWidth screenHeight
        , SDL.windowOpenGL = Just openGLConfig
        }
  SDL.showWindow window
  SDL.glCreateContext window
  discriptor <- initResources
  onDisplay window discriptor initialCamera
  SDL.destroyWindow window
  SDL.quit

onDisplay :: SDL.Window -> Descriptor -> Camera -> IO ()
onDisplay window descriptor camera = do
  GL.clearColor $= GL.Color4 1 1 1 1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.viewport $=
    ( GL.Position 0 0
    , GL.Size (fromIntegral screenWidth) (fromIntegral screenHeight))

  draw camera descriptor
  SDL.glSwapWindow window
  events <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
      updatedCamera = updateCamera camera events
  unless quit (onDisplay window descriptor updatedCamera)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

initResources :: IO Descriptor
initResources = do

  -- glEnable(GL_DEPTH_TEST)
  GL.depthFunc $= Just GL.Less

  -- vao
  triangles <- GL.genObjectName
  GL.bindVertexArrayObject $= Just triangles

  -- vbo
  arrayBuffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just arrayBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)

  -- program
  program <-
    loadShaders
      [ ShaderInfo GL.VertexShader (FileSource "./app/shader.vert")
      , ShaderInfo GL.FragmentShader (FileSource "./app/shader.frag")
      ]

  -- vertex attribute
  let firstIndex = 0
      aPos = GL.AttribLocation 0
      size = fromIntegral . sizeOf . head $ vertices
  GL.vertexAttribPointer aPos $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float (5 * size) (bufferOffset firstIndex))
  GL.vertexAttribArray aPos $= GL.Enabled

  -- color attribute
  -- let aColor = GL.AttribLocation 1
  -- GL.vertexAttribPointer aColor $=
    -- ( GL.ToFloat
    -- , GL.VertexArrayDescriptor 3 GL.Float (6 * size) (bufferOffset (size * 3)))
  -- GL.vertexAttribArray aColor $= GL.Enabled

  -- uniform time
  time <- GL.uniformLocation program "time"
  model <- GL.uniformLocation program "model"
  view <- GL.uniformLocation program "view"
  projection <- GL.uniformLocation program "projection"

  GL.currentProgram $= Just program
  let uniforms = Uniforms time model view projection
  return $ Descriptor triangles firstIndex (fromIntegral numVertices) uniforms

draw :: Camera -> Descriptor -> IO ()
draw (Camera cameraPos cameraFront cameraUp) (Descriptor triangles firstIndex numVertices uniforms) = do
  seconds <- SDL.time :: IO Float

  -- set model view project
  let
      view = Linear.lookAt cameraPos (cameraPos ^+^ cameraFront) cameraUp
      model =   Linear.m33_to_m44 . fromQuaternion $ axisAngle (V3 1 0 0) seconds
      projection = Linear.perspective (45.0 * pi / 180.0) (fromIntegral screenWidth / fromIntegral screenHeight) 0.1 100.0

  glModelMatrix <- toGlMatrix model
  GL.uniform (modelLocation uniforms) $= glModelMatrix

  glViewMatrix <- toGlMatrix view
  GL.uniform (viewLocation uniforms) $= glViewMatrix

  glProjectionMatrix <- toGlMatrix projection
  GL.uniform (projectionLocation uniforms) $= glProjectionMatrix

  -- set time
  GL.uniform (timeLocation uniforms) $= seconds

  -- draw triangle
  GL.bindVertexArrayObject $= Just triangles
  GL.drawArrays GL.Triangles firstIndex numVertices

vertices :: [Float]
vertices =
  [
      -0.5, -0.5, -0.5,  0.0, 0.0,
     0.5, -0.5, -0.5,  1.0, 0.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
    -0.5,  0.5, -0.5,  0.0, 1.0,
    -0.5, -0.5, -0.5,  0.0, 0.0,

    -0.5, -0.5,  0.5,  0.0, 0.0,
     0.5, -0.5,  0.5,  1.0, 0.0,
     0.5,  0.5,  0.5,  1.0, 1.0,
     0.5,  0.5,  0.5,  1.0, 1.0,
    -0.5,  0.5,  0.5,  0.0, 1.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,

    -0.5,  0.5,  0.5,  1.0, 0.0,
    -0.5,  0.5, -0.5,  1.0, 1.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,
    -0.5,  0.5,  0.5,  1.0, 0.0,

     0.5,  0.5,  0.5,  1.0, 0.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
     0.5, -0.5, -0.5,  0.0, 1.0,
     0.5, -0.5, -0.5,  0.0, 1.0,
     0.5, -0.5,  0.5,  0.0, 0.0,
     0.5,  0.5,  0.5,  1.0, 0.0,

    -0.5, -0.5, -0.5,  0.0, 1.0,
     0.5, -0.5, -0.5,  1.0, 1.0,
     0.5, -0.5,  0.5,  1.0, 0.0,
     0.5, -0.5,  0.5,  1.0, 0.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,

    -0.5,  0.5, -0.5,  0.0, 1.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
     0.5,  0.5,  0.5,  1.0, 0.0,
     0.5,  0.5,  0.5,  1.0, 0.0,
    -0.5,  0.5,  0.5,  0.0, 0.0,
    -0.5,  0.5, -0.5,  0.0, 1.0
  ]

numVertices = length vertices

toGlMatrix :: M44 Float -> IO (GL.GLmatrix GL.GLfloat)
toGlMatrix mat =
  GL.withNewMatrix GL.RowMajor $ \glPtr ->
    zipWithM_
      (pokeElemOff glPtr)
      [0 ..]
      (concat $ Foldable.toList <$> Foldable.toList mat)
