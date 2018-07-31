{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.ByteString           as BS
import qualified Data.Vector.Storable      as V
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           SDL.Vect
import           System.Exit               (exitFailure)
import           System.IO

import qualified Graphics.Rendering.OpenGL as GL
import           SDL                       (($=))
import qualified SDL
import           SDL.Video.OpenGL          (Mode (Normal))

import           LoadShaders

data Descriptor =
  Descriptor GL.VertexArrayObject
             GL.ArrayIndex
             GL.NumArrayIndices

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
  onDisplay window discriptor
  SDL.destroyWindow window
  SDL.quit

onDisplay :: SDL.Window -> Descriptor -> IO ()
onDisplay window descriptor = do
  GL.clearColor $= GL.Color4 1 1 1 1
  GL.clear [GL.ColorBuffer]
  GL.viewport $=
    ( GL.Position 0 0
    , GL.Size (fromIntegral screenWidth) (fromIntegral screenHeight))
  draw descriptor
  SDL.glSwapWindow window
  events <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  unless quit (onDisplay window descriptor)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

initResources :: IO Descriptor
initResources = do
  triangles <- GL.genObjectName
  GL.bindVertexArrayObject $= Just triangles
  arrayBuffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just arrayBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
  program <-
    loadShaders
      [ ShaderInfo GL.VertexShader (FileSource "./app/shader.vert")
      , ShaderInfo GL.FragmentShader (FileSource "./app/shader.frag")
      ]
  GL.currentProgram $= Just program
  let firstIndex = 0
      vPosition = GL.AttribLocation 0
  GL.vertexAttribPointer vPosition $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float 0 (bufferOffset firstIndex))
  GL.vertexAttribArray vPosition $= GL.Enabled
  return $ Descriptor triangles firstIndex (fromIntegral numVertices)

draw :: Descriptor -> IO ()
draw (Descriptor triangles firstIndex numVertices) = do
  GL.bindVertexArrayObject $= Just triangles
  GL.drawArrays GL.Triangles firstIndex numVertices

vertices :: [GL.Vertex2 GL.GLfloat]
vertices =
  [ GL.Vertex2 (-0.90) (-0.90) -- Triangle 1
  , GL.Vertex2 0.85 (-0.90)
  , GL.Vertex2 (-0.90) 0.85
  -- , GL.Vertex2 0.90 (-0.85) -- Triangle 2
  -- , GL.Vertex2 0.90 0.90
  -- , GL.Vertex2 (-0.85) 0.90
  ]

numVertices = length vertices
