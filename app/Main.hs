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
import           Data.Map.Strict           (Map, (!))
import qualified Data.Map.Strict           as Map
import qualified Graphics.Rendering.OpenGL as GL
import           Linear                    ((!*!), (^+^))
import qualified Linear
import           SDL                       (($=))
import qualified SDL
import           SDL.Vect
import           SDL.Video.OpenGL          (Mode (Normal))

import           Action
import           Camera
import           LoadShaders
import Chunk

data GLData = GLData
  { glProgram  :: GL.Program
  , glVAO      :: GL.VertexArrayObject
  , glIndex    :: GL.ArrayIndex
  , glIndices  :: GL.NumArrayIndices
  , glUniforms :: Map String GL.UniformLocation
  }

type GLDataMap = Map String GLData

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 600)

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
  -- SDL.setMouseLocationMode SDL.RelativeLocation
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
  onDisplay window discriptor initialCamera 0
  SDL.destroyWindow window
  SDL.quit

onDisplay :: SDL.Window -> GLDataMap -> Camera -> Float -> IO ()
onDisplay window glDataMap camera lastFrame = do
  GL.clearColor $= GL.Color4 0.1 0.1 0.1 1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.viewport $=
    ( GL.Position 0 0
    , GL.Size (fromIntegral screenWidth) (fromIntegral screenHeight))

  draw camera glDataMap
  SDL.glSwapWindow window
  events <- SDL.pollEvents
  currentFrame <- SDL.time
  let actions = parseEvents events
      quit = QuitProgram `elem` actions
      deltaTime = currentFrame - lastFrame
      updatedCamera = updateCamera camera actions deltaTime
  unless quit (onDisplay window glDataMap updatedCamera currentFrame)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

initResources :: IO GLDataMap
initResources = do

  -- glEnable(GL_DEPTH_TEST)
  GL.depthFunc $= Just GL.Less

  cubeProgram <- makeCubeProgram
  lampProgram <- makeLampProgram

  return $ Map.fromList [("cube", cubeProgram), ("lamp", lampProgram)]

makeCubeProgram :: IO GLData
makeCubeProgram = do
  let currentChunk = chunk (\(V3 x y z) -> (x * x + y * y + z * z) < 0.25)
  -- vao
  cube <- GL.genObjectName
  GL.bindVertexArrayObject $= Just cube

  -- vbo
  arrayBuffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just arrayBuffer
  withArray currentChunk $ \ptr -> do
    let size = fromIntegral (length currentChunk * sizeOf (head currentChunk))
    GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)

  -- load shader
  program <-
    loadShaders
      [ ShaderInfo GL.VertexShader (FileSource "./app/cube.vert")
      , ShaderInfo GL.FragmentShader (FileSource "./app/cube.frag")
      ]

  -- vertex attribute
  let firstIndex = 0
      aPos = GL.AttribLocation 0
      aNormal = GL.AttribLocation 1
      size = fromIntegral . sizeOf . head $ currentChunk

  GL.vertexAttribPointer aPos $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float (6 * size) (bufferOffset firstIndex))
  GL.vertexAttribArray aPos $= GL.Enabled

  GL.vertexAttribPointer aNormal $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float (6 * size) (bufferOffset $ 3 * size))
  GL.vertexAttribArray aNormal $= GL.Enabled

  uniforms <- makeUniforms program [ "model", "view", "projection", "objectColor", "lightColor", "lightPos"]

  return $ GLData program cube firstIndex (fromIntegral $ div (length currentChunk) 6) uniforms

makeLampProgram :: IO GLData
makeLampProgram = do
  let currentChunk = block 0 0 0
  -- vao
  lamp <- GL.genObjectName
  GL.bindVertexArrayObject $= Just lamp

  -- vbo
  arrayBuffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just arrayBuffer
  withArray currentChunk $ \ptr -> do
    let size = fromIntegral (length currentChunk * sizeOf (head currentChunk))
    GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)

  -- load shader
  program <-
    loadShaders
      [ ShaderInfo GL.VertexShader (FileSource "./app/lamp.vert")
      , ShaderInfo GL.FragmentShader (FileSource "./app/lamp.frag")
      ]

  -- vertex attribute
  let firstIndex = 0
      aPos = GL.AttribLocation 0
      size = fromIntegral . sizeOf . head $ currentChunk
  GL.vertexAttribPointer aPos $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float (6 * size) (bufferOffset firstIndex))
  GL.vertexAttribArray aPos $= GL.Enabled

  uniforms <- makeUniforms program ["model", "view", "projection"]

  return $ GLData program lamp firstIndex (fromIntegral $ div (length currentChunk) 6) uniforms

draw :: Camera -> GLDataMap -> IO ()
draw camera glDataMap = do
  drawChunk camera (glDataMap ! "cube")
  drawLamp camera (glDataMap ! "lamp")

drawChunk :: Camera -> GLData -> IO ()
drawChunk
  (Camera cameraPos cameraFront cameraUp yaw pitch fov)
  (GLData program vao index indices uniforms) = do
  seconds <- SDL.time :: IO Float
  let
      view = Linear.lookAt cameraPos (cameraPos ^+^ cameraFront) cameraUp
      model = Linear.mkTransformationMat (Linear.identity :: M33 Float) (V3 0 0 0)
      projection = Linear.perspective (fov * pi / 180.0) (fromIntegral screenWidth / fromIntegral screenHeight) 0.1 100.0
      light = let (V3 x y z) = lightPos seconds in GL.Vertex3 x y z

  glModelMatrix <- toGlMatrix model
  glViewMatrix <- toGlMatrix view
  glProjectionMatrix <- toGlMatrix projection

  GL.currentProgram $= Just program
  GL.uniform (uniforms ! "model") $= glModelMatrix
  GL.uniform (uniforms ! "view") $= glViewMatrix
  GL.uniform (uniforms ! "projection") $= glProjectionMatrix
  GL.uniform (uniforms ! "objectColor") $= (GL.Vertex3 1 0.5 0.31 :: GL.Vertex3 Float)
  GL.uniform (uniforms ! "lightColor") $= (GL.Vertex3 1 1 1 :: GL.Vertex3 Float)
  GL.uniform (uniforms ! "lightPos") $= light
  GL.bindVertexArrayObject $= Just vao
  GL.drawArrays GL.Triangles index indices

drawLamp  :: Camera -> GLData -> IO ()
drawLamp
  (Camera cameraPos cameraFront cameraUp yaw pitch fov)
  (GLData program vao index indices uniforms) = do
  seconds <- SDL.time :: IO Float
  let
      view = Linear.lookAt cameraPos (cameraPos ^+^ cameraFront) cameraUp
      model = Linear.mkTransformationMat (Linear.identity :: M33 Float) (lightPos seconds) !*! Linear.scaled (V4 0.1 0.1 0.1 1)
      projection = Linear.perspective (fov * pi / 180.0) (fromIntegral screenWidth / fromIntegral screenHeight) 0.1 100.0

  glModelMatrix <- toGlMatrix model
  glViewMatrix <- toGlMatrix view
  glProjectionMatrix <- toGlMatrix projection

  GL.currentProgram $= Just program
  GL.uniform (uniforms ! "model") $= glModelMatrix
  GL.uniform (uniforms ! "view") $= glViewMatrix
  GL.uniform (uniforms ! "projection") $= glProjectionMatrix
  GL.bindVertexArrayObject $= Just vao
  GL.drawArrays GL.Triangles index indices

toGlMatrix :: M44 Float -> IO (GL.GLmatrix GL.GLfloat)
toGlMatrix mat =
  GL.withNewMatrix GL.RowMajor $ \glPtr ->
    zipWithM_
      (pokeElemOff glPtr)
      [0 ..]
      (concat $ Foldable.toList <$> Foldable.toList mat)

makeUniforms :: GL.Program -> [String] -> IO (Map String GL.UniformLocation)
makeUniforms program names = Map.fromList <$> mapM makeUniform names
  where
    makeUniform name = do
      uniform <- GL.uniformLocation program name
      return (name, uniform)

lightPos :: Float -> V3 Float
lightPos s = V3  (10 * cos s) 0 (10 * sin s)
