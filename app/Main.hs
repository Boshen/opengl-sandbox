{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable

import qualified Data.Foldable             as Foldable
import qualified Graphics.Rendering.OpenGL as GL
import           Linear
import           SDL                       (($=))
import qualified SDL
import           SDL.Video.OpenGL          (Mode (Normal))

import           Action
import           Camera
import           Chunk
import           LoadShaders

data GLData = GLData
  { glProgram :: GL.Program
  , glVAO     :: GL.VertexArrayObject
  , glVBO     :: GL.BufferObject
  }

data AppState = AppState
  { terrain :: ([Chunk], GLData)
  , lamp    :: ([Chunk], GLData)
  }

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
  SDL.warpMouse SDL.WarpCurrentFocus (SDL.P $ V2 0 0)
  GL.depthFunc $= Just GL.Less -- glEnable(GL_DEPTH_TEST)
  app <- initApp
  onDisplay window app initialCamera 0
  SDL.destroyWindow window
  SDL.quit

onDisplay :: SDL.Window -> AppState -> Camera -> Float -> IO ()
onDisplay window app camera lastFrame = do
  GL.clearColor $= GL.Color4 0.1 0.1 0.1 1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.viewport $=
    ( GL.Position 0 0
    , GL.Size (fromIntegral screenWidth) (fromIntegral screenHeight))
  app' <- draw camera app
  SDL.glSwapWindow window
  events <- SDL.pollEvents
  currentFrame <- SDL.time
  let actions = parseEvents events
      quit = QuitProgram `elem` actions
      deltaTime = currentFrame - lastFrame
      updatedCamera = updateCamera camera actions deltaTime
  unless quit (onDisplay window app' updatedCamera currentFrame)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

initApp :: IO AppState
initApp = do
  cubeProgram <- makeCubeProgram
  lampProgram <- makeLampProgram
  let lampChunk = makeBlock (V3 0 0 0)
      cubeChunk =
        makeBlocks
          (\v ->
             let (V3 x y z) = v ^-^ V3 8 8 8
              in (x * x + y * y + z * z) < 64)
  return $
    AppState
      { terrain = ([Chunk cubeChunk False], cubeProgram)
      , lamp = ([Chunk lampChunk False], lampProgram)
      }

makeCubeProgram :: IO GLData
makeCubeProgram = do
  vao <- GL.genObjectName
  vbo <- GL.genObjectName
  program <-
    loadShaders
      [ ShaderInfo GL.VertexShader (FileSource "./app/cube.vert")
      , ShaderInfo GL.FragmentShader (FileSource "./app/cube.frag")
      ]
  mapM_
    (GL.uniformLocation program)
    ["model", "view", "projection", "objectColor", "lightColor", "lightPos"]
  return $ GLData program vao vbo

makeLampProgram :: IO GLData
makeLampProgram = do
  vao <- GL.genObjectName
  vbo <- GL.genObjectName
  program <-
    loadShaders
      [ ShaderInfo GL.VertexShader (FileSource "./app/lamp.vert")
      , ShaderInfo GL.FragmentShader (FileSource "./app/lamp.frag")
      ]
  mapM_ (GL.uniformLocation program) ["model", "view", "projection"]
  return $ GLData program vao vbo

draw :: Camera -> AppState -> IO AppState
draw camera app = do
  terrain <- drawChunk camera (terrain app)
  lamp' <- drawLamp camera (lamp app)
  return $ AppState terrain lamp'

drawChunk :: Camera -> ([Chunk], GLData) -> IO ([Chunk], GLData)
drawChunk camera@(Camera cameraPos cameraFront cameraUp yaw pitch fov) (chunks, glData@(GLData program vao vbo)) = do
  seconds <- SDL.time :: IO Float
  let (Chunk chunkBlocks isChunkUpdated) = head chunks
      view = getViewMatrix camera
      model = mkTransformationMat (identity :: M33 Float) (V3 0 0 0)
      projection =
        perspective
          (fov * pi / 180.0)
          (fromIntegral screenWidth / fromIntegral screenHeight)
          0.1
          100.0
      light =
        let (V3 x y z) = lightPos seconds
         in GL.Vertex3 x y z
  glModelMatrix <- toGlMatrix model
  glViewMatrix <- toGlMatrix view
  glProjectionMatrix <- toGlMatrix projection
  GL.currentProgram $= Just program
  unless isChunkUpdated $ do
    let firstIndex = 0
        aPos = GL.AttribLocation 0
        aNormal = GL.AttribLocation 1
        size = fromIntegral . sizeOf $ (0 :: Float)
    GL.bindVertexArrayObject $= Just vao
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    withArray chunkBlocks $ \ptr -> do
      let size = fromIntegral (length chunkBlocks * sizeOf (head chunkBlocks))
      GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
    GL.vertexAttribPointer aPos $=
      ( GL.ToFloat
      , GL.VertexArrayDescriptor 3 GL.Float (6 * size) (bufferOffset firstIndex))
    GL.vertexAttribArray aPos $= GL.Enabled
    GL.vertexAttribPointer aNormal $=
      ( GL.ToFloat
      , GL.VertexArrayDescriptor 3 GL.Float (6 * size) (bufferOffset $ 3 * size))
    GL.vertexAttribArray aNormal $= GL.Enabled
  setUniform program "model" glModelMatrix
  setUniform program "view" glViewMatrix
  setUniform program "projection" glProjectionMatrix
  setUniform program "objectColor" (GL.Vertex3 1 0.5 0.31 :: GL.Vertex3 Float)
  setUniform program "lightColor" (GL.Vertex3 1 1 1 :: GL.Vertex3 Float)
  setUniform program "lightPos" light
  GL.bindVertexArrayObject $= Just vao
  GL.drawArrays GL.Triangles 0 (fromIntegral $ div (length chunkBlocks) 6)
  return ([Chunk chunkBlocks True], glData)

drawLamp :: Camera -> ([Chunk], GLData) -> IO ([Chunk], GLData)
drawLamp camera@(Camera cameraPos cameraFront cameraUp yaw pitch fov) (chunks, glData@(GLData program vao vbo)) = do
  seconds <- SDL.time :: IO Float
  let (Chunk chunkBlocks isChunkUpdated) = head chunks
      view = getViewMatrix camera
      model = mkTransformationMat (identity :: M33 Float) (lightPos seconds)
      projection =
        perspective
          (fov * pi / 180.0)
          (fromIntegral screenWidth / fromIntegral screenHeight)
          0.1
          100.0
  glModelMatrix <- toGlMatrix model
  glViewMatrix <- toGlMatrix view
  glProjectionMatrix <- toGlMatrix projection
  GL.currentProgram $= Just program
  unless isChunkUpdated $ do
    let firstIndex = 0
        aPos = GL.AttribLocation 0
        size = fromIntegral . sizeOf $ (0 :: Float)
    GL.bindVertexArrayObject $= Just vao
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    withArray chunkBlocks $ \ptr -> do
      let size = fromIntegral (length chunkBlocks * sizeOf (head chunkBlocks))
      GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
    GL.vertexAttribPointer aPos $=
      ( GL.ToFloat
      , GL.VertexArrayDescriptor 3 GL.Float (6 * size) (bufferOffset firstIndex))
    GL.vertexAttribArray aPos $= GL.Enabled
  setUniform program "model" glModelMatrix
  setUniform program "view" glViewMatrix
  setUniform program "projection" glProjectionMatrix
  GL.bindVertexArrayObject $= Just vao
  GL.drawArrays GL.Triangles 0 (fromIntegral $ div (length chunkBlocks) 6)
  return ([Chunk chunkBlocks True], glData)

setUniform program name d = do
  location <- GL.uniformLocation program name
  GL.uniform location $= d

toGlMatrix :: M44 Float -> IO (GL.GLmatrix GL.GLfloat)
toGlMatrix mat =
  GL.withNewMatrix GL.RowMajor $ \glPtr ->
    zipWithM_
      (pokeElemOff glPtr)
      [0 ..]
      (concat $ Foldable.toList <$> Foldable.toList mat)

lightPos :: Float -> V3 Float
lightPos s = V3 (10 * cos s) 0 (10 * sin s)
