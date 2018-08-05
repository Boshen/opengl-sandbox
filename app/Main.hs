{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.Reader
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable

import qualified Data.Foldable             as Foldable
import           Data.Vector               (Vector)
import qualified Data.Vector               as V
import qualified Graphics.Rendering.OpenGL as GL
import           Linear
import           SDL                       (($=))
import qualified SDL
import           SDL.Video.OpenGL          (Mode (Normal))

import           Motion
import           Camera
import           Chunk
import           LoadShaders

data GLData = GLData
  { glProgram :: GL.Program
  , glVAO     :: GL.VertexArrayObject
  , glVBO     :: GL.BufferObject
  }

type GLDataMap = Map String GLData

data World = World
  { terrain :: Vector Chunk
  , lamp    :: Vector Chunk
  }

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 600)

worldSize :: Int
worldSize = 3

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
  SDL.setMouseLocationMode SDL.RelativeLocation
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
  (app, dataMap) <- initApp
  onDisplay app window initialCamera 0 dataMap
  SDL.destroyWindow window
  SDL.quit

onDisplay :: World -> SDL.Window -> Camera -> Float -> GLDataMap -> IO ()
onDisplay app window camera lastFrame dataMap = do
  GL.clearColor $= GL.Color4 0.1 0.1 0.1 1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.viewport $=
    ( GL.Position 0 0
    , GL.Size (fromIntegral screenWidth) (fromIntegral screenHeight))
  app' <- runReaderT (draw camera app) dataMap
  SDL.glSwapWindow window
  events <- SDL.pollEvents
  currentFrame <- SDL.time
  let actions = parseEvents events
      quit = QuitProgram `elem` actions
      deltaTime = currentFrame - lastFrame
      updatedCamera = updateCamera actions camera deltaTime
      app'' = updateApp currentFrame app'
  unless quit (onDisplay app'' window updatedCamera currentFrame dataMap)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

initApp :: IO (World, GLDataMap)
initApp = do
  terrainData <- makeCubeProgram
  lampData <- makeLampProgram
  let map = Map.fromList [ ("terrain", terrainData) , ("lamp", lampData) ]
  return (World V.empty V.empty, map)

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

draw :: Camera -> World -> ReaderT GLDataMap IO World
draw camera app = do
  terrain <- V.imapM (drawTerrain camera) (terrain app)
  lamp <- V.imapM (drawLamp camera) (lamp app)
  return $ World terrain lamp

drawTerrain :: Camera -> Int -> Chunk -> ReaderT GLDataMap IO Chunk
drawTerrain camera@Camera{..} index chunk = do
  dataMap <- ask
  liftIO $ do
    seconds <- SDL.time :: IO Float
    let
        (GLData program vao vbo) = dataMap Map.! "terrain"
        (Chunk chunkBlocks isChunkUpdated) = chunk
        chunkLocation = V3 (fromIntegral (div index worldSize) / fromIntegral blockSize) 0 (fromIntegral (mod index worldSize) / fromIntegral blockSize)
        view = getViewMatrix camera
        model = mkTransformationMat (identity :: M33 Float) chunkLocation !*! scaled (fromIntegral <$> V4 blockSize blockSize blockSize 1)
        projection =
          perspective
            (cameraFov * pi / 180.0)
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
    when isChunkUpdated $ do
      let firstIndex = 0
          aPos = GL.AttribLocation 0
          aNormal = GL.AttribLocation 1
          size = fromIntegral . sizeOf $ (0 :: Float)
          vertices = createVertex chunkBlocks
      GL.bindVertexArrayObject $= Just vao
      GL.bindBuffer GL.ArrayBuffer $= Just vbo
      withArray vertices $ \ptr -> do
        let size = fromIntegral (length vertices * sizeOf (head vertices))
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
    GL.drawArrays GL.Triangles 0 (fromIntegral $ V.length (V.filter showBlock chunkBlocks) * 36)

    return $ Chunk chunkBlocks False

drawLamp :: Camera -> Int -> Chunk -> ReaderT GLDataMap IO Chunk
drawLamp camera@Camera{..}  index chunk = do
  dataMap <- ask
  liftIO $ do
    seconds <- SDL.time :: IO Float
    let
        (GLData program vao vbo) = dataMap Map.! "lamp"
        (Chunk chunkBlocks isChunkUpdated) = chunk
        view = getViewMatrix camera
        model = mkTransformationMat (identity :: M33 Float) (lightPos seconds ^+^ V3 0 (fromIntegral index) 0)
        projection =
          perspective
            (cameraFov * pi / 180.0)
            (fromIntegral screenWidth / fromIntegral screenHeight)
            0.1
            100.0
    glModelMatrix <- toGlMatrix model
    glViewMatrix <- toGlMatrix view
    glProjectionMatrix <- toGlMatrix projection

    GL.currentProgram $= Just program
    when isChunkUpdated $ do
      let firstIndex = 0
          aPos = GL.AttribLocation 0
          size = fromIntegral . sizeOf $ (0 :: Float)
          vertices = createVertex chunkBlocks
      GL.bindVertexArrayObject $= Just vao
      GL.bindBuffer GL.ArrayBuffer $= Just vbo
      withArray vertices $ \ptr -> do
        let size = fromIntegral (length vertices * sizeOf (head vertices))
        GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
      GL.vertexAttribPointer aPos $=
        ( GL.ToFloat
        , GL.VertexArrayDescriptor 3 GL.Float (6 * size) (bufferOffset firstIndex))
      GL.vertexAttribArray aPos $= GL.Enabled

    setUniform program "model" glModelMatrix
    setUniform program "view" glViewMatrix
    setUniform program "projection" glProjectionMatrix
    GL.bindVertexArrayObject $= Just vao
    GL.drawArrays GL.Triangles 0 (fromIntegral $ length chunkBlocks * 36)
    return $ Chunk chunkBlocks False

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

updateApp :: Float -> World -> World
updateApp time (World terrain lamp) = World terrain' lamp'
  where
    terrain' = updateTerrain terrain
    lamp' = updateLamp lamp

updateTerrain :: Vector Chunk -> Vector Chunk
updateTerrain chunks = if null chunks
  then V.generate (worldSize * worldSize) (const createChunk)
  else chunks

updateLamp :: Vector Chunk -> Vector Chunk
updateLamp chunks = if null chunks
  then V.generate 1 (const createChunk)
  else chunks
