{-# LANGUAGE RecordWildCards #-}

module Chunk where

import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Foldable              as Foldable
import qualified Data.Map.Strict            as Map
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear
import           SDL                        (($=))

import           Camera
import           Program
import           States

blockSize :: Int
blockSize = 16

chunkSize :: Int
chunkSize = blockSize * blockSize * blockSize

makeBlocks :: Vector BlockType
makeBlocks = V.generate chunkSize makeBlock

makeBlock :: Int -> BlockType
makeBlock i = if y == 0 then BlockSolid else BlockEmpty
  where
    (V3 x y z) = indexToPosition i

indexToPosition :: Int -> V3 Int
indexToPosition i = V3 x y z
  where
    z = i `mod` blockSize
    y = (i `div` blockSize) `mod` blockSize
    x = i `div` (blockSize * blockSize)

positionToIndex :: V3 Int -> Int
positionToIndex (V3 x y z) = x + blockSize * (y + blockSize * z)

createVertex :: Vector BlockType -> [Float]
createVertex blocks = concat $ do
  x <- [0..blockSize - 1]
  y <- [0..blockSize - 1]
  z <- [0..blockSize - 1]
  let pos = V3 x y z
  return $ vertex pos

createIndices :: Vector BlockType -> [GL.GLuint]
createIndices blocks = map fromIntegral . concat $ do
  x <- [0..blockSize - 1]
  y <- [0..blockSize - 1]
  z <- [0..blockSize - 1]
  let i = positionToIndex (V3 x y z)
  guard $ showBlock (blocks V.! i)
  j <- [0..5]
  return $ map (\n -> n + 4 * (i * 6 + j)) [0, 1, 2, 2, 1, 3]

showBlock :: BlockType -> Bool
showBlock BlockEmpty = False
showBlock BlockSolid = True

vertex :: V3 Int -> [Float]
vertex pos =
  let (V3 x y z) = fromIntegral <$> pos
      n1 = [0, 0, 1]
      n2 = [0, 0, -1]
      n3 = [1, 0, 0]
      n4 = [-1, 0, 0]
      n5 = [0, -1, 0]
      n6 = [0, 1, 0]
      p1 = [x + 0,  y + 1,  z + 1]
      p2 = [x + 0,  y + 0,  z + 1]
      p3 = [x + 1,  y + 1,  z + 1]
      p4 = [x + 1,  y + 0,  z + 1]
      p5 = [x + 0,  y + 0,  z + 0]
      p6 = [x + 0,  y + 1,  z + 0]
      p7 = [x + 1,  y + 0,  z + 0]
      p8 = [x + 1,  y + 1,  z + 0]
  in concat [ p1, n1 , p2, n1 , p3, n1 , p4, n1
            , p5, n2 , p6, n2 , p7, n2 , p8, n2
            , p4, n3 , p7, n3 , p3, n3 , p8, n3
            , p2, n4 , p1, n4 , p5, n4 , p6, n4
            , p2, n5 , p5, n5 , p4, n5 , p7, n5
            , p6, n6 , p1, n6 , p8, n6 , p3, n6
            ]

makeChunks :: Game ()
makeChunks = do
  gameState@GameState{..} <- get
  let positions = [V3 x 0 z | x <- [0..2], z <- [0..2]]
  mapM_ makeChunk positions

makeChunk :: V3 Int -> Game ()
makeChunk pos = do
  gameState@GameState{..} <- get

  case Map.lookup pos gameChunks of
    Just chunk -> return ()
    Nothing -> do
      vao <- GL.genObjectName
      vbo <- GL.genObjectName
      ebo <- GL.genObjectName
      let
        blocks = makeBlocks
        chunk = Chunk { chunkBlocks = blocks
                        , chunkPos = pos
                        , chunkVAO = vao
                        , chunkVBO = vbo
                        , chunkEBO = ebo
                        , chunkLength = 0
                        , chunkModel =
                            mkTransformationMat
                              (identity :: M33 Float)
                              ((/ fromIntegral blockSize) . fromIntegral <$> pos)
                        , isChunkUpdated = True
                        }
      put gameState { gameChunks = Map.insert pos chunk gameChunks }

renderChunks :: Game ()
renderChunks = do
  GameState{..} <- get

  -- setup program
  let program = getTerrainProgram gamePrograms
  GL.currentProgram $= Just program

  -- set uniforms
  liftIO $ do
    let Camera{..} = gameCamera
    glViewMatrix <- toGlMatrix cameraViewMatrix
    setUniform program "view" glViewMatrix
    glProjectionMatrix <- toGlMatrix cameraProjectionMatrix
    setUniform program "projection" glProjectionMatrix
    setUniform program "objectColor" (GL.Vertex3 1 0.5 0.31 :: GL.Vertex3 Float)
    setUniform program "lightColor" (GL.Vertex3 1 1 1 :: GL.Vertex3 Float)
    setUniform program "lightPos" (GL.Vertex3 5 5 5 :: GL.Vertex3 Float)

  -- render each chunk
  mapM_ renderChunk gameChunks

renderChunk :: Chunk -> Game ()
renderChunk chunk@Chunk{..} = do
  gameState@GameState{..} <- get
  (Just program) <- GL.get GL.currentProgram

  -- reposition and rescale each chunk
  liftIO $ do
    glModelMatrix <- toGlMatrix chunkModel
    setUniform program "model" glModelMatrix

  GL.bindVertexArrayObject $= Just chunkVAO
  -- if the chunk is updated, setup its vertex data
  -- otherwise redraw the chunk if its not updated
  if not isChunkUpdated
  then
      -- draw the vertices
    liftIO $ GL.drawElements GL.Triangles (fromIntegral chunkLength) GL.UnsignedInt nullPtr
  else do
    -- add buffer data
    let vertices = createVertex chunkBlocks
        indices = createIndices chunkBlocks
        len = length indices -- * 6
    liftIO $ do
      GL.bindBuffer GL.ArrayBuffer $= Just chunkVBO
      withArray vertices $ \ptr ->
        GL.bufferData GL.ArrayBuffer $= (fromIntegral $ length vertices * 4, ptr, GL.StaticDraw)

      GL.bindBuffer GL.ElementArrayBuffer $= Just chunkEBO
      withArray indices $ \ptr ->
        GL.bufferData GL.ElementArrayBuffer $= (fromIntegral $ len * 4, ptr, GL.StaticDraw)

      GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
      GL.vertexAttribPointer (GL.AttribLocation 0) $=
        (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 24 (intPtrToPtr 0))

      GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
      GL.vertexAttribPointer (GL.AttribLocation 1) $=
        (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 24 (intPtrToPtr 12))

      GL.drawElements GL.Triangles (fromIntegral len) GL.UnsignedInt nullPtr

    -- no need to recreate buffer next time
    let updatedChunk = chunk { isChunkUpdated = False, chunkLength = len }
    put $ gameState { gameChunks = Map.insert chunkPos updatedChunk gameChunks }

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

floatSize = fromIntegral . sizeOf $ (0 :: Float)

toGlMatrix :: M44 Float -> IO (GL.GLmatrix GL.GLfloat)
toGlMatrix mat =
  GL.withNewMatrix GL.RowMajor $ \glPtr ->
    zipWithM_
      (pokeElemOff glPtr)
      [0 ..]
      (concat $ Foldable.toList <$> Foldable.toList mat)
