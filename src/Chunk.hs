{-# LANGUAGE RecordWildCards #-}

module Chunk where

import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Foldable              as Foldable
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromJust)
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
      let
        blocks = makeBlocks
        chunk = Chunk { chunkBlocks = blocks
                        , chunkPos = pos
                        , chunkVAO = Nothing
                        , chunkEBO = Nothing
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
  let Mesh{..} = fromJust gameMesh

  (Just program) <- GL.get GL.currentProgram

  -- reposition and rescale each chunk
  liftIO $ do
    glModelMatrix <- toGlMatrix chunkModel
    setUniform program "model" glModelMatrix

  -- redraw the chunk if its not updated
  -- otherwise create indices for drawing
  GL.bindVertexArrayObject $= Just meshVAO
  if not isChunkUpdated
  then
    liftIO $ GL.drawElements GL.Triangles (fromIntegral chunkLength) GL.UnsignedInt nullPtr
  else do
    let indices = createIndices chunkBlocks
        len = length indices

    vao <- GL.genObjectName
    ebo <- GL.genObjectName

    liftIO $ do
      GL.bindBuffer GL.ArrayBuffer $= Just meshVBO
      GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
      withArray indices $ \ptr ->
        GL.bufferData GL.ElementArrayBuffer $= (fromIntegral $ len * 4, ptr, GL.StaticDraw)

      GL.drawElements GL.Triangles (fromIntegral len) GL.UnsignedInt nullPtr

    -- no need to recreate buffer next time
    let updatedChunk = chunk { isChunkUpdated = False
                             , chunkVAO = Just vao
                             , chunkEBO = Just ebo
                             , chunkLength = len
                            }
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
