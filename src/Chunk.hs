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

createVertex :: Vector BlockType -> [Float]
createVertex blocks = concat $ do
  x <- [0..blockSize-1]
  y <- [0..blockSize-1]
  z <- [0..blockSize-1]
  guard $ showBlock (blocks V.! (x + blockSize * (y + blockSize * z)))
  return $ vertex (V3 x y z)

showBlock :: BlockType -> Bool
showBlock BlockEmpty = False
showBlock BlockSolid = True

vertex :: V3 Int -> [Float]
vertex pos =
  let
      (V3 x y z) = fromIntegral <$> pos
      p1 = [x - 1, y - 1, z + 1]
      p2 = [x + 1, y - 1, z + 1]
      p3 = [x + 1, y + 1, z + 1]
      p4 = [x - 1, y + 1, z + 1]
      p5 = [x + 1, y - 1, z - 1]
      p6 = [x - 1, y - 1, z - 1]
      p7 = [x - 1, y + 1, z - 1]
      p8 = [x + 1, y + 1, z - 1]
      n1 = [0.0, 0.0, 1.0] -- front
      n2 = [0.0, 0.0, -1.0] -- back
      n3 = [1.0, 0.0, 0.0] -- right
      n4 = [-1.0, 0.0, 0.0] -- left
      n5 = [0.0, 1.0, 0.0] -- top
      n6 = [0.0, -1.0, 0.0] -- bottom
   in concat
        [ p6 , n2 , p5 , n2 , p8 , n2 , p8 , n2 , p7 , n2 , p6 , n2
        , p1 , n1 , p2 , n1 , p3 , n1 , p3 , n1 , p4 , n1 , p1 , n1
        , p4 , n4 , p7 , n4 , p6 , n4 , p6 , n4 , p1 , n4 , p4 , n4
        , p3 , n3 , p8 , n3 , p5 , n3 , p5 , n3 , p2 , n3 , p3 , n3
        , p6 , n6 , p5 , n6 , p2 , n6 , p2 , n6 , p1 , n6 , p6 , n6
        , p7 , n5 , p8 , n5 , p3 , n5 , p3 , n5 , p4 , n5 , p7 , n5
        ]

makeChunks :: Game ()
makeChunks = do
  gameState@GameState{..} <- get
  let positions = [V3 x 0 z | x <- [0..3], z <- [0..3]]
  mapM_ makeChunk positions

makeChunk :: V3 Int -> Game ()
makeChunk pos = do
  gameState@GameState{..} <- get

  case Map.lookup pos gameChunks of
    Just chunk -> return ()
    Nothing -> do
      vao <- GL.genObjectName
      vbo <- GL.genObjectName
      let
        blocks = makeBlocks
        chunk = Chunk { chunkBlocks = blocks
                        , chunkPos = pos
                        , chunkVAO = vao
                        , chunkVBO = vbo
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

  -- if the chunk is updated,
  -- bind all vertext attributes and add buffer data
  when isChunkUpdated $ do
    -- triangles
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    GL.vertexAttribPointer (GL.AttribLocation 0) $=
      (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (6 * floatSize) (bufferOffset 0))

    -- normal
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
    GL.vertexAttribPointer (GL.AttribLocation 1) $=
      (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (6 * floatSize) (bufferOffset $ 3 * floatSize))

    -- add buffer data
    let vertices = createVertex chunkBlocks
    GL.bindBuffer GL.ArrayBuffer $= Just chunkVBO
    liftIO $ withArray vertices $ \ptr -> do
      let size = fromIntegral (length vertices * sizeOf (head vertices))
      GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)

    -- no need to recreate buffer next time
    let updatedChunk = chunk { isChunkUpdated = False }
    put $ gameState { gameChunks = Map.insert chunkPos updatedChunk gameChunks }

  -- finally draw the vertices
  GL.bindVertexArrayObject $= Just chunkVAO
  liftIO $ GL.drawArrays GL.Triangles 0 (fromIntegral $ V.length (V.filter showBlock chunkBlocks) * 36)

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
