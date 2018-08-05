module Chunk where

import           Control.Monad
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Linear

data BlockType = BlockEmpty | BlockSolid

data Chunk = Chunk
  { chunkBlocks    :: V.Vector BlockType
  , isChunkUpdated :: Bool
  }

blockSize :: Int
blockSize = 16

chunkSize :: Int
chunkSize = blockSize * blockSize * blockSize

createChunk :: Chunk
createChunk = Chunk makeBlocks True

makeBlocks :: Vector BlockType
makeBlocks = V.generate chunkSize makeBlock

makeBlock :: Int -> BlockType
makeBlock i = if y == 1 then BlockSolid else BlockEmpty
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
      (V3 x y z) =  (fromIntegral <$> pos) ^/ fromIntegral blockSize
      size = 1 / fromIntegral blockSize
      p1 = [x - size, y - size, z + size]
      p2 = [x + size, y - size, z + size]
      p3 = [x + size, y + size, z + size]
      p4 = [x - size, y + size, z + size]
      p5 = [x + size, y - size, z - size]
      p6 = [x - size, y - size, z - size]
      p7 = [x - size, y + size, z - size]
      p8 = [x + size, y + size, z - size]
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
