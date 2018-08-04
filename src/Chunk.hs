module Chunk where

import           Control.Monad
import           Linear

data Chunk = Chunk
  { chunkBlocks    :: [Float]
  , chunkLocation  :: V3 Float
  , isChunkUpdated :: Bool
  }

blockSize :: Float
blockSize = 8

makeBlocks :: V3 Float -> [Float]
makeBlocks (V3 dx dy dz) = concat blocks
  where
    blocks = do
      x <- [0 .. round blockSize - 1]
      y <- [0 .. round blockSize - 1]
      z <- [0 .. round blockSize - 1]
      guard $ y == 0
      let v = V3 (fromIntegral x) (fromIntegral y) (fromIntegral z)
      return $ makeBlock (v ^/ blockSize)

makeBlock :: V3 Float -> [Float]
makeBlock (V3 x y z) =
  let size = 1 / blockSize
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
        [ p6 , n2 , p5 , n2 , p8 , n2 , p8 , n2 , p7 , n2
        , p6 , n2 , p1 , n1 , p2 , n1 , p3 , n1 , p3 , n1
        , p4 , n1 , p1 , n1 , p4 , n4 , p7 , n4 , p6 , n4
        , p6 , n4 , p1 , n4 , p4 , n4 , p3 , n3 , p8 , n3
        , p5 , n3 , p5 , n3 , p2 , n3 , p3 , n3 , p6 , n6
        , p5 , n6 , p2 , n6 , p2 , n6 , p1 , n6 , p6 , n6
        , p7 , n5 , p8 , n5 , p3 , n5 , p3 , n5 , p4 , n5
        , p7 , n5
        ]
