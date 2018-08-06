module States where

import           Control.Monad.State.Strict
import           Data.Map.Strict            (Map)
import qualified Data.Vector                as V
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear

import           Camera

data GameState = GameState
  { gameCamera   :: Camera
  , gamePrograms :: Map String GL.Program
  , gameChunks   :: Map (V3 Int) Chunk
  } deriving (Show)

type Game = StateT GameState IO

data BlockType
  = BlockEmpty
  | BlockSolid
  deriving (Show)

data Chunk = Chunk
  { chunkBlocks    :: V.Vector BlockType
  , chunkPos       :: V3 Int
  , chunkVAO       :: GL.VertexArrayObject
  , chunkVBO       :: GL.BufferObject
  , isChunkUpdated :: Bool
  } deriving (Show)
