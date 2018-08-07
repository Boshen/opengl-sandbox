module States where

import           Control.Monad.State.Strict
import           Data.Map.Strict            (Map)
import           Data.Maybe                 (Maybe)
import qualified Data.Vector                as V
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear

import           Camera

data GameState = GameState
  { gameCamera   :: Camera
  , gamePrograms :: Map String GL.Program
  , gameChunks   :: Map (V3 Int) Chunk
  , gameMesh     :: Maybe Mesh
  } deriving (Show)

type Game = StateT GameState IO

data BlockType
  = BlockEmpty
  | BlockSolid
  deriving (Show)

data Chunk = Chunk
  { chunkBlocks    :: V.Vector BlockType
  , chunkPos       :: V3 Int
  , chunkVAO       :: Maybe GL.VertexArrayObject
  , chunkEBO       :: Maybe GL.BufferObject
  , chunkLength    :: Int
  , chunkModel     :: M44 Float
  , isChunkUpdated :: Bool
  } deriving (Show)

data Mesh = Mesh
  { meshVAO    :: GL.VertexArrayObject
  , meshVBO    :: GL.BufferObject
  } deriving (Show)
