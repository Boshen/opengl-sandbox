module Main where

import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as Map

import           Camera
import           Game
import           States

main :: IO ()
main = do
  let state = GameState {
      gameCamera = initialCamera
    , gamePrograms = Map.empty
    , gameChunks = Map.empty
    , gameMesh = Nothing
    }
  evalStateT game state
