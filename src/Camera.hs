module Camera
  ( Camera(..)
  , initialCamera
  , updateCamera
  ) where

import           Data.Foldable
import           Data.Function
import           Linear
import           SDL.Event
import           SDL.Input.Keyboard

data Camera = Camera
  { cameraPos   :: V3 Float
  , cameraFront :: V3 Float
  , cameraUp    :: V3 Float
  }

data MoveAction
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | MoveHalt

initialCamera :: Camera
initialCamera = Camera (V3 0 0 3) (V3 0 0 (-1)) (V3 1 0 0)

updateCamera :: Camera -> [Event] -> Float -> Camera
updateCamera camera events deltaTime =
  events & map eventToAction & foldl' (addCamera deltaTime) camera

addCamera :: Float -> Camera -> MoveAction -> Camera
addCamera deltaTime camera@(Camera pos front up) event = Camera p front up
  where
    cameraSpeed = 2.5 * deltaTime
    p =
      case event of
        MoveUp    -> pos ^+^ (up ^* cameraSpeed)
        MoveDown  -> pos ^-^ (up ^* cameraSpeed)
        MoveLeft  -> pos ^+^ signorm (cross front up) ^* cameraSpeed
        MoveRight -> pos ^-^ signorm (cross front up) ^* cameraSpeed
        MoveHalt  -> pos

eventToAction :: Event -> MoveAction
eventToAction event =
  case eventPayload event of
    KeyboardEvent d ->
      case keysymKeycode . keyboardEventKeysym $ d of
        KeycodeRight -> MoveRight
        KeycodeLeft  -> MoveLeft
        KeycodeDown  -> MoveDown
        KeycodeUp    -> MoveUp
        _            -> MoveHalt
    _ -> MoveHalt
