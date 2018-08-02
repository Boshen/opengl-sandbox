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
  , cameraYaw   :: Float
  , cameraPitch :: Float
  } deriving (Show)

data MoveAction
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | MoveHalt
  | MoveMouse Float
              Float
  | ResetPosition

initialCamera :: Camera
initialCamera = Camera (V3 0 0 3) (V3 0 0 (-1)) (V3 0 1 0) (-90) 0

sensitivity = 0.05

updateCamera :: Camera -> [Event] -> Float -> Camera
updateCamera camera events deltaTime =
  events & map eventToAction & foldl' (addCamera deltaTime) camera

addCamera :: Float -> Camera -> MoveAction -> Camera
addCamera deltaTime camera@(Camera pos front up yaw pitch) event =
  case event of
    ResetPosition -> initialCamera
    MoveMouse dx dy -> Camera pos front' up yaw' pitch'
      where yaw' = yaw + dx
            pitch' = max (-89) . min 89 $ pitch + dy
            radYaw = radians yaw'
            radPitch = radians pitch
            front' =
              signorm $
              V3
                (cos radPitch * cos radYaw)
                (sin radPitch)
                (sin radYaw * cos radPitch)
    _ -> Camera pos' front up yaw pitch
      where cameraSpeed = 2.5 * deltaTime
            pos' =
              case event of
                MoveUp    -> pos ^+^ (front ^* cameraSpeed)
                MoveDown  -> pos ^-^ (front ^* cameraSpeed)
                MoveLeft  -> pos ^+^ signorm (cross front up) ^* cameraSpeed
                MoveRight -> pos ^-^ signorm (cross front up) ^* cameraSpeed
                MoveHalt  -> pos
                _         -> pos

eventToAction :: Event -> MoveAction
eventToAction event =
  case eventPayload event of
    KeyboardEvent d ->
      case keysymKeycode . keyboardEventKeysym $ d of
        KeycodeRight -> MoveRight
        KeycodeLeft  -> MoveLeft
        KeycodeDown  -> MoveDown
        KeycodeUp    -> MoveUp
        KeycodeSpace -> ResetPosition
        _            -> MoveHalt
    MouseMotionEvent d ->
      case mouseMotionEventRelMotion d of
        V2 x y ->
          MoveMouse
            (fromIntegral x * sensitivity)
            (fromIntegral y * sensitivity)
    _ -> MoveHalt

radians deg = deg * pi / 180
