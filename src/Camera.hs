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

import           Actions

data Camera = Camera
  { cameraPos   :: V3 Float
  , cameraFront :: V3 Float
  , cameraUp    :: V3 Float
  , cameraYaw   :: Float
  , cameraPitch :: Float
  } deriving (Show)

initialCamera :: Camera
initialCamera = Camera (V3 0 0 3) (V3 0 0 (-1)) (V3 0 1 0) (-90) 0

sensitivity = 0.05

updateCamera :: Camera -> [ProgramAction] -> Float -> Camera
updateCamera camera actions deltaTime =
  foldl' (addCamera deltaTime) camera actions

addCamera :: Float -> Camera -> ProgramAction -> Camera
addCamera deltaTime camera@(Camera pos front up yaw pitch) event =
  case event of
    ResetPosition -> initialCamera
    MoveMouse dx dy -> Camera pos front' up yaw' pitch'
      where yaw' = yaw + dx * sensitivity
            pitch' = max (-89) . min 89 $ pitch + dy * sensitivity
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

radians deg = deg * pi / 180
