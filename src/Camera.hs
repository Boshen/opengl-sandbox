module Camera
  ( Camera(..)
  , initialCamera
  , updateCamera
  , getViewMatrix
  ) where

import           Data.Foldable
import           Linear

import           Action

data Camera = Camera
  { cameraPos   :: V3 Float
  , cameraFront :: V3 Float
  , cameraUp    :: V3 Float
  , cameraYaw   :: Float
  , cameraPitch :: Float
  , cameraFov   :: Float
  } deriving (Show)

initialCamera :: Camera
initialCamera =
  Camera
    { cameraPos = V3 0 0 5
    , cameraFront = V3 0 0 (-1)
    , cameraUp = V3 0 1 0
    , cameraYaw = -90
    , cameraPitch = 0
    , cameraFov = 45
    }

sensitivity = 0.05

updateCamera :: Camera -> [ProgramAction] -> Float -> Camera
updateCamera camera actions deltaTime =
  foldl' (addCamera deltaTime) camera actions

addCamera :: Float -> Camera -> ProgramAction -> Camera
addCamera deltaTime camera@(Camera pos front up yaw pitch fov) event =
  let cameraSpeed = 10 * deltaTime
   in case event of
        ResetPosition -> initialCamera
        Zoom dz -> Camera pos front up yaw pitch fov'
          where fov' = max 1 . min 45 $ fov + dz * cameraSpeed * 100
        MoveMouse dx dy -> Camera pos front' up yaw' pitch' fov
          where yaw' = yaw + dx * sensitivity
                pitch' = max (-89) . min 89 $ pitch + dy * sensitivity
                radYaw = radians yaw'
                radPitch = radians pitch'
                front' =
                  signorm $
                  V3
                    (cos radPitch * cos radYaw)
                    (sin radPitch)
                    (sin radYaw * cos radPitch)
        _ -> Camera (V3 x y z) front up yaw pitch fov -- set y = 0 to keep at ground level
          where (V3 x y z) =
                  case event of
                    MoveForward -> pos ^+^ (front ^* cameraSpeed)
                    MoveBackward -> pos ^-^ (front ^* cameraSpeed)
                    MoveUp -> pos ^+^ (up ^* cameraSpeed)
                    MoveDown -> pos ^-^ (up ^* cameraSpeed)
                    MoveLeft -> pos ^-^ signorm (cross front up) ^* cameraSpeed
                    MoveRight -> pos ^+^ signorm (cross front up) ^* cameraSpeed
                    MoveHalt -> pos
                    _ -> pos

radians deg = deg * pi / 180

getViewMatrix :: Camera -> M44 Float
getViewMatrix camera = Linear.lookAt pos (pos ^+^ front) up
  where
    pos = cameraPos camera
    front = cameraFront camera
    up = cameraUp camera
