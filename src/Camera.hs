module Camera
  ( Camera(..)
  , initialCamera
  , updateCamera
  , getViewMatrix
  ) where

import           Linear

import           Motion

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

updateCamera :: [Motion] -> Camera -> Float -> Camera
updateCamera actions = changeCamera (foldl sumActions (V3 0 0 0, V2 0 0) actions)

sumActions :: (V3 Float, V2 Float) -> Motion -> (V3 Float, V2 Float)
sumActions (pos, rot) action = case action of
    MoveKeyboard pos' -> (pos ^+^ pos', rot)
    MoveMouse rot' -> (pos, rot ^+^ rot')
    _ -> (pos, rot)

changeCamera :: (V3 Float, V2 Float) -> Camera -> Float  -> Camera
changeCamera (pos', V2 rx ry) Camera{..} dt =
  let cameraSpeed = 10 * dt
      yaw = cameraYaw + rx * sensitivity
      pitch = max (-89) . min 89 $ cameraPitch + ry * sensitivity
      radYaw = radians yaw
      radPitch = radians pitch
      front = signorm $ V3
        (cos radPitch * cos radYaw)
        (sin radPitch)
        (sin radYaw * cos radPitch)
      right = signorm (cross cameraFront cameraUp)
      pos = cameraPos ^+^ cameraSpeed *^ liftU2 (*) pos' (right + cameraUp + cameraFront)

  in
    Camera
        { cameraPos = pos
        , cameraFront = front
        , cameraUp = cameraUp
        , cameraYaw = yaw
        , cameraPitch = pitch
        , cameraFov = cameraFov
        }

radians deg = deg * pi / 180

getViewMatrix :: Camera -> M44 Float
getViewMatrix camera = Linear.lookAt pos (pos ^+^ front) up
  where
    pos = cameraPos camera
    front = cameraFront camera
    up = cameraUp camera
