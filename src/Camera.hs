{-# LANGUAGE RecordWildCards #-}

module Camera where

import           Linear
import           SDL

data Camera = Camera
  { cameraPos   :: V3 Float
  , cameraFront :: V3 Float
  , cameraUp    :: V3 Float
  , cameraYaw   :: Float
  , cameraPitch :: Float
  , cameraFov   :: Float
  , cameraViewMatrix :: M44 Float
  , cameraProjectionMatrix :: M44 Float
  } deriving (Show)

data Motion
  = MoveKeyboard (V3 Float)
  | MoveMouse (V2 Float)
  | MoveHalt
  | QuitProgram
  deriving (Show, Eq)

initialCamera :: Camera
initialCamera =
  Camera
    { cameraPos = V3 0 0 5
    , cameraFront = V3 0 0 (-1)
    , cameraUp = V3 0 1 0
    , cameraYaw = -90
    , cameraPitch = 0
    , cameraFov = 45
    , cameraViewMatrix = identity :: M44 Float
    , cameraProjectionMatrix = identity :: M44 Float
    }

parseEvents :: [Event] -> [Motion]
parseEvents = map parseEvent

parseEvent :: Event -> Motion
parseEvent event =
  case eventPayload event of
    QuitEvent -> QuitProgram
    KeyboardEvent d ->
      case keysymKeycode . keyboardEventKeysym $ d of
        KeycodeD      -> MoveKeyboard $ V3 1 0 0
        KeycodeA      -> MoveKeyboard $ V3 (-1) 0 0
        KeycodeW      -> MoveKeyboard $ V3 0 0 1
        KeycodeS      -> MoveKeyboard $ V3 0 0 (-1)
        KeycodeQ      -> MoveKeyboard $ V3 0 1 0
        KeycodeE      -> MoveKeyboard $ V3 0 (-1) 0
        KeycodeEscape -> QuitProgram
        _             -> MoveHalt
    MouseMotionEvent d ->
      case mouseMotionEventRelMotion d of
        V2 x y -> MoveMouse $ V2 (fromIntegral x) (fromIntegral $ -1 * y)
    _ -> MoveHalt

sensitivity :: Float
sensitivity = 0.05

updateCamera :: [Motion] -> Camera -> Float -> Camera
updateCamera actions = changeCamera (foldl sumMotion (V3 0 0 0, V2 0 0) actions)

sumMotion :: (V3 Float, V2 Float) -> Motion -> (V3 Float, V2 Float)
sumMotion (pos, rot) action =
  case action of
    MoveKeyboard pos' -> (pos ^+^ pos', rot)
    MoveMouse rot'    -> (pos, rot ^+^ rot')
    _                 -> (pos, rot)

changeCamera :: (V3 Float, V2 Float) -> Camera -> Float -> Camera
changeCamera (pos', V2 rx ry) Camera {..} dt =
  let cameraSpeed = 10 * dt
      yaw = cameraYaw + rx * sensitivity
      pitch = max (-89) . min 89 $ cameraPitch + ry * sensitivity
      radYaw = yaw * pi / 180
      radPitch = pitch * pi / 180
      front =
        signorm $
        V3
          (cos radPitch * cos radYaw)
          (sin radPitch)
          (sin radYaw * cos radPitch)
      right = signorm (cross cameraFront cameraUp)
      pos =
        cameraPos ^+^ cameraSpeed *^
        liftU2 (*) pos' (right + cameraUp + cameraFront)
   in Camera
        { cameraPos = pos
        , cameraFront = front
        , cameraUp = cameraUp
        , cameraYaw = yaw
        , cameraPitch = pitch
        , cameraFov = cameraFov
        , cameraViewMatrix = Linear.lookAt pos (pos ^+^ front) cameraUp
        , cameraProjectionMatrix = Linear.perspective (cameraFov * pi / 180.0) (800 / 600) 0.1 100.0
        }
