{-# LANGUAGE RecordWildCards #-}

module Camera where

import           Linear
import           SDL

data Camera = Camera
  { cameraPos              :: V3 Float
  , cameraFront            :: V3 Float
  , cameraUp               :: V3 Float
  , cameraRotation         :: V2 Float
  , cameraFov              :: Float
  , cameraViewMatrix       :: M44 Float
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
    { cameraPos = V3 0 10 5
    , cameraFront = V3 0 0 (-1)
    , cameraUp = V3 0 1 0
    , cameraRotation = V2 0 (-pi / 4) -- pitch and yaw
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

updateCamera :: [Motion] -> Camera -> Float -> Camera
updateCamera actions camera dt =
  foldl (changeCamera dt) camera (map motion actions)

motion :: Motion -> (V3 Float, V2 Float)
motion (MoveKeyboard pos) = (pos, V2 0 0)
motion (MoveMouse pos)    = (V3 0 0 0, pos)
motion _                  = (V3 0 0 0, V2 0 0)

changeCamera :: Float -> Camera -> (V3 Float, V2 Float) -> Camera
changeCamera dt camera@Camera {..} (pos', V2 mx my) =
  let cameraSpeed = 10 * dt
      sensitivity = 0.2 * dt
      V2 rx' ry' = cameraRotation
      rx = rx' + mx * sensitivity
      ry = max (-pi / 4) . min (pi / 4) $ ry' + my * sensitivity
      front = signorm $ V3 (cos ry * cos rx) (sin ry) (sin rx * cos ry)
      right = signorm $ cross front cameraUp
      pos =
        cameraPos ^+^ cameraSpeed *^
        liftU2 (*) pos' (right + cameraUp + cameraFront)
   in camera
        { cameraPos = pos
        , cameraFront =
            let (V3 x y z) = front
             in V3 x 0 z
        , cameraRotation = V2 rx ry
        , cameraViewMatrix = Linear.lookAt pos (pos ^+^ front) cameraUp
        , cameraProjectionMatrix =
            Linear.perspective (pi / 4) (800 / 600) 0.1 100.0
        }
