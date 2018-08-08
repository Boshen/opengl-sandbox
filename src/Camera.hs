{-# LANGUAGE RecordWildCards #-}

module Camera where

import           Control.Monad              (foldM)
import qualified Control.Monad.State.Strict as State
import           Linear
import           SDL

import           Chunk                      (getBlock)
import           States

initialCamera :: Camera
initialCamera =
  Camera
    { cameraPos = V3 0 3 0
    , cameraFront = V3 0 0 (-1)
    , cameraUp = V3 0 1 0
    , cameraRotation = V2 (pi / 2) 0 -- pitch and yaw
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
        KeycodeSpace  -> MoveKeyboard $ V3 0 1 0
        KeycodeEscape -> QuitProgram
        _             -> MoveHalt
    MouseMotionEvent d ->
      case mouseMotionEventRelMotion d of
        V2 x y -> MoveMouse $ V2 (fromIntegral x) (fromIntegral $ -1 * y)
    _ -> MoveHalt

runCamera :: Float -> [Motion] -> Game ()
runCamera dt actions = mapM_ (changeCamera dt) (gravity : map motion actions)

gravity :: (V3 Float, V2 Float)
gravity = (V3 0 (-0.1) 0, V2 0 0)

motion :: Motion -> (V3 Float, V2 Float)
motion (MoveKeyboard pos) = (pos, V2 0 0)
motion (MoveMouse pos)    = (V3 0 0 0, pos)
motion _                  = (V3 0 0 0, V2 0 0)

changeCamera :: Float -> (V3 Float, V2 Float) -> Game ()
changeCamera dt (pos', V2 mx my) = do
  gameState@GameState {..} <- State.get
  let camera@Camera {..} = gameCamera
      cameraSpeed = 10 * dt
      sensitivity = 0.02 * dt
      V2 rx' ry' = cameraRotation
      rx = rx' + mx * sensitivity
      ry = max (-pi / 4) . min (pi / 4) $ ry' + my * sensitivity
      front = V3 (cos ry * cos rx) (sin ry) (sin rx * cos ry)
      right = cross front cameraUp
      dir = cameraSpeed *^ liftU2 (*) pos' (right + cameraUp + front)
  pos <- checkCollision cameraPos dir
  let updatedCamera =
        camera
          { cameraPos = pos
          , cameraFront =
              let (V3 x y z) = front
               in V3 x 0 z
          , cameraRotation = V2 rx ry
          , cameraViewMatrix = Linear.lookAt pos (pos ^+^ front) cameraUp
          , cameraProjectionMatrix =
              Linear.perspective (pi / 4) (800 / 600) 0.1 100.0
          }
  State.put gameState {gameCamera = updatedCamera}

checkCollision :: V3 Float -> V3 Float -> Game (V3 Float)
checkCollision pos@(V3 x y z) dir@(V3 dx dy dz) = do
  let xp = x + dx + 0.5 > fromIntegral (ceiling x)
      yp = y + dy + 0.5 > fromIntegral (ceiling y)
      zp = z + dz + 0.5 > fromIntegral (ceiling z)
      xn = x + dx - 0.5 < fromIntegral (floor x)
      yn = y + dy - 0.5 < fromIntegral (floor y)
      zn = z + dz - 0.5 < fromIntegral (floor z)
      V3 x' y' z' = floor <$> pos
      cond =
        [ (xp, V3 (-1.0) 0.0 0.0, V3 (x' + 1) y' z')
        , (xn, V3 1.0 0.0 0.0, V3 (x' - 1) y' z')
        , (zp, V3 0.0 0.0 (-1.0), V3 x' y' (z' + 1))
        , (zn, V3 0.0 0.0 1.0, V3 x' y' (z' - 1))
        , (yp, V3 0.0 (-1.0) 0.0, V3 x' (y' + 1) z')
        , (yn, V3 0.0 1.0 0.0, V3 x' (y' - 1) z')
        ]
      test dir (cond, norm, block)
        | not cond = return dir
        | otherwise = do
          block <- getBlock block
          case block of
            Nothing -> return dir
            Just _  -> return (dir ^-^ (norm ^* (norm `dot` dir)))
  (pos ^+^) <$> foldM test dir cond
