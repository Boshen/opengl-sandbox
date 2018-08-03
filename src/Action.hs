module Action where

import           SDL

data ProgramAction
  = MoveForward
  | MoveBackward
  | MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | MoveHalt
  | MoveMouse Float
              Float
  | Zoom Float
  | ResetPosition
  | QuitProgram
  deriving (Eq)

parseEvents :: [Event] -> [ProgramAction]
parseEvents = map parseEvent

parseEvent :: Event -> ProgramAction
parseEvent event =
  case eventPayload event of
    QuitEvent -> QuitProgram
    KeyboardEvent d ->
      case keysymKeycode . keyboardEventKeysym $ d of
        KeycodeD      -> MoveRight
        KeycodeA      -> MoveLeft
        KeycodeW      -> MoveForward
        KeycodeS      -> MoveBackward
        KeycodeQ      -> MoveUp
        KeycodeE      -> MoveDown
        KeycodeO      -> Zoom (-1.0)
        KeycodeP      -> Zoom 1.0
        KeycodeSpace  -> ResetPosition
        KeycodeEscape -> QuitProgram
        _             -> MoveHalt
    MouseMotionEvent d ->
      case mouseMotionEventRelMotion d of
        V2 x y -> MoveMouse (fromIntegral x) (fromIntegral $ -1 * y)
    _ -> MoveHalt
