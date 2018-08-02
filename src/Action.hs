module Action where

import           SDL

data ProgramAction
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | MoveHalt
  | MoveMouse Float
              Float
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
        KeycodeRight -> MoveRight
        KeycodeLeft  -> MoveLeft
        KeycodeDown  -> MoveDown
        KeycodeUp    -> MoveUp
        KeycodeSpace -> ResetPosition
        KeycodeEscape -> QuitProgram
        _            -> MoveHalt
    MouseMotionEvent d ->
      case mouseMotionEventRelMotion d of
        V2 x y -> MoveMouse (fromIntegral x) (fromIntegral y)
    _ -> MoveHalt
