module Motion where

import           SDL

data Motion
  = MoveKeyboard (V3 Float)
  | MoveMouse (V2 Float)
  | MoveHalt
  | QuitProgram
  deriving (Show, Eq)

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
