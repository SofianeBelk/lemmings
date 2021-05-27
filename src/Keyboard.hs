module Keyboard where

import SDL

import Data.List (foldl')

import Data.Set (Set)
import qualified Data.Set as S

type Keyboard = Set Keycode

createKeyboard :: Keyboard
createKeyboard = S.empty

handleEvent :: Event -> Keyboard -> Keyboard
handleEvent event kbd =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      if keyboardEventKeyMotion keyboardEvent == Pressed
      then S.insert (keysymKeycode (keyboardEventKeysym keyboardEvent)) kbd
      else if keyboardEventKeyMotion keyboardEvent == Released
           then S.delete (keysymKeycode (keyboardEventKeysym keyboardEvent)) kbd
           else kbd
    _ -> kbd

handleEvents :: [Event] -> Keyboard -> Keyboard
handleEvents events kbd = foldl' (flip handleEvent) kbd events

keypressed :: Keycode -> Keyboard -> Bool
keypressed = S.member
