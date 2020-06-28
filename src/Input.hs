{-# LANGUAGE Arrows #-}

module Input (
  GameInput,
  gameInput,
  keyArrow,
  inputEvent,
) where

import FRP.Yampa
import Data.Vector2
import Graphics.Gloss.Interface.IO.Game hiding (Event)
import Graphics.Gloss.Interface.FRP.Yampa

import PhysicalDimensions

data GameInput = GameInput {
  giKeyArrow :: Vector2 GameReal,
  giInputEvent :: Event InputEvent
}

gameInput :: SF (Event InputEvent) GameInput
gameInput = proc ie -> do
  arrow <- impulseIntegral -< (vector2 0 0, toVec ie)
  returnA -< GameInput {
    giKeyArrow = arrow,
    giInputEvent = ie
  }
  where
    toVec :: Event InputEvent -> Event (Vector2 GameReal)
    toVec (Event (EventKey k state _ _)) = maybe NoEvent (sTv state) (keyToVec k)
    toVec _ = NoEvent
    sTv Down v = Event v
    sTv Up v = Event $ (-1) *^ v

keyArrow :: SF GameInput (Vector2 GameReal)
keyArrow = arr giKeyArrow

keyToVec :: Key -> Maybe (Vector2 GameReal)
keyToVec (Char 'w') = Just (vector2   0   1 )
keyToVec (Char 'a') = Just (vector2 (-1)  0 )
keyToVec (Char 's') = Just (vector2   0 (-1))
keyToVec (Char 'd') = Just (vector2   1   0 )
keyToVec (SpecialKey KeyUp)    = Just (vector2   0    1 )
keyToVec (SpecialKey KeyLeft)  = Just (vector2 (-1)   0 )
keyToVec (SpecialKey KeyDown)  = Just (vector2   0  (-1))
keyToVec (SpecialKey KeyRight) = Just (vector2   1    0 )
keyToVec _ = Nothing

inputEvent :: GameInput -> Event InputEvent
inputEvent = giInputEvent
