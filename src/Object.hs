module Object (
  Object,
  ObjInput(..),
  ObjOutput(..),
  ObsObjState(..),
  oosBox,
  isBox,
  touches,
  approaches,
  colliding,
) where

import Graphics.Gloss
import Data.AffineSpace
import FRP.Yampa 
import Graphics.Gloss.Interface.FRP.Yampa

import Input
import PhysicalDimensions

type Object = SF ObjInput ObjOutput

data ObjInput = ObjInput {
  oiGameInput :: GameInput
}
data ObjOutput = ObjOutput {
  ooObsObjState :: !ObsObjState
}

data ObsObjState =
    OOSBox {
      oosPos :: !Position2,
      oosVel :: !Velocity2,
      oosLength :: !Length,
      oosRadius :: !Length
    }
  
oosBox :: Position2 -> Velocity2 -> ObsObjState
oosBox p v = OOSBox {
    oosPos = p,
    oosVel = v,
    oosLength = boxRadius,
    oosRadius = boxRadius
  }

isBox :: ObsObjState -> Bool
isBox (OOSBox {}) = True
isBox _           = False

touches :: ObsObjState -> ObsObjState -> Bool
oos1 `touches` oos2 =
  norm ((oosPos oos2) .-. (oosPos oos1)) < (oosRadius oos2 + oosRadius oos1)

approaches :: ObsObjState -> ObsObjState -> Bool
oos1 `approaches` oos2 =
  (oosVel oos2 ^-^ oosVel oos1) `dot` (oosPos oos2 .-. oosPos oos1) < 0.0

colliding :: ObsObjState -> ObsObjState -> Bool
oos1 `colliding` oos2 = oos1 `touches` oos2 && oos1 `approaches` oos2

boxRadius :: Length
boxRadius = 25
