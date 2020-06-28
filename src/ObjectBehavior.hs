{-# LANGUAGE Arrows #-}

module ObjectBehavior (
  box,
) where

import FRP.Yampa
import Data.AffineSpace
import Data.Point2
import Data.Vector2

import Input
import PhysicalDimensions
import Object

box :: Position2 -> Object
box p0 = proc (ObjInput {oiGameInput = gi}) -> do
  a <- (50 *^) ^<< keyArrow -< gi
  v <- integral -< a
  p <- (p0 .+^) ^<< integral -< v
  returnA -< ObjOutput {
    ooObsObjState = oosBox p (vector2 0 0)
  }
