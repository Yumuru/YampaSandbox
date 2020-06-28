module RenderObject (
  renderObjects
) where

import Graphics.Gloss
import Data.Point2

import Object

renderObjects :: [ObsObjState] -> Picture
renderObjects ooss = pictures (map renderObject ooss)

r2f = realToFrac

renderObject :: ObsObjState -> Picture
renderObject (OOSBox { oosPos = (Point2 px py), oosLength = l }) =
  translate (r2f px) (r2f py) $ rectangleSolid (r2f l) (r2f l)
