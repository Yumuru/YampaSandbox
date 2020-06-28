{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Debug.Trace
import FRP.Yampa
import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.Yampa
import Graphics.Gloss.Interface.IO.Game hiding (Event)
import Data.Point2
import Data.Maybe

import IdentityList
import Object
import ObjectBehavior
import Input
import PhysicalDimensions

windowWidth, windowHeight :: Num a => a
windowWidth = 1024
windowHeight = 576

window :: Display
window = InWindow "Hello world" (windowWidth, windowHeight) (100, 100)

r2f = realToFrac

square :: Position2 -> Float -> Picture
square (Point2 px py) l = translate (r2f px) (r2f py) $ rectangleSolid l l

game :: SF GameInput [ObsObjState]
game = proc gi -> do
  rec
    oos <- game' objs0 -< (gi, oos)
  returnA -< map ooObsObjState (elemsIL oos)
  where
    objs0 = listToIL []

    game' :: IL Object -> SF (GameInput, IL ObjOutput) (IL ObjOutput)
    game' objs = dpSwitch route
                          (objs :: IL Object)
                          test
                          k
    route :: (GameInput, IL ObjOutput) -> IL sf -> IL (ObjInput, sf)
    route (gi, oos) objs = mapIL routeAux objs
      where
        routeAux (k, obj) = (ObjInput { oiGameInput = gi }, obj)

    test :: SF ((GameInput, IL ObjOutput), IL ObjOutput) (Event (IL Object -> IL Object))
    test = noEvent --> arr killOrSpawn

    k :: IL Object -> (IL Object -> IL Object) -> SF (GameInput, IL ObjOutput) (IL ObjOutput)
    k sfs' f = game' (f sfs')

    killOrSpawn :: (a, IL ObjOutput) -> (Event (IL Object -> IL Object))
    killOrSpawn (_, oos) = 
      foldl (mergeBy (.)) noEvent es
      where 
        es :: [Event (IL Object -> IL Object)]
        es = [ mergeBy (.)
                       ()]
      
onClick :: SF (Event InputEvent) (Event Position2)
onClick = proc ie -> do
  returnA -< mapFilterE isClick ie

isClick :: InputEvent -> Maybe Position2
isClick (EventKey (MouseButton LeftButton) Down _ (x, y)) = Just (Point2 (r2f x) (r2f y))
isClick _ = Nothing

sf :: SF (Event InputEvent) Picture
sf = proc ie -> do
  returnA -< square (Point2 30 30) 50

main :: IO ()
main = playYampa window white 240 sf
