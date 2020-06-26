{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Test where
import Debug.Trace
import FRP.Yampa
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.FRP.Yampa
import qualified Graphics.Gloss.Interface.IO.Game as G

sf2 :: [Double] -> SF () [Double]
sf2 ls = dSwitch loop sf2
  where
    loop = proc () -> do
      returnA -< ([], NoEvent)


sf :: (Double, Double) -> SF () Double
sf (p0, v0) = switch loop sf
  where
    loop = proc () -> do
      p <- (+p0) ^<< integral -< v0
      let event 
            | p < -1 = Event (-1, -v0)
            | p > 1 = Event (1, -v0)
            | otherwise = NoEvent 
      returnA -< (p, event)

main = reactimate (return ()) sence out (sf (0, 0.1))
  where
    sence b = return (0.01, Nothing)
    out b v = print v >> return False

