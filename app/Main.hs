{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Main where
import Debug.Trace
import FRP.Yampa
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.FRP.Yampa
import qualified Graphics.Gloss.Interface.IO.Game as G

-------------------
-- Display の設定
-------------------

windowWidth, windowHeight :: Num a => a
windowWidth  = 1024
windowHeight = 576

window :: Display
window = InWindow "Hello World" (windowWidth, windowHeight) (100, 100)

type Direction = Point
type Acceralation = Point
type Position = Point
type Velocity = Point
data State =
  CircleState {
    pos :: Position
  } |
  ShotState {
    pos :: Position,
    dire :: Direction
  } |
  PlayerState {
    pos :: Position
  } deriving (Eq)

--------------------------
-- シミュレーションの実装
--------------------------

boxWidth, boxHeight :: Float
boxWidth  = 50
boxHeight = 50

d2f = fromRational.toRational :: Double -> Float

keyToVec :: InputEvent -> Direction
keyToVec (G.EventKey key state _ _)
  | key == G.SpecialKey G.KeyUp = toVec state (0, 1)
  | key == G.SpecialKey G.KeyDown = toVec state (0, -1)
  | key == G.SpecialKey G.KeyRight  = toVec state (1, 0)
  | key == G.SpecialKey G.KeyLeft  = toVec state (-1, 0)
  | otherwise = (0, 0)
  where toVec G.Down   v = v
        toVec G.Up (x,y) = (-x, -y)
keyToVec _ = (0, 0)

data ReflectV = ReflectX | ReflectY

player :: (Acceralation, Position, Velocity) -> SF (Event InputEvent) State
player ((a0x, a0y), (p0x, p0y), (v0x, v0y)) = dSwitch loop player
  where
    loop = proc ie -> do
      let dir = fmap keyToVec ie
      a <- impulseIntegral -< ((0, 0), fmap (50 P.*) dir)
      v <- (\(x, y) -> (v0x + x, v0y + y)) ^<< integral -< a
      p <- (\(x, y) -> (p0x + x, p0y + y)) ^<< integral -< v
      let event
            | x < -tx = Event (a, (-tx, y), (-vx, vy))
            | x > tx = Event (a, (tx, y), (-vx, vy))
            | y < -ty = Event (a, (x, -ty), (vx, -vy))
            | y > ty = Event (a, (x, ty), (vx, -vy))
            | otherwise = NoEvent 
            where (tx, ty) = (windowWidth / 2 - boxWidth, windowHeight / 2 - boxHeight)
                  (x, y) = p
                  (vx, vy) = v
      returnA -< (PlayerState { pos = p }, event )

shot :: (Position, Velocity) -> SF () State
shot (p, v) = proc () -> do
  returnA -< ShotState { pos = (0, 0), dire = (0, 0) }

shots :: [State] -> SF (Event InputEvent) [State]
shots ls = dSwitch f shots
  where
    f = proc ie -> do
      let onShot = tag (filterE check ie) 
      returnA -< (ls, )
      where
        check (G.EventKey c state _ _) = c == G.Char 'z' && state == G.Down
        check _ = False

showPicture :: State -> Picture
showPicture state =
  let
    (x, y) = pos state
    trans = translate x y
    rot = rotate $ (\(x, y) -> atan2 y x).dire $ state
    showP :: State -> Picture
    showP CircleState {} = trans $ circle 10
    showP PlayerState {} = trans $ rectangleSolid 50 50
    showP ShotState {} = rot.trans $ rectangleSolid 10 20
  in showP state

showPictures :: [State] -> Picture
showPictures states = foldl (<>) blank $ map showPicture states

sf :: SF (Event InputEvent) Picture
sf = proc ie -> do
  player <- player ((0, 0), (0, 0), (0, 0)) -< ie
  let states = [player]
  let pic = foldl (<>) blank $ map showPicture states
  returnA -< pic

-------------
-- main 関数
-------------

main :: IO ()
main = playYampa window white 240 sf

