module PhysicalDimensions (
  GameReal,

-- One dimensional
  Time,
  DTime,
  Length,
  Distance,
  Position,
  Velocity,
  Acceleration,

-- Two dimensional
  Position2,
  Velocity2,
  Acceleration2,
) where

import FRP.Yampa (Time, DTime)
import Data.Point2
import Data.Vector2

type GameReal = Time

type Length       = GameReal
type Position     = GameReal
type Distance     = GameReal
type Velocity     = GameReal
type Acceleration = GameReal

type Position2     = Point2 Position
type Distance2     = Vector2 Distance
type Velocity2     = Vector2 Velocity
type Acceleration2 = Vector2 Acceleration
