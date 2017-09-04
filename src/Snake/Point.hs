module Snake.Point (Point, new, next, xAxis, yAxis) where

import Snake.Direction (Direction(..))

data Point = Point Int Int deriving (Show, Eq)

new :: Int -> Int -> Point
new = Point

next :: Int -> Direction -> Point -> Point
next size UP (Point x y) = Point x y'
  where y' = if y - 1  < 0 then size - 1 else y - 1
next size DOWN (Point x y) = Point x y'
  where y' = if y + 1  >= size then 0 else y + 1
next size LEFT (Point x y) = Point x' y
  where x' = if x - 1 < 0 then size - 1 else x - 1
next size RIGHT (Point x y) = Point x' y
  where x' = if x + 1 >= size then 0 else x + 1

xAxis :: Point -> Int
xAxis (Point x _) = x

yAxis :: Point -> Int
yAxis (Point _ y) = y
