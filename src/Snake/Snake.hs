module Snake.Snake (Snake, currentDirection, hasFullSize, hasCollision, increase, move, new, positions, updateDirection) where

import qualified Snake.Point as P
import Snake.Direction (Direction(..))

type GridSize = Int

data Snake = Snake Direction [P.Point] deriving Show

currentDirection :: Snake -> Direction
currentDirection (Snake direction _) = direction

hasCollision :: Snake -> Bool
hasCollision (Snake _ points) = any hasCollision' points
  where hasCollision' point = [point] /= filter ((==) point) points

hasFullSize :: GridSize -> Snake -> Bool
hasFullSize gridSize = ((==) fullSize) . length . positions
  where fullSize = gridSize * gridSize

increase :: GridSize -> Snake -> Snake
increase gridSize (Snake direction []) = Snake direction [P.new position position]
  where position = quot gridSize 2
increase gridSize (Snake direction (point:points)) = Snake direction points'
  where nextPoint = P.next gridSize direction point
        points' = nextPoint:point:points

move :: GridSize -> Snake -> Snake
move gridSize snake = Snake (currentDirection snake) points'
  where snake' = increase gridSize snake
        points' = (reverse . drop 1 . reverse . positions) snake'

new :: Snake
new = Snake RIGHT []

positions :: Snake -> [P.Point]
positions (Snake _ points) = points

updateDirection :: Direction -> Snake -> Snake
updateDirection direction (Snake _ points) = Snake direction points
