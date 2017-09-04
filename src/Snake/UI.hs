module Snake.UI (UIBoard, UIElement(..), createBoard, elements, score, speed) where

import Snake.Point (Point, xAxis, yAxis)
import Snake.Speed (Speed)

data UIElement
  = Empty
  | Body
  | Bean

type GridSize = Int
data UIBoard = UIBoard [[UIElement]] Int Speed

createBoard :: GridSize -> Point -> [Point] -> Int -> Speed -> UIBoard
createBoard size beanPoint bodyPoints score speed = uiBoard
  where
    beanPoint' = (Bean, beanPoint)
    bodyPoints' = (fmap ((,) Body) bodyPoints)
    uiBoard = addPoints (emptyBoard size score speed) (beanPoint':bodyPoints')

elements :: UIBoard -> [[UIElement]]
elements (UIBoard uiElements _ _) = uiElements

emptyBoard :: GridSize -> Int -> Speed -> UIBoard
emptyBoard size score speed = UIBoard elements score speed
  where lines = replicate size Empty
        elements = replicate size lines

addPoints :: UIBoard -> [(UIElement, Point)] -> UIBoard
addPoints board elements = foldl addPoint board elements

addPoint :: UIBoard  -> (UIElement, Point) -> UIBoard
addPoint (UIBoard uiElements score speed) (element, point) = UIBoard elements' score speed
  where (previousLines, elements:nextLines) = splitAt (yAxis point) uiElements
        (previousColumns, _:nextColumns) = splitAt (xAxis point) elements
        newLine = previousColumns ++ [element] ++ nextColumns
        elements' = previousLines ++ [newLine] ++ nextLines

score :: UIBoard -> Int
score (UIBoard _ score _) = score

speed :: UIBoard -> Speed
speed (UIBoard _ _ speed) = speed
