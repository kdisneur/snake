module Snake.UI.Console (clearScreen, frame) where

import Snake.UI (UIBoard, UIElement(..), elements, score, speed)
import Data.List (intercalate, replicate)

clearFromCursor :: String
clearFromCursor = escape [1] "J"

clearScreen :: UIBoard -> String
clearScreen uiBoard =  moveCursorToStart uiBoard ++ clearFromCursor

createFrame :: UIBoard -> String
createFrame board =  intercalate "\n" $ [border]
                                     ++ (innerFrame (elements board))
                                     ++ [border]
                                     ++ [scoreFrame (score board)]
                                     ++ [speedFrame (speed board)]
  where innerFrame = map displayLine
        border = replicate ((length (elements board)) + 2) '-'
        scoreFrame score = "Score: " ++ (show score)
        speedFrame speed = "Speed: " ++ (show speed)

escape :: [Int] -> String -> String
escape sequenceParameters controlFunction = "\ESC[" ++ controlSequence ++ controlFunction
  where controlSequence = (intercalate ";" . map show) sequenceParameters

displayLine ::  [UIElement] -> String
displayLine elements = "|" ++ (stringifiedElements elements) ++ "|"
  where stringifiedElements  = concat . map toString

frame :: UIBoard -> String
frame uiBoard = createFrame uiBoard

inverseBackground :: String -> String
inverseBackground content = (escape [7] "m") ++ content ++ (escape [27] "m")

moveCursorToStart :: UIBoard -> String
moveCursorToStart uiBoard = (escape [rows] "A") ++ (escape [1] "G")
  where rows = (length (elements uiBoard)) + 5

toString :: UIElement -> String
toString Empty = " "
toString Body = inverseBackground " "
toString Bean = "◉"
