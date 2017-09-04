module Snake.Game (play) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, takeMVar, putMVar)
import Snake.Direction (Direction(..))
import qualified Snake.Point as P
import Snake.Snake (Snake, currentDirection, hasFullSize, hasCollision, increase, move, new, positions, updateDirection)
import Snake.Speed (Speed(..), toInt)
import Snake.UI (createBoard)
import Snake.UI.Console (clearScreen, frame)
import System.Random (StdGen, newStdGen, randomR)

type Bean = P.Point
type State = (Int, Speed, StdGen, MVar Direction, Bean, Snake, Int)

availableSpaces :: Int -> Snake -> [P.Point]
availableSpaces size snake = [P.new x y | x <- [0..(size - 1)],
                                          y <- [0..(size - 1)],
                                          isAvailableSpace (P.new x y)]
  where isAvailableSpace point  = not (point `elem` usedSpaces)
        usedSpaces = positions snake

beanIsEaten :: P.Point -> [P.Point] -> Bool
beanIsEaten point (head:_) = point == head
beanIsEaten point _ = False

generatePoint :: StdGen -> [P.Point] -> (P.Point, StdGen)
generatePoint generator points = (point, generator')
  where (index, generator') = randomR (0, (length points)) generator
        point = points !! index

initialize :: Int -> Speed -> IO State
initialize size speed = do
  let snake = (increase size) . (increase size) $ new
  generator <- newStdGen
  let (bean, generator') = generatePoint generator (availableSpaces size snake)
  mutableDirection <- newMVar (currentDirection snake)
  return (size, speed, generator', mutableDirection, bean, snake, 0)

newDirection :: Char -> Direction -> Direction
newDirection 'w' _ = UP
newDirection 'a' _ = LEFT
newDirection 's' _ = DOWN
newDirection 'd' _ = RIGHT
newDirection _ direction = direction

play :: Int -> Speed -> IO ()
play size speed = do
  state@(_, _, _, mutableDirection, _, _, _) <- initialize size speed
  forkIO (readInputLoop mutableDirection)
  renderLoop state
  return ()

readInputLoop :: MVar Direction -> IO ()
readInputLoop mutableDirection = do
  input <- getChar
  direction <- takeMVar mutableDirection
  let direction' = newDirection input direction
  putMVar mutableDirection direction'
  readInputLoop mutableDirection

renderLoop :: State -> IO State
renderLoop (size, speed, generator, mutableDirection, bean, snake, score) = do
  let uiBoard = createBoard size bean (positions snake) score speed
  putStrLn (frame uiBoard)
  threadDelay (speedToMicroseconds speed)
  direction <- readMVar mutableDirection
  let snakePositions = positions snake
  let beanEaten = beanIsEaten bean snakePositions
  let score' = if beanEaten then score + (toInt speed) else score
  let snake' = if beanEaten then (increase size . updateDirection direction) snake else (move size . updateDirection direction) snake
  let (bean', generator') = if beanEaten then generatePoint generator (availableSpaces size snake) else (bean, generator)
  let newState = (size, speed, generator', mutableDirection, bean', snake', score')
  if hasCollision snake
     then do
       putStrLn "Game Over!"
       return newState
     else if hasFullSize size snake
          then do
            putStrLn "You Win!"
            return newState
          else do
            putStrLn (clearScreen uiBoard)
            renderLoop newState

speedToMicroseconds :: Speed -> Int
speedToMicroseconds speed = slowestMicrosecond - ((toInt speed) * gapMicrosecond)

slowestMicrosecond :: Int
slowestMicrosecond = 500000

gapMicrosecond :: Int
gapMicrosecond = 50000
