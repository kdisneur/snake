module Main where

import Control.Exception (bracket)
import Data.List (find, isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import Snake.Game (play)
import Snake.Speed (Speed, mkSpeed, minSpeed)
import System.Environment (getArgs)
import System.IO
import Text.Read (readMaybe)

main :: IO ()
main =
  bracket
  (do prevBuff <- hGetBuffering stdin
      prevEcho <- hGetEcho stdin

      hSetBuffering stdin NoBuffering
      hSetEcho stdin False

      return (prevBuff, prevEcho))

  (\(prevBuff, prevEcho) -> do
    hSetBuffering stdin prevBuff
    hSetEcho stdin prevEcho)

  (\_ -> do
    size <- getSize 10
    speed <- getSpeed minSpeed
    play size speed)

getSize :: Int -> IO Int
getSize defaultValue = do
  rawValue <- getParam "--size="
  value <- return (rawValue >>= readMaybe :: Maybe Int)
  return (fromMaybe defaultValue value)

getSpeed :: Speed -> IO Speed
getSpeed defaultValue = do
    rawValue <- getParam "--speed="
    value <- return (rawValue >>= readMaybe :: Maybe Int)
    return (fromMaybe defaultValue $ value >>= mkSpeed)

getParam :: String -> IO (Maybe String)
getParam param = do
  parameters <- getArgs
  parameter <- return $ find (isPrefixOf param) parameters
  value <- return $ parameter >>= (stripPrefix param)
  return value
