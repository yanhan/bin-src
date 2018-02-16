module Main where

import Conduit (sinkList)
import Data.Conduit ((.|), runConduitRes)
import System.Environment (getArgs)

import Lib (doubleFromFileC)

main :: IO ()
main = do
  args <- getArgs
  case args of
    inputFile:_ -> do
      y <- runConduitRes $ doubleFromFileC inputFile .| sinkList
      print $ sum y
    _ -> print "Please supply the file with the list of numbers to sum"
