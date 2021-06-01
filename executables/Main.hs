module Main
  ( main
  ) where

import Prelude

import qualified Stream as Stream
import Text.Read (readMaybe)

main :: IO ()
main = do
  line <- getLine
  case readMaybe line of
    Just n -> print $ sumSquareEven n
    Nothing -> main

sumSquareEven :: Int -> Int
sumSquareEven =
  Stream.sum . Stream.map square . Stream.filter even . Stream.enumFromTo 1

square :: Int -> Int
square x = x * x
