module Main
  ( main
  ) where

import Prelude hiding (enumFromTo, filter, map, sum)

import Stream
import Text.Read (readMaybe)

main :: IO ()
main = do
  line <- getLine
  case readMaybe line of
    Just n -> print $ sumSquareEven n
    Nothing -> main

sumSquareEven :: Int -> Int
sumSquareEven = sum . map square . filter even . enumFromTo 1

square :: Int -> Int
square x = x * x
