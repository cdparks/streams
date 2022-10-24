module Main
  ( main
  ) where

import Prelude hiding (enumFromTo, filter, map, sum)

import Stream
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStr "[integer]> "
  hFlush stdout
  line <- getLine
  case readMaybe line of
    Just n -> putStrLn $ concat
      [ "sumSquareEven "
      , show n
      , " => "
      , show $ sumSquareEven n
      ]
    Nothing -> main

sumSquareEven :: Int -> Int
sumSquareEven = sum . map square . filter even . enumFromTo 1
 where
  square x = x * x
