module Main where

import Lib

main :: IO ()
main = do
  Just sol <- aquarium example
  putStrLn "Expected Board:"
  printBoard example solution
  putStrLn "Actual Board:"
  printBoard example sol
