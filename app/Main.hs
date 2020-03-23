module Main where

import Lib
import Data.Holmes
import Data.List.Split (chunksOf)

main :: IO ()
main = do
  Just sol <- solve example
  printBoard example sol

printBoard :: Puzzle -> [ Defined AquariumCell ] -> IO ()
printBoard p cells = mapM_ putStrLn $ chunksOf (_size p) $ map showCell cells
  where showCell (Exactly Air)   = '_'
        showCell (Exactly Water) = 'x'

example = Puzzle {
  _size = 6,
  _colSums = [1, 5, 4, 5, 4, 4],
  _rowSums = [2, 3, 4, 4, 5, 5],
  _puzzleRows = [
      [2, 2, 1, 1],
      [1, 3, 1, 1],
      [1, 1, 1, 1, 1, 1],
      [2, 1, 1, 1, 1],
      [1, 2, 1, 1, 1],
      [1, 1, 4]
    ],
  _puzzleCols = [
      [1, 5],
      [1, 2, 1, 2],
      [2, 3, 1],
      [3, 3],
      [5, 1],
      [6]
    ]
}
