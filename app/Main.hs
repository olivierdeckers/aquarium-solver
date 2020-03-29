module Main where

import Lib
import Data.Holmes
import Data.List.Split (chunksOf)
import Puzzle
import qualified Data.JoinSemilattice.Intersect as I

main :: IO ()
main = do
  Just sol <- solve example4
  printBoard example4 sol

printBoard :: Puzzle -> [ Intersect AquariumCell ] -> IO ()
printBoard p cells = mapM_ putStrLn $ chunksOf (_size p) $ map (showCell . I.toList) cells
  where showCell [Air]   = '_'
        showCell [Water] = 'x'

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

example4 = Puzzle {
    _size = 10
  , _colSums = [4, 3, 8, 9, 8, 6, 6, 8, 7, 2]
  , _rowSums = [7, 6, 8, 8, 3, 4, 7, 5, 4, 9]
  , _puzzleRows = [
    [1, 5, 1, 2, 1],
    [1, 1, 1, 2, 1, 2, 1, 1],
    [2, 1, 2, 1, 3, 1],
    [2, 3, 2, 2, 1],
    [1, 1, 1, 2, 2, 2, 1],
    [1, 1, 3, 1, 3, 1],
    [1, 1, 1, 1, 2, 1, 2, 1],
    [1, 1, 2, 3, 2, 1],
    [1, 1, 1, 3, 1, 1, 1, 1],
    [3, 2, 1, 1, 1, 1, 1]
  ]
  , _puzzleCols = [
    [2, 2, 2, 3, 1]
    , [1, 4, 4, 1]
    , [2, 3, 1, 2, 2]
    , [1, 2, 1, 1, 2, 1, 1, 1]
    , [1, 2, 1, 1, 1, 1, 2, 1]
    , [1, 2, 1, 1, 2, 3]
    , [2, 1, 1, 1, 2, 1, 2]
    , [1, 1, 1, 1, 2, 1, 1, 2]
    , [1, 1, 1, 1, 2, 1, 1, 2]
    , [4, 2, 2, 2]
  ]
}
