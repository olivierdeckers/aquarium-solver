module Puzzle where

import           Control.Monad (guard)
import           Data.List     (foldl1', nub, sort, transpose)

data Puzzle = Puzzle
  { _size       :: Int
  , _colSums    :: [Int]
  , _rowSums    :: [Int]
  , _puzzleRows :: [[Int]]
  , _puzzleCols :: [[Int]]
  }

calculateRows :: Puzzle -> [x] -> [[x]]
calculateRows _ [] = []
calculateRows p xs = take s xs : calculateRows p (drop s xs)
  where
    s = _size p

calculateCols :: Puzzle -> [x] -> [[x]]
calculateCols p xs = transpose $ calculateRows p xs

groupsWithDepth :: Puzzle -> [x] -> [[(x, Int)]]
groupsWithDepth p xs =
  let s = _size p
      groupPositions = calculateGroupPositions p
   in do ps <- groupPositions
         return $ do
           (x, y) <- ps
           return (xs !! (y * s + x), y)

type Pos = (Int, Int)

calculatePositions :: Puzzle -> [Pos]
calculatePositions Puzzle {_size = s} = do
  x <- init [0 .. s]
  y <- init [0 .. s]
  return (x, y)

calculateGroupPositions :: Puzzle -> [[Pos]]
calculateGroupPositions p = nub $ map (\x -> nub . sort $ buildGroup [x]) (calculatePositions p)
  where
    buildGroup :: [Pos] -> [Pos]
    buildGroup ps =
      let newPositions = do
            (x, y) <- ps
            let colGroupNumber = colGroupNumbers p !! x !! y
            let rowGroupNumber = rowGroupNumbers p !! y !! x
            neighbour@(nx, ny) <- neighbours p (x, y)
            let nColGroupNumber = colGroupNumbers p !! nx !! ny
            let nRowGroupNumber = rowGroupNumbers p !! ny !! nx
            guard $ notElem neighbour ps
            guard $ (x == nx && nColGroupNumber == colGroupNumber) || (y == ny && nRowGroupNumber == rowGroupNumber)
            return neighbour
       in case newPositions of
            [] -> ps
            _  -> buildGroup $ ps ++ newPositions

rowGroupNumbers :: Puzzle -> [[Int]]
rowGroupNumbers p = map generateGroupNumbers (_puzzleRows p)

colGroupNumbers :: Puzzle -> [[Int]]
colGroupNumbers p = map generateGroupNumbers (_puzzleCols p)

generateGroupNumbers :: [Int] -> [Int]
generateGroupNumbers counts =
  concat $ do
    (i, count) <- zip ([1 ..] :: [Int]) counts
    return $ replicate count i

neighbours :: Puzzle -> Pos -> [Pos]
neighbours p (x, y) = filter inrange [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  where
    inrange (x, y) = x >= 0 && y >= 0 && x < s && y < s
    s = _size p
