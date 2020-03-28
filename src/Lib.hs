{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           Control.Monad                  (guard)
import           Data.Hashable                  (Hashable)
import           Data.Holmes
import qualified Control.Monad.Cell.Class as Cell
import qualified Data.JoinSemilattice.Defined   as D
import qualified Data.JoinSemilattice.Intersect as I
import           Data.Kind                      (Type)
import           Data.List                      (foldl1', nub, sort, transpose)
import           Data.List.Split                (chunksOf)
import           Data.Propagator
import           Debug.Trace
import           GHC.Generics                   (Generic)
import          Combinatorics

data AquariumCell = Air | Water
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

data Puzzle = Puzzle {
    _size       :: Int,
    _colSums    :: [Int],
    _rowSums    :: [Int],
    _puzzleRows :: [[Int]],
    _puzzleCols :: [[Int]]
  }

solve :: Puzzle -> IO (Maybe [Intersect AquariumCell])
solve p = config p `satisfying` constraints p

config :: Puzzle -> Config Holmes (Intersect AquariumCell)
config p = gridSize `from` [Air, Water]
  where gridSize = _size p * _size p

constraints :: forall m. MonadCell m => Puzzle -> [Prop m (Intersect AquariumCell)] -> Prop m (Intersect Bool)
constraints p board =
  let rows, cols :: [[Prop m (Intersect AquariumCell)]]
      rows = calculateRows p board
      cols = calculateCols p board

      groups :: [[(Prop m (Intersect AquariumCell), Int)]]
      groups = groupsWithDepth p board

      w :: Prop m (Intersect AquariumCell)
      w = lift $ I.singleton Water

   in and' $ concat [
      zipWith (\row sum -> exactly sum (w .==) row) rows (_rowSums p),
      zipWith (\col sum -> exactly sum (w .==) col) cols (_colSums p),
      map noAirBubbles groups
    ]

exactly :: forall x m. MonadCell m => Int -> (Prop m x -> Prop m (Intersect Bool)) -> [Prop m x] -> Prop m (Intersect Bool)
exactly count pred xs =
  let l = length xs
      choices :: [[Bool]]
      choices = choose l count
      applyChoice :: [Bool] -> [Prop m (Intersect Bool)]
      applyChoice picks = zipWith (\pick prop -> if pick then pred prop .== true else pred prop .== false) picks xs
  in or' (map (and'.applyChoice) choices)

noAirBubbles
  :: forall m . MonadCell m
  => [(Prop m (Intersect AquariumCell), Int)] 
  -> Prop m (Intersect Bool)
noAirBubbles gs =
  flip allWithIndex' gs $ \i (cell, depth) ->
    flip allWithIndex' gs $ \i2 (cell2, depth2) ->
      if i2 /= i && depth >= depth2
        then or' [
            cell2 .== lift (I.singleton Air),
            cell .== lift (I.singleton Water)
          ]
        else
          (true :: Prop m (Intersect Bool))

calculateRows :: Puzzle -> [x] -> [[x]]
calculateRows _ [] = []
calculateRows p xs = take s xs : calculateRows p (drop s xs)
  where s = _size p

calculateCols :: Puzzle -> [x] -> [[x]]
calculateCols p xs = transpose $ calculateRows p xs

groupsWithDepth :: Puzzle -> [x] -> [[(x, Int)]]
groupsWithDepth p xs =
  let s = _size p
      groupPositions = calculateGroupPositions p
  in do
    ps <- groupPositions
    return $ do
      (x,y) <- ps
      return (xs !! (y * s + x), y)

type Pos = (Int, Int)

calculatePositions :: Puzzle -> [Pos]
calculatePositions Puzzle {_size=s} = do
  x <- init [0..s]
  y <- init [0..s]
  return (x,y)

calculateGroupPositions :: Puzzle -> [[Pos]]
calculateGroupPositions p = nub $ map (\x -> nub . sort $ buildGroup [x]) (calculatePositions p)
  where
        buildGroup :: [Pos] -> [Pos]
        buildGroup ps =
          let newPositions = do
                                (x, y) <- ps
                                let colGroupNumber = colGroupNumbers p !! x !! y
                                let rowGroupNumber = rowGroupNumbers p !! y !! x
                                neighbour@(nx, ny) <- neighbours p (x,y)
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
generateGroupNumbers counts = concat $ do
                               (i, count) <- zip ([1..] :: [Int]) counts
                               return $ replicate count i

neighbours :: Puzzle -> Pos -> [Pos]
neighbours p (x,y) = filter inrange [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
  where inrange (x,y) = x >= 0 && y >= 0 && x < s && y < s
        s = _size p
