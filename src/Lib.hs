
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module Lib where

import           Control.Monad   (guard)
import           Data.Hashable   (Hashable)
import           Data.Holmes
import           Data.List       (foldl1', nub, sort, transpose)
import           Data.Propagator
import qualified Data.JoinSemilattice.Intersect as I
import           Debug.Trace
import           GHC.Generics    (Generic)
import Data.Kind (Type)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data AquariumCell = Air | Water
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

data WaterCount = V0 | V1 | V2 | V3 | V4 | V5 | V6
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

instance Num WaterCount where
  fromInteger a = traceCtx ("fromInteger " <> show a) $ toEnum . fromInteger $ a
  a + b       = toEnum $ fromEnum a + fromEnum b
  a - b       = traceCtx (show a ++ " - " ++ show b) $ toEnum $ fromEnum a - fromEnum b
  negate a    = error "no negate defined"
  a * b       = error "no times defined"
  abs a       = a
  signum a    = error "no signum defined"

data Puzzle = Puzzle {
    size       :: Int,
    colSums    :: [Int],
    rowSums    :: [Int],
    puzzleRows :: [[Int]],
    puzzleCols :: [[Int]]
  }

example = Puzzle {
  size = 6,
  colSums = [1, 5, 4, 5, 4, 4],
  rowSums = [2, 3, 4, 4, 5, 5],
  puzzleRows = [
      [2, 2, 1, 1],
      [1, 3, 1, 1],
      [1, 1, 1, 1, 1, 1],
      [2, 1, 1, 1, 1],
      [1, 2, 1, 1, 1],
      [1, 1, 4]
    ],
  puzzleCols = [
      [1, 5],
      [1, 2, 1, 2],
      [2, 3, 1],
      [3, 3],
      [5, 1],
      [6]
    ]
}

type Pos = (Int, Int)

config :: Puzzle -> Config Holmes (Intersect AquariumCell)
config p = gridSize `from` [Air, Water]
  where
    gridSize = size p * size p

constraints :: forall m. MonadCell m => Puzzle -> [Prop m (Intersect AquariumCell)] -> Prop m (Intersect Bool)
constraints p board =
  let rs :: [[Prop m (Intersect AquariumCell)]]
      rs = rows p board
      cs = cols p board
      gs = groups p board
      sumsandrows :: [(Prop m (Intersect WaterCount), [Prop m (Intersect AquariumCell)])]
      sumsandrows = zip (map (lift . I.singleton) $ ((map fromIntegral $ rowSums p) :: [WaterCount])) rs
   in and' [
--      and' $ map (\r -> foldl1' (.+) (map (over countWater) r) .>= 0) rs,
      and' $ [(over countWater c) .>= 0 | r <- rs, c <- r],
      and' $ map (\(sum, r) -> foldl1' (.+) (map (over countWater) r) .== sum) sumsandrows

    ]

aquarium :: Puzzle -> IO (Maybe [Intersect AquariumCell])
aquarium p = config p `satisfying` constraints p

countWater :: Intersect AquariumCell -> Intersect WaterCount
countWater = I.map f
  where
    f Water = 1
    f Air = 0

rows :: Puzzle -> [x] -> [[x]]
rows _ [] = []
rows p xs = take s xs : rows p (drop s xs)
  where s = size p

cols :: Puzzle -> [x] -> [[x]]
cols p xs = transpose $ rows p xs

traceCtx s a = trace (s ++ " " ++ show a) a

groups :: Puzzle -> [x] -> [[x]]
groups p xs =
  let s = size p
      groupPositions = calculateGroupPositions p
  in do
    ps <- groupPositions
    return $ do
      (x,y) <- ps
      return xs !! (y * s + x)

calculatePositions :: Puzzle -> [Pos]
calculatePositions Puzzle {size=s} = do
  x <- init [0..s]
  y <- init [0..s]
  return (x,y)

calculateGroupPositions :: Puzzle -> [[Pos]]
calculateGroupPositions p = nub $ map (\x -> nub . sort $ buildGroup [x]) (calculatePositions p)
  where
        buildGroup :: [Pos] -> [Pos]
        buildGroup ps =
          let newPositions = do
                                (x, y) <- traceCtx "Positions so far" ps
                                let !_ = traceCtx "expanding" (x,y)
                                let colGroupNumber = traceCtx "colGroupNumber" $ colGroupNumbers p !! x !! y
                                let rowGroupNumber = traceCtx "rowGroupNumber" $ rowGroupNumbers p !! y !! x
                                neighbour@(nx, ny) <- traceCtx "neighbours" $ neighbours p (x,y)
                                let !_ = traceCtx "checking neighbour" neighbour
                                let nColGroupNumber = traceCtx "neighbour colGroupNumber" $ colGroupNumbers p !! nx !! ny
                                let nRowGroupNumber = traceCtx "neighbour rowGroupNumber" $ rowGroupNumbers p !! ny !! nx
                                guard $ notElem neighbour ps
                                guard $ (x == nx && nColGroupNumber == colGroupNumber) || (y == ny && nRowGroupNumber == rowGroupNumber)
                                return $ traceCtx "selected neighbour" neighbour
          in case traceCtx "newPositions" newPositions of
            [] -> ps
            _  -> buildGroup $ ps ++ newPositions

rowGroupNumbers :: Puzzle -> [[Int]]
rowGroupNumbers p = map generateGroupNumbers (puzzleRows p)

colGroupNumbers :: Puzzle -> [[Int]]
colGroupNumbers p = map generateGroupNumbers (puzzleCols p)

generateGroupNumbers :: [Int] -> [Int]
generateGroupNumbers counts = concat $ do
                               (i, count) <- zip ([1..] :: [Int]) counts
                               return $ replicate count i


neighbours :: Puzzle -> Pos -> [Pos]
neighbours p (x,y) = filter inrange [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
  where inrange (x,y) = x >= 0 && y >= 0 && x < s && y < s
        s = size p

