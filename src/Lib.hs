{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           Control.Monad                  (guard)
import           Data.Hashable                  (Hashable)
import           Data.Holmes
import qualified Data.JoinSemilattice.Defined   as D
import qualified Data.JoinSemilattice.Intersect as I
import           Data.Kind                      (Type)
import           Data.List                      (foldl1', nub, sort, transpose)
import           Data.List.Split                (chunksOf)
import           Data.Propagator
import           Debug.Trace
import           GHC.Generics                   (Generic)

data AquariumCell = Air | Water
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

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

config :: Puzzle -> Config Holmes (Defined AquariumCell)
config p = gridSize `from` [Air, Water]
  where
    gridSize = size p * size p

constraints :: forall m. MonadCell m => Puzzle -> [Prop m (Defined AquariumCell)] -> Prop m (Defined Bool)
constraints p board =
  let rs :: [[Prop m (Defined AquariumCell)]]
      rs = rows p board
      cs = cols p board
      gs :: [[(Prop m (Defined AquariumCell), Int)]]
      gs = groupsWithDepth p board
      sumsandrows :: [(Prop m (Defined Int), [Prop m (Defined AquariumCell)])]
      sumsandrows = zip (map (lift . fromIntegral) (rowSums p)) rs
      sumsandcols = zip (map (lift . fromIntegral) (colSums p)) cs
   in and' [
      and' $ map (\(sum, r) -> foldl1' (.+) (map countWater r) .== sum) sumsandrows,
      and' $ map (\(sum, c) -> foldl1' (.+) (map countWater c) .== sum) sumsandcols,
      and' $ map noAirBubbles gs
    ]

noAirBubbles :: forall m . MonadCell m => [(Prop m (Defined AquariumCell), Int)] -> Prop m (Defined Bool)
noAirBubbles gs =
  let gs' :: [(Prop m (Defined AquariumCell), Prop m (Defined Int))]
      gs' = map (\(ac, d) -> (ac, (lift . fromIntegral) d)) gs
      zipped :: [Prop m (Defined (AquariumCell, Int))]
      zipped = map (uncurry (zipWith' (,))) gs'
      compatible :: (AquariumCell, Int) -> (AquariumCell, Int) -> Bool
      compatible (Air, d1) (Water, d2) | d1 >= d2 = False
      compatible (Water, d1) (Air, d2) | d1 <= d2 = False
      compatible _ _                   = True
   in flip allWithIndex' zipped $ \i g ->
        flip allWithIndex' zipped $ \i2 g2 ->
          or' [
            ((lift $ fromIntegral i2) :: Prop m (Defined Int)) .<= lift (fromIntegral i)
          , zipWith' compatible g g2
          ]

aquarium :: Puzzle -> IO (Maybe [Defined AquariumCell])
aquarium p = config p `satisfying` constraints p

-- For some reason, if I use this implementation, I get no solutions, while .$ works fine
--countWater ac = (over (fmap f)) ac
countWater :: forall m. Prop m (Defined AquariumCell) -> Prop m (Defined Int)
countWater ac = f .$ ac
  where
    f Water = 1
    f Air   = 0

rows :: Puzzle -> [x] -> [[x]]
rows _ [] = []
rows p xs = take s xs : rows p (drop s xs)
  where s = size p

cols :: Puzzle -> [x] -> [[x]]
cols p xs = transpose $ rows p xs

groupsWithDepth :: Puzzle -> [x] -> [[(x, Int)]]
groupsWithDepth p xs =
  let s = size p
      groupPositions = calculateGroupPositions p
  in do
    ps <- groupPositions
    return $ do
      (x,y) <- ps
      return (xs !! (y * s + x), y)

type Pos = (Int, Int)

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

solution :: [Defined AquariumCell]
solution = [
    w, w, a, a, a, a,
    a, w, w, w, a, a,
    a, w, a, w, w, w,
    a, a, w, w, w, w,
    a, w, w, w, w, w,
    a, w, w, w, w, w
  ]
    where a = Exactly Air
          w = Exactly Water

printBoard :: Puzzle -> [ Defined AquariumCell ] -> IO ()
printBoard p cells = mapM_ putStrLn $ chunksOf (size p) $ map showCell cells
  where showCell (Exactly Air)   = '_'
        showCell (Exactly Water) = 'x'
