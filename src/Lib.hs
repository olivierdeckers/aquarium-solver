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

solve :: Puzzle -> IO (Maybe [Defined AquariumCell])
solve p = config p `satisfying` constraints p

config :: Puzzle -> Config Holmes (Defined AquariumCell)
config p = gridSize `from` [Air, Water]
  where gridSize = _size p * _size p

constraints :: forall m. MonadCell m => Puzzle -> [Prop m (Defined AquariumCell)] -> Prop m (Defined Bool)
constraints p board =
  let rows, cols :: [[Prop m (Defined AquariumCell)]]
      rows = calculateRows p board
      cols = calculateCols p board

      groups :: [[(Prop m (Defined AquariumCell), Int)]]
      groups = groupsWithDepth p board

      rowSums, colSums :: [Prop m (Defined Int)]
      rowSums = map liftInt $ _rowSums p
      colSums = map liftInt $ _colSums p

   in and' $ concat [
      zipWith amountOfWaterEqualsSum rows rowSums,
      zipWith amountOfWaterEqualsSum cols colSums,
      map noAirBubbles groups
    ]

amountOfWaterEqualsSum
  :: forall m . MonadCell m
  => [Prop m (Defined AquariumCell)]
  -> Prop m (Defined Int)
  -> Prop m (Defined Bool)
amountOfWaterEqualsSum cells sum = foldl1' (.+) (map countWater cells) .== sum

noAirBubbles
  :: forall m . MonadCell m
  => [(Prop m (Defined AquariumCell), Int)] 
  -> Prop m (Defined Bool)
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

countWater :: forall m. Prop m (Defined AquariumCell) -> Prop m (Defined Int)
-- For some reason, if I use this implementation, I get no solutions, while .$ works fine
--countWater ac = (over (fmap f)) ac
countWater ac = f .$ ac --this works, but it goes only in one direction
--countWater = Data.Propagator.unary (mapR (Just f, Just g)) -- This results in errors because it tries to get the aquariumcell for eg. 7
  where
    f Water = 1
    f Air = 0
    g 1 = Water
    g 0 = Air
    g a = error $ "There is no aquarium call defined for " <> show a

liftInt :: forall m . MonadCell m => Int -> Prop m (Defined Int)
liftInt = lift . fromIntegral

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
