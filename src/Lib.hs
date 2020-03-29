{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           Combinatorics
import           Data.Hashable                  (Hashable)
import           Data.Holmes
import qualified Data.JoinSemilattice.Intersect as I
import           Data.Propagator
import           GHC.Generics                   (Generic)
import           Puzzle

data AquariumCell = Air | Water
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

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
      applyChoice picks = zipWith (\pick prop -> if pick then pred prop .== true else pred prop .== false) picks xs --TODO might be faster if we use the complement if count > l/2
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


