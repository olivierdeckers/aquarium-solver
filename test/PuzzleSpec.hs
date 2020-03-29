module PuzzleSpec where

import Puzzle
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Control.Monad (join, replicateM)
import Data.List (nub, sort)
import           Test.Hspec.Hedgehog    (PropertyT, diff, forAll, hedgehog,
                                         (/==), (===))
import Test.Hspec (describe, it)

genGroups :: Gen [Int]
genGroups = go []
  where
    go :: [Int] -> Gen [Int]
    go xs
      | sum xs == 6 = Gen.constant xs
      | otherwise = do
        x <- Gen.int (Range.linear 1 6)
        if sum xs + x > 6
          then Gen.constant $ (6 - sum xs) : xs
          else go $ x : xs

genPuzzleWithGroups :: PropertyT IO Puzzle
genPuzzleWithGroups = do
  rowGroups <- replicateM 6 (forAll genGroups)
  colGroups <- replicateM 6 (forAll genGroups)

  return Puzzle {
      _size = 6
    , _colSums = []
    , _rowSums = []
    , _puzzleRows = rowGroups
    , _puzzleCols = colGroups
  }

spec = describe "calculateGroupPositions" $ do
  it "should generate groups that do not contain overlapping positions" $ hedgehog $ do
    puzzle <- genPuzzleWithGroups

    let groups = calculateGroupPositions puzzle

    (length . nub . join) groups === (length . join) groups
    
  it "should generate groups that cover the entire board" $ hedgehog $ do
    puzzle <- genPuzzleWithGroups
    
    let groups = calculateGroupPositions puzzle
    
    (sort . join) groups === [(x,y) | x <- [0..5], y <- [0..5]]