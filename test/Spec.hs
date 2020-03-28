import           Data.Holmes
import qualified Data.JoinSemilattice.Intersect as I
import           Lib
import           Test.Hspec

main :: IO ()
main =
  hspec $
  describe "solve" $ do
    it "should calculate the correct solution for example" $ do
      Just sol <- solve example1
      let solution =
            [w, w, a, a, a, a, a, w, w, w, a, a, a, w, a, w, w, w, a, a, w, w, w, w, a, w, w, w, w, w, a, w, w, w, w, w]
      sol `shouldBe` solution
    it "should calculate the correct solution for example2" $ do
      Just sol <- solve example2
      let solution =
            [a, a, w, w, a, a, w, w, w, w, w, a, a, w, w, w, w, w, w, w, w, w, a, a, w, a, a, a, a, w, a, a, w, w, w, w]
      sol `shouldBe` solution
    it "should calculate the correct solution for a 10x10 puzzle" $
         do
          Just sol <- solve example4
          let solution = 
                [a, w, w, w, w, w, a, w, w, a,
                 a, a, w, w, w, a, w, w, w, a,
                 a, a, w, w, w, w, w, w, w, w,
                 a, a, w, w, w, w, w, w, w, w,
                 a, a, w, w, w, a, a, a, a, a, 
                 a, a, w, w, w, w, a, a, a, a, 
                 w, a, a, w, w, w, w, w, w, a, 
                 w, a, w, w, a, a, a, w, w, a,
                 w, w, a, a, a, a, w, w, a, a,
                 w, w, w, w, w, w, w, w, w, a
                ]
          sol `shouldBe` solution
--    it "should calculate the correct solution for a 15x15 puzzle" $ -- This one is very slow, maybe an input error?
--     do
--      putStrLn $ show $ map (foldr1 (+)) $ _puzzleCols example3
--      putStrLn $ show $ map (foldr1 (+)) $ _puzzleRows example3
--      Just sol <- solve example3
--      let solution = []
--      sol `shouldBe` solution
  where
    w = I.singleton Water
    a = I.singleton Air

example1 =
  Puzzle
    { _size = 6
    , _colSums = [1, 5, 4, 5, 4, 4]
    , _rowSums = [2, 3, 4, 4, 5, 5]
    , _puzzleRows = [[2, 2, 1, 1], [1, 3, 1, 1], [1, 1, 1, 1, 1, 1], [2, 1, 1, 1, 1], [1, 2, 1, 1, 1], [1, 1, 4]]
    , _puzzleCols = [[1, 5], [1, 2, 1, 2], [2, 3, 1], [3, 3], [5, 1], [6]]
    }

example2 =
  Puzzle
    { _size = 6
    , _colSums = [3, 3, 5, 5, 3, 3]
    , _rowSums = [2, 5, 5, 4, 2, 4]
    , _puzzleRows = [[1, 1, 2, 2], [1, 1, 1, 2, 1], [1, 2, 2, 1], [3, 1, 2], [1, 2, 1, 1, 1], [2, 1, 1, 2]]
    , _puzzleCols = [[2, 3, 1], [2, 1, 1, 1, 1], [1, 1, 1, 1, 2], [1, 1, 1, 1, 2], [1, 1, 1, 1, 1, 1], [1, 2, 1, 1, 1]]
    }

example3 =
  Puzzle
    { _size = 15
    , _colSums = [8, 8, 9, 6, 7, 10, 10, 9, 9, 6, 7, 10, 10, 6, 3]
    , _rowSums = [7, 9, 5, 10, 4, 5, 9, 12, 13, 1, 9, 4, 4, 12, 14]
    , _puzzleRows =
        [ [3, 2, 2, 1, 2, 1, 3, 1]
        , [1, 2, 1, 1, 2, 1, 1, 2, 2, 2]
        , [2, 1, 3, 1, 3, 2, 3]
        , [1, 2, 1, 2, 2, 2, 1, 1, 3]
        , [2, 1, 2, 1, 4, 2, 1, 1, 1]
        , [2, 1, 1, 1, 1, 2, 4, 1, 2]
        , [1, 1, 2, 3, 3, 2, 1, 2]
        , [1, 1, 1, 1, 2, 1, 2, 1, 1, 2, 1, 1]
        , [2, 2, 1, 2, 2, 1, 3, 1, 1]
        , [2, 1, 5, 4, 3]
        , [2, 1, 1, 1, 2, 6, 1, 1]
        , [2, 1, 2, 3, 2, 4, 1]
        , [1, 3, 2, 1, 2, 1, 1, 1, 3]
        , [1, 2, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1]
        , [1, 3, 5, 3, 1, 1, 1]
        ]
    , _puzzleCols =
        [ [1, 5, 2, 1, 6]
        , [2, 1, 1, 2, 3, 3, 2, 1]
        , [4, 2, 2, 2, 2, 2, 1]
        , [1, 4, 2, 2, 2, 2, 2]
        , [2, 1, 1, 2, 1, 3, 2, 2, 1]
        , [1, 1, 1, 4, 1, 1, 1, 1, 1, 1, 2]
        , [1, 2, 3, 2, 1, 1, 1, 1, 2, 1]
        , [2, 1, 4, 2, 1, 1, 1, 1, 2]
        , [2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1]
        , [1, 2, 1, 1, 1, 3, 1, 1, 3, 1]
        , [2, 2, 4, 1, 1, 1, 1, 3]
        , [2, 1, 2, 2, 2, 1, 1, 3, 1]
        , [2, 2, 3, 2, 1, 1, 1, 1, 2]
        , [1, 3, 3, 2, 1, 2, 3]
        , [5, 2, 6, 2]
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