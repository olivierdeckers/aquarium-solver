import Test.Hspec
import Lib
import Data.Holmes

main :: IO ()
main = hspec $
  describe "solve" $ do
    it "should calculate the correct solution for example" $ do
      Just sol <- solve example1
      let solution = [
              w, w, a, a, a, a,
              a, w, w, w, a, a,
              a, w, a, w, w, w,
              a, a, w, w, w, w,
              a, w, w, w, w, w,
              a, w, w, w, w, w
            ]
      sol `shouldBe` solution

    it "should calculate the correct solution for example2" $ do
      Just sol <- solve example2
      let solution = [
              a, a, w, w, a, a,
              w, w, w, w, w, a,
              a, w, w, w, w, w,
              w, w, w, w, a, a,
              w, a, a, a, a, w,
              a, a, w, w, w, w
            ]
      sol `shouldBe` solution

    it "should calculate the correct solution for a 15x15 puzzle" $ do -- This one is very slow
      putStrLn $ show $ map (foldr1 (+)) $ _puzzleCols example3
      putStrLn $ show $ map (foldr1 (+)) $ _puzzleRows example3
      Just sol <- solve example3
      let solution = []
      sol `shouldBe` solution
  where
    w = Exactly Water
    a = Exactly Air


example1 = Puzzle {
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

example2 = Puzzle {
  _size = 6,
  _colSums = [3, 3, 5, 5, 3, 3],
  _rowSums = [2, 5, 5, 4, 2, 4],
  _puzzleRows = [
      [1, 1, 2, 2],
      [1, 1, 1, 2, 1],
      [1, 2, 2, 1],
      [3, 1, 2],
      [1, 2, 1, 1, 1],
      [2, 1, 1, 2]
    ],
  _puzzleCols = [
      [2, 3, 1],
      [2, 1, 1, 1, 1],
      [1, 1, 1, 1, 2],
      [1, 1, 1, 1, 2],
      [1, 1, 1, 1, 1, 1],
      [1, 2, 1, 1, 1]
    ]
}

example3 = Puzzle {
  _size = 15,
  _colSums = [8, 8, 9, 6, 7, 10, 10, 9, 9, 6, 7, 10, 10, 6, 3],
  _rowSums = [7, 9, 5, 10, 4, 5, 9, 12, 13, 1, 9, 4, 4, 12, 14],
  _puzzleRows = [
      [3, 2, 2, 1, 2, 1, 3, 1],
      [1, 2, 1, 1, 2, 1, 1, 2, 2, 2],
      [2, 1, 3, 1, 3, 2, 3],
      [1, 2, 1, 2, 2, 2, 1, 1, 3],
      [2, 1, 2, 1, 4, 2, 1, 1, 1],
      [2, 1, 1, 1, 1, 2, 4, 1, 2],
      [1, 1, 2, 3, 3, 2, 1, 2],
      [1, 1, 1, 1, 2, 1, 2, 1, 1, 2, 1, 1],
      [2, 2, 1, 2, 2, 1, 3, 1, 1],
      [2, 1, 5, 4, 3],
      [2, 1, 1, 1, 2, 6, 1, 1],
      [2, 1, 2, 3, 2, 4, 1],
      [1, 3, 2, 1, 2, 1, 1, 1, 3],
      [1, 2, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1],
      [1, 3, 5, 3, 1, 1, 1]
    ],
  _puzzleCols = [
      [1, 5, 2, 1, 6],
      [2, 1, 1, 2, 3, 3, 2, 1],
      [4, 2, 2, 2, 2, 2, 1],
      [1, 4, 2, 2, 2, 2, 2],
      [2, 1, 1, 2, 1, 3, 2, 2, 1],
      [1, 1, 1, 4, 1, 1, 1, 1, 1, 1, 2],
      [1, 2, 3, 2, 1, 1, 1, 1, 2, 1],
      [2, 1, 4, 2, 1, 1, 1, 1, 2],
      [2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1],
      [1, 2, 1, 1, 1, 3, 1, 1, 3, 1],
      [2, 2, 4, 1, 1, 1, 1, 3],
      [2, 1, 2, 2, 2, 1, 1, 3, 1],
      [2, 2, 3, 2, 1, 1, 1, 1, 2],
      [1, 3, 3, 2, 1, 2, 3],
      [5, 2, 6, 2]
    ]
}