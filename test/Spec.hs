import Test.Hspec
import Lib
import Data.Holmes

main :: IO ()
main = hspec $ do
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