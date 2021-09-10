module MoveJumpSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import MoveJump

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Calculate jumped position" $ do

    it "returns the correct position jumped" $ do
      
      calcJumpedPosition (0,  1) (3, 4)
        `shouldBe` (2, 3)

    it "returns the correct position jumped" $ do
      
      calcJumpedPosition (4, 5) (1, 2)
        `shouldBe` (2, 3)

    it "returns the correct position jumped" $ do
      
      calcJumpedPosition (2, 5) (6, 1)
        `shouldBe` (5, 2)

    it "returns the correct position jumped" $ do
      
      calcJumpedPosition (2, 1) (6, 5)
        `shouldBe` (5, 4)
