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

    it "returns the correct postion jumped" $ do
      
      calcJumpedPosition (3,  7) (5, 5)
        `shouldBe` (4, 6)

