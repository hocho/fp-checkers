module BoardSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Board

main :: IO ()
main = hspec spec

specIsValidPosition :: Position -> SpecWith ()
specIsValidPosition pos = 
  describe "Valid Position" $ do
    it "returns true if position is valid" $ do
      isValidPosition pos `shouldBe` True

specIsInvalidPosition :: Position -> SpecWith ()
specIsInvalidPosition pos = 
  describe "Invalid Position" $ do
    it "returns false if position is invalid" $ do
      isValidPosition pos `shouldBe` False

spec :: Spec
spec = do
  specIsValidPosition (0, 1)
  specIsValidPosition (1, 0)
  specIsValidPosition (7, 4)
  specIsValidPosition (2, 5)

  specIsInvalidPosition (0, 0)
  specIsInvalidPosition (8, 0)
  specIsInvalidPosition (0, 8)
  specIsInvalidPosition (2, -1)
