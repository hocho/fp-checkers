module BoardSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Board
import Data.Array
import Data.Maybe

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
  -- valid positions
  specIsValidPosition (0, 1)
  specIsValidPosition (1, 0)
  specIsValidPosition (7, 4)
  specIsValidPosition (2, 5)

  -- invalid positions
  specIsInvalidPosition (0, 0)
  specIsInvalidPosition (8, 0)
  specIsInvalidPosition (0, 8)
  specIsInvalidPosition (2, -1)

  describe "Build board row from a string" $ do

    describe "Valid string" $ do
      it "returns valid row" $ do
        let
          row = buildRowFromStringDefault "| |1| |0|"

        row `shouldBe` [Nothing, pawn1, Nothing, pawn0]

    describe "Invalid string" $ do
      it "raises exception" $ do
        evaluate(buildRowFromStringDefault "| |1| |0|2|3|4|" !! 6)
          `shouldThrow` anyException

  describe "Player and PieceType" $ do

    let
        row = buildRowFromStringDefault "| |0|1|2|3|"
        board = [row]

    describe "Incorrect Player and PieceType" $ do

      it "no piece, returns false" $ do
          playerPieceType board player0 Pawn (0, 0) `shouldBe` False

      it "wrong player, returns false" $ do
          playerPieceType board player1 Pawn (0, 1) `shouldBe` False 

      it "wrong piece, returns false" $ do
          playerPieceType board player0 Queen (0, 1) `shouldBe` False 

    describe "Correct Player and PieceType" $ do

      it "Player 0 and Pawn" $ do
          playerPieceType board player0 Pawn (0, 1) `shouldBe` True 

      it "Player 1 and Pawn" $ do
          playerPieceType board player1 Pawn (0, 2) `shouldBe` True 

      it "Player 0 and Queen" $ do
          playerPieceType board player0 Queen (0, 3) `shouldBe` True 

      it "Player 1 and Queen" $ do
          playerPieceType board player1 Queen (0, 4) `shouldBe` True 


