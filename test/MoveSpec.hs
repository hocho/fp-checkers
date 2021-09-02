module MoveSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Move
import Board
    (   pawn1
    )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Moves" $ do
      
    it "updates a board row correctly" $ do

        boardRowUpdated `shouldBe` [Nothing, pawn1, Nothing]
        
        where 
            boardRow = [Nothing, Nothing, Nothing]
            boardRowUpdated = rowUpdate boardRow 1 pawn1
