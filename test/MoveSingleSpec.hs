module MoveSingleSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)


import MoveSingle
import Board

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Get Single Moves" $ do

    describe "-> Edge Move" $ do

      let board = buildBoardWithRows 
            [   emptyRow 
            ,   buildRowFromStringDefault "|0| | | | | | | |"
            ]

      it " -> returns the correct edge move" $ do
      
        movesSingleGet board player0 `shouldBe` [MoveSingle (1, 0) (2, 1)]

    describe "-> Inner Move" $ do

      let board = buildBoardWithRows 
            [   emptyRow 
            ,   buildRowFromStringDefault "| | |0| | | | | |"
            ]

      it " -> returns the correct inner moves" $ do
      
        movesSingleGet board player0 `shouldBe` 
          [ MoveSingle (1, 2) (2, 1)
          , MoveSingle (1, 2) (2, 3)
          ]

    describe "-> Blocked Move" $ do

      let board = buildBoardWithRows 
            [   emptyRow 
            ,   buildRowFromStringDefault "| | |0| | | | | |"
            ,   buildRowFromStringDefault "| | | |1| | | | |"
            ]

      it " -> returns the correct inner move" $ do
      
        movesSingleGet board player0 `shouldBe` 
          [ MoveSingle (1, 2) (2, 1)
          ]

