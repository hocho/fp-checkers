module AnalyticsSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Analytics
import Move
import Board
    (   Piece
    ,   pawn1
    ,   pawn2
    ,   boardInitial
    ,   player1
    ,   player2
    )
import Data.List

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Analytics for Move Single" $ do

    describe "Initial Board" $ do

      it "It returns analytics with all 0's" $ do

          let
              board = boardInitial ()
              result = map snd $ analyticsGenerateMoveSingle board player1
              count = length result
          
          result `shouldBe` replicate count (Analytics 0 0 0)

    describe "White to play with 2 pre move captures" $ do

      it "All moves result in 1 per and 1 post move capture" $ do

        let
            board =    
              [   br "| | | | | | | | |"
              ,   br "| | | | | | | | |"
              ,   br "| |B| | | |B| | |"
              ,   br "| | |W| |W| | | |"
              ,   br "| | | | | | | | |"
              ,   br "| | | | | | | | |"
              ,   br "| | | | | | | | |"
              ,   br "| | | | | | | | |"
              ] 
            result = map snd $ analyticsGenerateMoveSingle board player2
            count = length result

        result `shouldBe` replicate count (Analytics 2 1 0)

-- Creates a board row from a string
br :: String -> [Maybe Piece]
br str =
  unfoldr 
    (\s@(c : cs) -> if null s then Nothing else Just (toPiece c, cs))
    str'
  where
    str' = filter (/='|') str 
    toPiece ch =
      case ch of
        'B' -> pawn1
        'W' -> pawn2
        _   -> Nothing