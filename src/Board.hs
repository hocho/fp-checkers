{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveAnyClass #-}

 module Board
    where

import Data.Array.IArray
import Data.Maybe
import Data.List

boardSize = 8

data Side
    =   North
    |   South
    deriving (Show, Eq)

data Color
    =   Black
    |   White
    deriving (Show, Eq)

data Player = Player
    {   color :: Color
    ,   side :: Side
    }
    deriving (Show, Eq)

data PieceType
    =   Pawn
    |   Queen
    deriving (Show, Eq)

data Piece = Piece
    {   player :: Player
    ,   pieceType :: PieceType
    }
    deriving (Eq)

instance Show Piece where
    show piece = case color (player piece) of
        Black -> "B"
        White -> "W"

type Position = (Int, Int)

isValidPosition :: Position -> Bool
isValidPosition (row, col) =
        isValid row
    &&  
        isValid col
    &&  
        odd (row  + col)
    where
        isValid x =
                x >= 0
            &&  
                x < boardSize
    
type BoardRow = [Maybe Piece]
type Board = [BoardRow]

player1 = Player { color = Black, side = North }
player2 = Player { color = White, side = South }

otherPlayer player = if player == player1 then player2 else player1

pawn1 = Just Piece { player = player1, pieceType = Pawn}
pawn2 = Just Piece { player = player2, pieceType = Pawn}

-- Initialize board with Black - North and White - South
boardInitial :: () -> Board
boardInitial () =
    boardInitialize (fromJust pawn1) (fromJust pawn2)

-- Initialize board with 2 pieces
boardInitialize :: Piece -> Piece -> Board
boardInitialize p1 p2 =
    let
        br = buildRowFromStringDefault
    in
    [   br "| |0| |0| |0| |0|"
    ,   br "|0| |0| |0| |0| |"
    ,   br "| |0| |0| |0| |0|"
    ,   br "| | | | | | | | |"
    ,   br "| | | | | | | | |"
    ,   br "|1| |1| |1| |1| |"
    ,   br "| |1| |1| |1| |1|"
    ,   br "|1| |1| |1| |1| |"
    ]

boardPiece :: Board -> Position -> Maybe Piece
boardPiece board (row, col) = board !! row !! col

-- Row string is of the form "| |1| |0| | | |"
-- where 0 and 1 are indexes into the Piece Array passed
buildRowFromString :: String -> Array Int Piece -> [Maybe Piece]
buildRowFromString str pieces =
  unfoldr 
    (\s -> if null s then Nothing else Just (toPiece (head s), tail s))
    str'
  where
    str' = filter (/='|') str 
    toPiece ch =
      case ch of
        '0' -> Just $ pieces!0
        '1' -> Just $ pieces!1
        ' ' -> Nothing
        _   -> error "Unexpected Character"               

buildRowFromStringDefault :: String -> [Maybe Piece]
buildRowFromStringDefault str =
    buildRowFromString str pieces
    where 
        pieces = array(0,1) [(0, fromJust pawn1), (1, fromJust pawn2)]
  