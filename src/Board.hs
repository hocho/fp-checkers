{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveAnyClass #-}

 module Board
    where

import Data.Maybe

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
    [   buildRow [Nothing, piece1]
    ,   buildRow [piece1, Nothing]
    ,   buildRow [Nothing, piece1]
    ,   buildRow [Nothing]
    ,   buildRow [Nothing]
    ,   buildRow [piece2, Nothing]
    ,   buildRow [Nothing, piece2]
    ,   buildRow [piece2, Nothing]
    ]
    where 
        piece1 = Just p1
        piece2 = Just p2

buildRow :: [Maybe Piece] -> [Maybe Piece]
buildRow segment = take boardSize $ concat $ repeat segment

boardPiece :: Board -> Position -> Maybe Piece
boardPiece board (row, col) = board !! row !! col


               