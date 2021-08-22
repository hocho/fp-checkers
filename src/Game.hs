 module Game
    (   Board
    ,   Move
    ,   Player
    ,   boardInitial
    ,   boardDisplay
    ,   movesGet
    ,   movesDisplay
    ,   movePlay
    ,   player1
    ,   player2
    ) where

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

data Move = Move
    {   from :: Position
    ,   to :: Position
    }
    deriving (Show)

type BoardRow = [Maybe Piece]
type Board = [BoardRow]

player1 = Player { color = Black, side = North }
player2 = Player { color = White, side = South }

pawn1 = Just Piece { player = player1, pieceType = Pawn}
pawn2 = Just Piece { player = player2, pieceType = Pawn}

boardInitial :: () -> Board
boardInitial () =
    [
        buildRow [Nothing, pawn1]
    ,   buildRow [pawn1, Nothing]
    ,   buildRow [Nothing, pawn1]
    ,   buildRow [Nothing]
    ,   buildRow [Nothing]
    ,   buildRow [pawn2, Nothing]
    ,   buildRow [Nothing, pawn2]
    ,   buildRow [pawn2, Nothing]
    ]

    where
        buildRow :: [Maybe Piece] -> [Maybe Piece]
        buildRow segment = take boardSize $ concat $ repeat segment

boardPiece :: Board -> Position -> Maybe Piece
boardPiece board (row, col) = board !! row !! col

isValidMove :: Board -> Player -> Move -> Bool
isValidMove board movePlayer move =
        let
            fromPiece = boardPiece board (from move)
            isFromPieceOfPlayer =
                case fromPiece of
                Just p
                    ->  player p == movePlayer
                Nothing
                    ->  False
        in
                isValidPosition (from move)
            &&  isValidPosition (to move)
            &&  isFromPieceOfPlayer
            &&  isNothing(boardPiece board (to move))

movesGet :: Board -> Player -> [Move]
movesGet board player =
    filter (isValidMove board player) moves
    where
        delta = case side player of
            North -> 1
            South -> -1
        createMove (row, col) deltaCol =
            Move { from = (row, col), to = (row + delta, col + deltaCol) }
        createMoves position =
            [   createMove position (-1) 
            ,   createMove position 1 
            ]
        moves = concat $ [createMoves (row, col) | row <- [0 .. boardSize], col <- [0 .. boardSize]]

-- Updates a row 
rowUpdate :: BoardRow -> Int -> Maybe Piece -> BoardRow
rowUpdate boardRow col piece =
    let
        (l, r) = splitAt col boardRow
    in
        l ++ piece : tail r

-- Assumes that hte move is a valid move
movePlay :: Board -> Move -> Board
movePlay board move =
    let
        movePiece = boardPiece board (from move)
    in
        map
            (\(row, idx) ->
                if idx == fst (from move) then
                    rowUpdate row (snd(from move)) Nothing
                else if idx == fst (to move) then
                    rowUpdate row (snd(to move)) movePiece
                else
                    row
                ) 
            $ zip board [0 .. ] 

boardDisplay :: Board -> IO()
boardDisplay [] = do
    putStrLn ""
boardDisplay (x : xs) = do
    rowDisplay x
    boardDisplay xs

rowDisplay :: BoardRow -> IO()
rowDisplay [] = do
    putStrLn "|"
rowDisplay (x : xs) = do
    putStr $ "|" ++ maybe " " show x
    rowDisplay xs

movesDisplay :: [Move] -> IO()
movesDisplay [] = do
    putStrLn ""
movesDisplay (move : moves) = do
    print move
    movesDisplay moves


-- Jump
-- 	From Location
-- 	Over Location
-- 	To Location

-- MultiMove
-- 	Move[]

-- MultiJump
-- 	Jump[]	               