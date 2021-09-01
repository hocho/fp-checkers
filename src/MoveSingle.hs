{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE InstanceSigs #-}

 module MoveSingle
    where

import Move
    (   Move_(..)
    )

import Board
    (   Board
    ,   Player(..)
    ,   Side(..)
    ,   Piece(..)
    ,   BoardRow
    ,   Position
    ,   boardSize
    ,   isValidPosition
    ,   boardPiece
    )
    
import Data.Maybe

data MoveSingle = MoveSingle
    {   from :: Position
    ,   to :: Position
    }
    deriving (Show)

isValidMove :: Board -> Player -> MoveSingle -> Bool
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

movesSingleGet :: Board -> Player -> [MoveSingle]
movesSingleGet board player =
    filter (isValidMove board player) moves
    where
        delta = case side player of
            North -> 1
            South -> -1
        createMove (row, col) deltaCol =
            MoveSingle { from = (row, col), to = (row + delta, col + deltaCol) }
        createMoves position =
            [   createMove position (-1) 
            ,   createMove position 1 
            ]
        moves = concat $ [createMoves (row, col) | row <- [0 .. boardSize], col <- [0 .. boardSize]]

instance Move_ MoveSingle where 
    movePlay :: Board -> MoveSingle -> Board  
    movePlay board move =
        let
            movePiece = boardPiece board (from move)
        in
            map
                (\(row, idx) ->
                    if 
                    |   idx == fst (from move) -> 
                            rowUpdate row (snd(from move)) Nothing
                    |   idx == fst (to move) ->
                            rowUpdate row (snd(to move)) movePiece
                    |   otherwise ->
                            row)
                $ zip board [0 .. ] 

    moveShow :: MoveSingle -> String 
    moveShow move = "Move " ++ show (from move) ++ " -> " ++ show(to move)

    moveName :: MoveSingle -> String
    moveName move = "MoveSingle"

-- Updates a row 
rowUpdate :: BoardRow -> Int -> Maybe Piece -> BoardRow
rowUpdate boardRow col piece =
    let
        (l, r) = splitAt col boardRow
    in
        l ++ piece : tail r

