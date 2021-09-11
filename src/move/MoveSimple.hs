{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE InstanceSigs #-}

 module MoveSimple
    where

import Move
    (   Move_(..)
    ,   rowUpdate
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

data MoveSimple = MoveSimple
    {   from :: Position
    ,   to :: Position
    }
    deriving (Show)

isValidMove :: Board -> Player -> MoveSimple -> Bool
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

movesSingleGet :: Board -> Player -> [MoveSimple]
movesSingleGet board player =
    filter (isValidMove board player) moves
    where
        delta = case side player of
            North -> 1
            South -> -1
        createMove (row, col) deltaCol =
            MoveSimple { from = (row, col), to = (row + delta, col + deltaCol) }
        createMoves position =
            [   createMove position (-1)
            ,   createMove position 1
            ]
        moves = concat $ [createMoves (row, col) | row <- [0 .. boardSize], col <- [0 .. boardSize]]

instance Move_ MoveSimple where
    movePlay :: Board -> MoveSimple -> Board
    movePlay board move =
        let 
            movePiece = boardPiece board (from move)
        in           
            zipWith 
                (\ idx row -> 
                    if 
                    | idx == fst (from move) -> 
                        rowUpdate row (snd (from move)) Nothing
                    | idx == fst (to move) -> 
                        rowUpdate row (snd (to move)) movePiece
                    | otherwise -> 
                        row)
            [0 ..] board

    moveShow :: MoveSimple -> String
    moveShow move = "Move " ++ show (from move) ++ " -> " ++ show(to move)

    moveName :: MoveSimple -> String
    moveName move = "MoveSingle"

