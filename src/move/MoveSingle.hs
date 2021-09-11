{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE InstanceSigs #-}

 module MoveSingle
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
    ,   getPositions
    ,   boardPiece, PieceType (Pawn), isPlayerPieceType
    )

import Data.Maybe

data MoveSingle = MoveSingle
    {   from :: Position
    ,   to :: Position
    }
    deriving (Show)

isValidMove :: Board -> Player -> MoveSingle -> Bool
isValidMove board movePlayer move =
            isValidPosition (from move)
        &&  isValidPosition (to move)
        &&  isNothing(boardPiece board (to move))

movesSingleGet :: Board -> Player -> [MoveSingle]
movesSingleGet board player =
    filter isValidPlayerMove moves
    where
        isValidPlayerMove = isValidMove board player
        delta = case side player of
            North -> 1
            South -> -1
        createMove (row, col) deltaCol =
            MoveSingle (row, col) (row + delta, col + deltaCol) 
        createMoves position =
            [   createMove position (-1)
            ,   createMove position 1
            ]
        moves =
            concatMap createMoves (getPositions board player Pawn)

instance Move_ MoveSingle where
    movePlay :: Board -> MoveSingle -> Board
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

    moveShow :: MoveSingle -> String
    moveShow move = "Move " ++ show (from move) ++ " -> " ++ show(to move)

    moveName :: MoveSingle -> String
    moveName move = "MoveSingle"

