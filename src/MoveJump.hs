{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE InstanceSigs #-}

 module MoveJump
    where

import Move
    (   MoveJump(..)
    ,   Move_(..)
    )

import Game
    (   Board
    ,   Player(..)
    ,   Side(..)
    ,   Piece(..)
    ,   BoardRow
    ,   boardSize
    ,   isValidPosition
    ,   boardPiece
    )
    
import Data.Maybe

isValidMove :: Board -> Player -> MoveJump -> Bool
isValidMove board movePlayer move =
        let
            fromPiece = boardPiece board (fromX move)
            isFromPieceOfPlayer =
                case fromPiece of
                Just p
                    ->  player p == movePlayer
                Nothing
                    ->  False
        in
                isValidPosition (fromX move)
            &&  isValidPosition (toX move)
            &&  isFromPieceOfPlayer
            &&  isNothing(boardPiece board (toX move))

movesJumpGet :: Board -> Player -> [MoveJump]
movesJumpGet board player =
    filter (isValidMove board player) moves
    where
        delta = case side player of
            North -> 1
            South -> -1
        createMove (row, col) deltaCol =
            MoveJump { fromX = (row, col), toX = (row + delta, col + deltaCol) }
        createMoves position =
            [   createMove position (-1) 
            ,   createMove position 1 
            ]
        moves = concat $ [createMoves (row, col) | row <- [0 .. boardSize], col <- [0 .. boardSize]]


instance Move_ MoveJump where 
    movePlay :: Board -> MoveJump -> Board  
    movePlay board move =
        let
            movePiece = boardPiece board (fromX move)
        in
            map
                (\(row, idx) ->
                    if 
                    |   idx == fst (fromX move) -> 
                            rowUpdate row (snd(fromX move)) Nothing
                    |   idx == fst (toX move) ->
                            rowUpdate row (snd(toX move)) movePiece
                    |   otherwise ->
                            row
                )
                $ zip board [0 .. ] 

    moveShow :: MoveJump -> String 
    moveShow move = show (fromX move) ++ " -> " ++ show(toX move)

-- Updates a row 
rowUpdate :: BoardRow -> Int -> Maybe Piece -> BoardRow
rowUpdate boardRow col piece =
    let
        (l, r) = splitAt col boardRow
    in
        l ++ piece : tail r

