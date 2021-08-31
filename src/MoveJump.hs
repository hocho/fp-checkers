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
    ,   otherPlayer, Position
    )
    
import Data.Maybe

calcJumpedPosition :: Position -> Position -> Position 
calcJumpedPosition p1 p2 = 
    (avg (fst p1) (fst p2), avg (snd p1) (snd p2)) 
    where 
        avg x y = div (x + y) 2

isValidJump :: Board -> Player -> MoveJump -> Bool
isValidJump board movePlayer move =
        let
            fromPiece = boardPiece board (fromX move)
            isFromPieceOfPlayer =
                case fromPiece of
                Just p
                    ->  player p == movePlayer
                Nothing
                    ->  False
            jumpedOverPosition = calcJumpedPosition (fromX move) (toX move) 
            jumpOverPiece = boardPiece board jumpedOverPosition
            isJumpOverPieceOfOpponent =
                case jumpOverPiece of
                Just p
                    ->  player p == otherPlayer movePlayer
                Nothing
                    ->  False
        in
                isValidPosition (fromX move)
            &&  isValidPosition (toX move)
            &&  isFromPieceOfPlayer
            &&  isNothing(boardPiece board (toX move))
            &&  isJumpOverPieceOfOpponent

movesJumpGet :: Board -> Player -> [MoveJump]
movesJumpGet board player =
    filter (isValidJump board player) moves
    where
        delta = case side player of
            North -> 2
            South -> -2
        createJump (row, col) deltaCol =
            MoveJump { fromX = (row, col), toX = (row + delta, col + deltaCol) }
        createMoves position =
            [   createJump position (-2) 
            ,   createJump position 2 
            ]
        moves = concat $ [createMoves (row, col) | row <- [0 .. boardSize], col <- [0 .. boardSize]]

instance Move_ MoveJump where 
    movePlay :: Board -> MoveJump -> Board  
    movePlay board move =
        let
            movePiece = boardPiece board (fromX move)
            jumpedOverPosition = calcJumpedPosition (fromX move) (toX move) 
        in
            map
                (\(row, idx) ->
                    if 
                    |   idx == fst (fromX move) -> 
                            rowUpdate row (snd(fromX move)) Nothing
                    |   idx == fst (toX move) ->
                            rowUpdate row (snd(toX move)) movePiece
                    |   idx == fst jumpedOverPosition ->
                            rowUpdate row (snd jumpedOverPosition) Nothing
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

