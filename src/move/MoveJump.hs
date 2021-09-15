{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE InstanceSigs #-}

 module MoveJump
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
    ,   PieceType (Pawn)
    ,   boardSize
    ,   isValidPosition
    ,   boardPiece
    ,   otherPlayer
    ,   getPositions
    )
    
import Data.Maybe
import Data.Bifunctor 

data MoveJump = MoveJump
    {   fromX :: Position
    ,   toX:: Position
    }
    deriving (Show)

calcJumpedPosition :: Position -> Position -> Position 
calcJumpedPosition p1 p2 = 
    let 
        delta f = if f p1 < f p2 then -1 else 1
    in
        bimap (+ delta fst) (+ delta snd) p2


isValidJump :: Board -> Player -> MoveJump -> Bool
isValidJump board movePlayer move =
        let
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
        moves = concatMap createMoves (getPositions board player Pawn)

instance Move_ MoveJump where 
    movePlay :: Board -> MoveJump -> Board  
    movePlay board move =
        let
            movePiece = boardPiece board (fromX move)
            jumpedOverPosition = calcJumpedPosition (fromX move) (toX move) 
        in
            zipWith
                (\ idx row ->
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
                [0 ..] board

    moveShow :: MoveJump -> String 
    moveShow move = "Jump " ++ show (fromX move) ++ " -> " ++ show(toX move)

    moveName :: MoveJump -> String
    moveName move = "MoveJump"


