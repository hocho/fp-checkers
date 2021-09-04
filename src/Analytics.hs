-- Captures the analysis of a move 

 module Analytics
    where

import Board 
    (   Board
    ,   Player
    ,   otherPlayer
    )

import Move
    (   Move_
    ,    Move(..)
    ,   movePlay
    )

import MoveSingle
import MoveJump

data Analytics = Analytics
    {   preMoveCaptures     ::  Int 
    ,   postMoveCaptures    ::  Int 
    ,   captures            ::  Int
    }
    deriving (Eq, Show)


analyticsGenerate :: Board -> Player -> [(Move, Analytics)]
analyticsGenerate board player =
    analyticsGenerateMoveJump board player ++ 
    analyticsGenerateMoveSingle board player

analyticsGenerateMoveSingle :: Board -> Player -> [(Move, Analytics)]
analyticsGenerateMoveSingle board player =
    let 
        preMoveCaptures = length $ movesJumpGet board (otherPlayer player)
        moves = map Move $ movesSingleGet board player
    in
        map 
            (\move -> 
                (move, 
                Analytics {
                    preMoveCaptures = preMoveCaptures, 
                    postMoveCaptures = computePostMoveCaptures board player move, 
                    captures = 0 }) )
            moves

-- postMoveSingleCaptures :: Board -> Player -> MoveSingle -> Int
-- postMoveSingleCaptures board player move =
--     let
--         newBoard = movePlay board move 
--     in
--         length $ movesJumpGet newBoard (otherPlayer player)

computePostMoveCaptures :: Board -> Player -> Move -> Int
computePostMoveCaptures board player (Move move) =
    let
        newBoard = movePlay board move
    in
        length $ movesJumpGet newBoard (otherPlayer player)

-- to fix
analyticsGenerateMoveJump :: Board -> Player -> [(Move, Analytics)]
analyticsGenerateMoveJump board player =
    let 
        preMoveCaptures = length $ movesJumpGet board (otherPlayer player)
        moves = map Move $ movesJumpGet board player
    in
        map 
            (\move -> 
                (move, 
                Analytics {
                    preMoveCaptures = preMoveCaptures, 
                    postMoveCaptures = computePostMoveCaptures board player move, 
                    captures = 0 }) )
            moves
