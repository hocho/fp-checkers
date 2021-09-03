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
    -- get all move single
    -- for each compute post move captures
    []

analyticsGenerateMoveSingle :: Board -> Player -> [(Move, Analytics)]
analyticsGenerateMoveSingle board player =
    let 
        preMoveCaptures = length $ movesJumpGet board (otherPlayer player)
        moves = movesSingleGet board player
    in
        map 
            (\move -> 
                (Move move, 
                Analytics {
                    preMoveCaptures = preMoveCaptures, 
                    postMoveCaptures = postMoveSingleCaptures board player move, 
                    captures = 0 }) )
            moves

postMoveSingleCaptures :: Board -> Player -> MoveSingle -> Int
postMoveSingleCaptures board player move =
    let
        newBoard = movePlay board move 
    in
        length $ movesJumpGet newBoard (otherPlayer player)