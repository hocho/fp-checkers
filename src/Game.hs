module Game where

import Board
    (
        Board
    ,   Player
    ,   Color
    ,   color
    ,   otherPlayer
    ,   boardInitial
    ,   player1
    ,   player2
    )

import Move
    (   Move(..)
    ,   Move_(..)
    )

import Display
    (   boardDisplay
    )

import MoveSingle
import MoveJump

import Data.List

gamePlay :: IO ()
gamePlay = do
    let
        board = boardInitial()
    do
        boardDisplay board
        gamePlay'
            board
            player1

movesGet :: Board -> Player -> [Move]
movesGet board player =
    map Move (movesJumpGet board player)
    ++
    map Move (movesSingleGet board player)

gamePlay' :: Board -> Player -> IO()
gamePlay' board player = do
    let
        moves = movesGet board player
        move = uncons moves
    do
        case move of
            Nothing ->
                return ()
            Just (Move nextMove, _) -> do
                let
                    newBoard = movePlay board nextMove
                    nextPlayer =
                        if moveName nextMove == "MoveSingle"
                            then otherPlayer player
                            else player
                do
                    putStr $ show (color player) ++ " "
                    putStrLn $ moveShow nextMove
                    putStrLn ""
                    boardDisplay newBoard
                    gamePlay' newBoard nextPlayer
