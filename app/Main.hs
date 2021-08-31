module Main where

import Game
    (
        Board
    ,   Player
    ,   Color
    ,   color
    ,   otherPlayer
    ,   boardInitial
    ,   boardDisplay
    ,   player1
    ,   player2
    )

import Move
    (   Move(..)
    ,   Move_(..)
    )

import MoveSingle
import MoveJump

import Data.List

main :: IO ()
main = do
    let
        board = boardInitial()
    do
        boardDisplay board
        playGame
            board
            player1

movesGet :: Board -> Player -> [Move]
movesGet board player = 
    (map Move $ movesJumpGet board player)
    ++ 
    (map Move $ movesSingleGet board player)

playGame :: Board -> Player -> IO()
playGame board player = do
    let
        moves = movesGet board player
        move = uncons moves
    do
        case move of
            Nothing ->
                return ()
            Just ((Move nextMove), _) -> do
                let
                    newBoard = movePlay board nextMove
                    nextPlayer = otherPlayer player
                do
                    putStr $ show (color player) ++ " "
                    putStrLn $ moveShow nextMove
                    putStrLn ""
                    boardDisplay newBoard
                    playGame newBoard nextPlayer
