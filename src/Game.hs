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

import Strategy
import StrategyFirst

import Analytics

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

gamePlay' :: Board -> Player -> IO()
gamePlay' board player = do
    let
        strategy = StrategyFirst 0 0
        moveAnalytics = analyticsGenerate board player
        move = getMove strategy moveAnalytics
    do
        case move of
            Nothing ->
                return ()
            Just (Move nextMove) -> do
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
