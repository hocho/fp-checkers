{-# LANGUAGE InstanceSigs #-}

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
    ,   MovePlayer(..)
    ,   movePlay
    ,   moveShow
    )

import MoveSingle
import MoveJump

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
    map MoveSingleCtor $ movesSingleGet board player

playGame :: Board -> Player -> IO()
playGame board player = do
    let
        moves = movesGet board player
    do
        case moves of 
            [] ->
                return ()
            _ -> do
                let 
                    nextMove = head moves
                    newBoard = movePlay board nextMove
                    nextPlayer = otherPlayer player
                do
                    putStr $ show (color player) ++ " "
                    putStrLn $ moveShow nextMove
                    putStrLn ""
                    boardDisplay newBoard
                    playGame newBoard nextPlayer

instance MovePlayer Move where 
    movePlay :: Board -> Move -> Board  
    movePlay board move =
        case move of 
            MoveSingleCtor single ->
                 movePlay board single
            MoveJumpCtor jump ->
                movePlay board jump
    moveShow :: Move -> String 
    moveShow move = 
        case move of 
            MoveSingleCtor single ->
                 moveShow single
            MoveJumpCtor jump ->
                moveShow jump
