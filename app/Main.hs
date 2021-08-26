module Main where

import Game
    (
        Board
    ,   Move
    ,   Player
    ,   Color
    ,   color
    ,   from
    ,   to
    ,   otherPlayer
    ,   boardInitial
    ,   boardDisplay
    ,   movesGet
    ,   movesDisplay
    ,   movePlay
    ,   player1
    ,   player2
    )

main :: IO ()
main = do
    let
        board = boardInitial()
    do
        boardDisplay board
        playGame
            board
            player1

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
                    putStrLn $ formatMove nextMove
                    putStrLn ""
                    boardDisplay newBoard
                    playGame newBoard nextPlayer

formatMove ::  Move -> String
formatMove move = show (from move) ++ " -> " ++ show(to move)