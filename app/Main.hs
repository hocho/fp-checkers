module Main where

import Game
    (
        Board
    ,   Move
    ,   Player
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
            (movesGet board player1)

playGame :: Board -> Player -> [Move] -> IO()
playGame board player [] = do
    return ()
playGame board player moves = do
    let 
        nextMove = head moves
        newBoard = movePlay board nextMove
        nextPlayer = if player == player1 then player2 else player1
        nextMoves = movesGet newBoard nextPlayer
    do
        print nextMove
        boardDisplay newBoard
        playGame newBoard nextPlayer nextMoves

            