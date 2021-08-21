module Main where

import Game
    (
        boardInitial
    ,   boardDisplay    
    ,   movesGet
    ,   movesDisplay
    ,   player1
    ,   player2
    )

main :: IO ()
main = do
    let
        board = boardInitial ()
        moves = movesGet board player1
    boardDisplay board
    movesDisplay moves
