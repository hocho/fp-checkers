module Main where

import Game
    (
        boardInitial
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
        board = boardInitial ()
        moves1 = movesGet board player1
        move = head moves1 
        newBoard = movePlay board move
        moves2 = movesGet newBoard player2 
        move2 = head moves2
        newBoard2 = movePlay newBoard move2
    boardDisplay board
    movesDisplay moves1
    boardDisplay newBoard
    boardDisplay newBoard2
 