module Display where

import Board
    (   Board
    ,   BoardRow
    )

boardDisplay :: Board -> IO()
boardDisplay [] = do
    putStrLn ""
boardDisplay (x : xs) = do
    rowDisplay x
    boardDisplay xs

rowDisplay :: BoardRow -> IO()
rowDisplay [] = do
    putStrLn "|"
rowDisplay (x : xs) = do
    putStr $ "|" ++ maybe " " show x
    rowDisplay xs
