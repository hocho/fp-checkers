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

import Strategy
     (  Strategy(..)
     ,  Strategy_(getMove) 
     )

import StrategyFirst ()

import StrategyRandom 
    (   mkStrategyRandom 
    )

import StrategyAnalytics 
    (   mkStrategyAnalytics 
    )

import Analytics 
    (   analyticsGenerate 
    )

import Data.List ()

import Data.Array.IArray 
    (   (!)
    ,   (//)
    ,   array
    ,   Array 
    )

import System.Random 
    (   randomIO 
    )

buildArray :: [a] -> Array Int a
buildArray items =
    array(0, length items) $ zip [0..] items

gamePlay :: IO ()
gamePlay = do
    seed0 <- randomIO :: IO Int
    seed1 <- randomIO :: IO Int
    let
        board = boardInitial()
        -- strategy0 = Strategy $ StrategyFirst 0 0
        strategy0 = Strategy $ mkStrategyRandom seed0
        strategy1 = Strategy $ mkStrategyAnalytics seed1
        players = buildArray [player1 , player2]
        strategies = buildArray [strategy0, strategy1]
    do
        boardDisplay board
        gamePlay'
            board
            players
            strategies
            1
            0

otherPlay :: Int -> Int
otherPlay play = (play + 1)  `mod` 2

gamePlay' :: Board -> Array Int Player -> Array Int Strategy -> Int -> Int -> IO()
gamePlay' board players strategies num play = do
    let
        moveAnalytics = analyticsGenerate board (players!play)
        (move, newStrategy) = 
            case strategies!play of 
                (Strategy strat) -> getMove strat moveAnalytics
    do
        case move of
            Nothing ->
                return ()
            Just (Move nextMove) -> do
                let
                    newBoard = movePlay board nextMove
                    newPlay = 
                        if moveName nextMove == "MoveSingle"
                            then otherPlay play
                            else play
                    newStrategies = strategies//[(play, newStrategy)]
                do
                    putStr $ show num ++ ". "
                    putStr $ show (color (players!play)) ++ " "
                    putStrLn $ moveShow nextMove
                    putStrLn ""
                    boardDisplay newBoard
                    gamePlay' newBoard players newStrategies (num + 1) newPlay
