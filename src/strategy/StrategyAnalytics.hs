{-# LANGUAGE InstanceSigs #-}

module StrategyAnalytics
    where

import System.Random

import Strategy
    (   Strategy_(..), Strategy (Strategy)
    )
import Move 
    (   Move 
    )

import Analytics
    (   Analytics(preMoveCaptures, postMoveCaptures, captures) 
    )

import Data.List 
    (   sortOn 
    )

import Data.Bifunctor 
    (   Bifunctor(second) 
    )

newtype StrategyAnalytics = StrategyAnalytics
    {   stdGen :: StdGen
    }

mkStrategyAnalytics :: Int -> StrategyAnalytics
mkStrategyAnalytics seed = StrategyAnalytics $ mkStdGen seed

instance Strategy_ StrategyAnalytics where
    getMove :: StrategyAnalytics -> [(Move, Analytics)] -> (Maybe Move, Strategy)
    getMove strategy [] =
        (Nothing, Strategy strategy)
    getMove strategy moveAnalytics =
        let
            moveScoresSorted =
                sortOn
                    (((-1) * ) . snd)
                    $ map
                        (second computeScore)
                        moveAnalytics

            bestScore = snd $ head moveScoresSorted
            bestScores = takeWhile ((bestScore ==) . snd) moveScoresSorted
            lastIdx = length bestScores - 1

            (rndIdx, seed) = randomR(0, lastIdx) $ stdGen strategy
            move = fst (moveAnalytics !! rndIdx)
        in
            (Just move, Strategy (StrategyAnalytics seed))

computeScore :: Analytics -> Int
computeScore analytics =
        preMoveCaptures analytics
    -   postMoveCaptures  analytics
    +   captures analytics


