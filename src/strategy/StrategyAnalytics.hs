{-# LANGUAGE InstanceSigs #-}

module StrategyAnalytics
    where

import System.Random

import Strategy
    (   Strategy_(..), Strategy (Strategy)
    )
import Move
import Analytics
import Foreign (Int)
import Data.List

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
                        (\(move, analytics) -> (move, computeScore analytics)) 
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


