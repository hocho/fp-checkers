{-# LANGUAGE InstanceSigs #-}

module StrategyRandom
    where

import System.Random

import Strategy
    (   Strategy_(..), Strategy (Strategy)
    )

import Move 
    (   Move 
    )

import Analytics 
    (   Analytics 
    )

newtype StrategyRandom = StrategyRandom
    {   stdGen :: StdGen
    }

mkStrategyRandom :: Int -> StrategyRandom
mkStrategyRandom seed = StrategyRandom $ mkStdGen seed

instance Strategy_ StrategyRandom where 
    getMove :: StrategyRandom -> [(Move, Analytics)] -> (Maybe Move, Strategy)
    getMove strategy [] = 
        (Nothing, Strategy strategy)
    getMove strategy moveAnalytics = 
        let
            lastIdx = length moveAnalytics - 1
            (rndIdx, seed) = randomR(0, lastIdx) $ stdGen strategy
            move = fst (moveAnalytics !! rndIdx)
        in
            (Just move, Strategy (StrategyRandom seed))

    
    
        
