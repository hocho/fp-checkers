{-# LANGUAGE InstanceSigs #-}

module StrategyFirst
    where

import Strategy
    (   Strategy_(..)
    )
import Move
import Analytics

data StrategyFirst = StrategyFirst
    {   dummy :: Int
    ,   dummy2 :: Int
    }

instance Strategy_ StrategyFirst where 
    getMove :: StrategyFirst -> [(Move, Analytics)] -> Maybe Move
    getMove strategy [] = Nothing
    getMove strategy ((move, _) : _) = Just move

    
    
        
