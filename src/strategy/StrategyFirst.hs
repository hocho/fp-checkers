{-# LANGUAGE InstanceSigs #-}

module StrategyFirst
    where

import Strategy
    (   Strategy_(..)
    ,   Strategy(..)
    )
import Move
import Analytics

data StrategyFirst = StrategyFirst
    {   dummy :: Int
    ,   dummy2 :: Int
    }

instance Strategy_ StrategyFirst where 
    getMove :: StrategyFirst -> [(Move, Analytics)] -> (Maybe Move, Strategy)
    getMove strategy [] = (Nothing, Strategy strategy)
    getMove strategy ((move, _) : _) = (Just move, Strategy strategy)

    
    
        
