{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}

 module Move
    where

import Board
    (   Board
    ,   Position
    )

import Data.Maybe

class Move_ a where
    movePlay :: Board -> a -> Board
    moveShow :: a -> String
    moveName :: a -> String

data Move = forall a. Move_ a => Move a


-- movesDisplay :: [MoveSingle] -> IO()
-- movesDisplay [] = do
--     putStrLn ""
-- movesDisplay (move : moves) = do
--     print move
--     movesDisplay moves
