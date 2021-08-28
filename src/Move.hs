{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE InstanceSigs #-}

 module Move
    where

import Game
    (   Board
    ,   Position
    )

import Data.Maybe

class MovePlayer a where
    movePlay :: Board -> a -> Board
    moveShow :: a -> String

data MoveSingle = MoveSingle
    {   from :: Position
    ,   to :: Position
    }
    deriving (Show)

data MoveJump = MoveJump
    {   fromX :: Position
    ,   toX:: Position
    }
    deriving (Show)

data Move 
    =   MoveSingleCtor MoveSingle
    |   MoveJumpCtor MoveJump 

movesDisplay :: [MoveSingle] -> IO()
movesDisplay [] = do
    putStrLn ""
movesDisplay (move : moves) = do
    print move
    movesDisplay moves
