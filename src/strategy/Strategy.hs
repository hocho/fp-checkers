{-# LANGUAGE ExistentialQuantification #-}
 module Strategy
    where

import Board
    (   Board
    ,   Player
    )
import Move
    (   Move
    )
import Analytics
    (   Analytics
    )

class Strategy_ a where
    getMove :: a -> [(Move, Analytics)] -> Maybe Move

data Strategy = forall a. Strategy_ a => Strategy a

