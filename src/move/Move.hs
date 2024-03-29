{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}

 module Move
    where

import Board
    (   Board
    ,   BoardRow
    ,   Position
    ,   Piece
    )

import Data.Maybe

class Move_ a where
    movePlay :: Board -> a -> Board
    moveShow :: a -> String
    moveName :: a -> String

data Move = forall a. Move_ a => Move a

instance Show Move where 
    show :: Move -> String 
    show (Move move) =  moveShow move

-- Updates a row 
rowUpdate :: BoardRow -> Int -> Maybe Piece -> BoardRow
rowUpdate boardRow col piece =
    let
        (l, r) = splitAt col boardRow
    in
        l ++ piece : tail r
