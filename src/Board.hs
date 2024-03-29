{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveAnyClass #-}

 module Board
    where

import Data.Array.IArray 
    (   (!)
    ,    array
    ,   Array 
    )

import Data.Maybe 
    (   fromJust 
    )

import Data.List 
    (    unfoldr 
    )

boardSize = 8

data Side
    =   North
    |   South
    deriving (Show, Eq)

data Color
    =   Black
    |   White
    deriving (Show, Eq)

data Player = Player
    {   color :: Color
    ,   side :: Side
    }
    deriving (Show, Eq)

data PieceType
    =   Pawn
    |   Queen
    deriving (Show, Eq)

data Piece = Piece
    {   player :: Player
    ,   pieceType :: PieceType
    }
    deriving (Eq)

instance Show Piece where
    show piece = case color (player piece) of
        Black -> "B"
        White -> "W"

type Position = (Int, Int)

isValidPosition :: Position -> Bool
isValidPosition pos@(row, col) =
        isValid row
    &&  
        isValid col
    &&  
        isValidSquare pos
    where
        isValid x =
                x >= 0
            &&  
                x < boardSize
    
isValidSquare :: Position -> Bool 
isValidSquare (row, col) = odd (row + col)

type BoardRow = [Maybe Piece]
type Board = [BoardRow]

player0 = Player { color = Black, side = North }
player1 = Player { color = White, side = South }

otherPlayer player = if player == player0 then player1 else player0

pawn0 = Just Piece { player = player0, pieceType = Pawn}
pawn1 = Just Piece { player = player1, pieceType = Pawn}

queen0 = Just Piece { player = player0, pieceType = Queen}
queen1 = Just Piece { player = player1, pieceType = Queen}

-- Initialize board with Black - North and White - South
boardInitial :: () -> Board
boardInitial () =
    boardInitialize (fromJust pawn0) (fromJust pawn1)

-- Initialize board with 2 pieces
boardInitialize :: Piece -> Piece -> Board
boardInitialize p1 p2 =
    let
        br = buildRowFromStringDefault
    in
    [   br "| |0| |0| |0| |0|"
    ,   br "|0| |0| |0| |0| |"
    ,   br "| |0| |0| |0| |0|"
    ,   br "| | | | | | | | |"
    ,   br "| | | | | | | | |"
    ,   br "|1| |1| |1| |1| |"
    ,   br "| |1| |1| |1| |1|"
    ,   br "|1| |1| |1| |1| |"
    ]

boardPiece :: Board -> Position -> Maybe Piece
boardPiece board (row, col) = board !! row !! col

-- Returns true if the piece on the board position is of the player and the piece type
isPlayerPieceType :: Board -> Player -> PieceType -> Position -> Bool 
isPlayerPieceType board player pieceType position = 
    case boardPiece board position of
        Nothing ->
            False 
        Just (Piece piecePlayer piecePieceType) -> 
            piecePlayer == player && piecePieceType == pieceType

-- Returns all positions on the board which have the specified player and piece type
getPositions :: Board -> Player -> PieceType -> [Position]
getPositions board player pieceType =
    filter selectPlayerPieceType validPositions
    where
        selectPlayerPieceType = isPlayerPieceType board player pieceType
        validPositions = [(row, col) | row <- [0 .. boardSize - 1], col <- [0 .. boardSize - 1], isValidSquare(row, col)] 


-- Row string is of the form "| |1| |0| | | |"
-- where 0 and 1 are indexes into the Piece Array passed
buildRowFromString :: String -> Array Int Piece -> [Maybe Piece]
buildRowFromString str pieces =
  unfoldr 
    (\s -> if null s then Nothing else Just (toPiece (head s), tail s))
    str'
  where
    str' = filter (/='|') str 
    toPiece ch =
      case ch of
        '0' -> Just $ pieces!0
        '1' -> Just $ pieces!1
        '2' -> Just $ pieces!2
        '3' -> Just $ pieces!3
        ' ' -> Nothing
        _   -> error "Unexpected Character"               

buildRowFromStringDefault :: String -> [Maybe Piece]
buildRowFromStringDefault str =
    buildRowFromString str pieces
    where 
        pieces = array(0,3) $ zip [0 ..][fromJust pawn0, fromJust pawn1, fromJust queen0, fromJust queen1]
  
emptyRow = replicate boardSize Nothing

-- builds a board with the supplied rows, adds empty rows to round off the board
-- does not validate
buildBoardWithRows :: [BoardRow] -> Board
buildBoardWithRows rows = 
    buildBoardWithRows' rows boardSize
    where 
        buildBoardWithRows' [] 0 = 
            []

        buildBoardWithRows' [] count =
            emptyRow : buildBoardWithRows' [] (count - 1)

        buildBoardWithRows' (row : rows) count =
            row : buildBoardWithRows' rows (count - 1)

