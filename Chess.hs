module Chess (Color(..),Piece(..),Chess(..)) where

import Data.List

data Color = Black | White
    deriving (Eq, Ord, Bounded, Enum)
color_chars = "BW"

data Piece = King | Queen | Rock | Bishop | Knight | Pawn
    deriving (Eq, Ord, Bounded, Enum)
piece_chars = "KQRBKP"

data Chess = Chess Color Piece
    deriving (Eq, Bounded)

