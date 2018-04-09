module Chess (Color(..),Kind(..),Piece(..)) where

import Data.List

data Color = Black | White
    deriving (Eq, Ord, Bounded, Enum)
color_chars = "BW"

data Kind = King | Queen | Rock | Bishop | Knight | Pawn
    deriving (Eq, Ord, Bounded, Enum)
kind_chars = "KQRBKP"

data Piece = Piece Color Kind
    deriving (Eq, Bounded)

instance Show Color where
    show r = [color_chars !! fromEnum r]

instance Show Kind where
    show r = [kind_chars !! fromEnum r]

instance Show Piece where
    show (Piece c k) = show c ++ show k
