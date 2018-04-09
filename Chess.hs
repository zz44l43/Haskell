module Piece (Color(..),Kind(..),Piece(..)) where

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
    show c = [color_chars !! fromEnum c]

instance Show Kind where
    show p = [kind_chars !! fromEnum p]

instance Show Piece where
    show (Piece c p) = show c ++ show p

instance Read Color where
    readsPrec _ = read_enum color_chars

instance Read Kind where
    readsPrec _ = read_enum kind_chars

read_enum :: Enum e => String -> String -> [(e,String)]
read_enum str (c:cs) =
    case elemIndex c str of
        Nothing -> []
        Just i -> [(toEnum i, cs)]

instance Read Piece where
    readsPrec _ string = 
        [(Piece c p,rest) | (p,rest0) <- reads string,
                            (c,rest)  <- reads rest0]
        
