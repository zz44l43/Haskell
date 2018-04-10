module Proj2 (initialGuess, nextGuess, GameState) where
import Piece
import Data.List

data GameState = GameState {pieces :: [String], previousGuess :: [String], previousResults :: [Int]}
	deriving (Eq)
instance Show GameState where
	show (GameState p _ _) = show p

allRightColor:: [String] -> Int -> Int -> Bool
allRightColor s r x
  | (length s - r) == x = True
  | otherwise = False

allWrongColor:: [String] -> Int -> Bool
allWrongColor _ 0 = True
allWrongColor _ _ = False

initialGuess :: Int -> ([String],GameState)
initialGuess  x
	| x == 3 = (["BK", "BR", "WQ"], GameState ["BK","BQ","BR","BR","BB","BB","BN","BN","BP","BP","BP","BP","BP","BP","BP","BP","WK","WQ","WR","WR","WB","WB","WN","WN","WP","WP","WP","WP","WP","WP","WP","WP"])
  | x == 4 = (["BK", "BR", "WQ", "WK"], GameState ["BK","BQ","BR","BR","BB","BB","BN","BN","BP","BP","BP","BP","BP","BP","BP","BP","WK","WQ","WR","WR","WB","WB","WN","WN","WP","WP","WP","WP","WP","WP","WP","WP"])
  | x == 5 = (["BK", "BR", "BQ", "WQ", "WK"], GameState ["BK","BQ","BR","BR","BB","BB","BN","BN","BP","BP","BP","BP","BP","BP","BP","BP","WK","WQ","WR","WR","WB","WB","WN","WN","WP","WP","WP","WP","WP","WP","WP","WP"])

removeAllBlack :: [String] -> GameState -> GameState
removeAllBlack s g = GameState (filter startsWithW (getGameStatePieces g) ++ s) (getGameStatePrvGuess g) (getGameStatePrvResult g)

removeAllWhite :: [String] -> GameState -> GameState
removeAllBlack s g = GameState (filter startsWithB (getGameStatePieces g) ++ s) (getGameStatePrvGuess g) (getGameStatePrvResult g)

removeAllKind :: String -> GameState -> GameState
removeAllKind s g =

removePieceFromState :: [String] -> GameState -> GameState
removePieceFromState [] g = []
removePieceFromState s g = GameState (removeMultiplePieces s (getGameStatePieces g)) (getGameStatePrvGuess g) (getGameStatePrvResult g)

removeKind :: String -> [String] -> [String]
removeKind _ [] = []
removeKind x (y:ys)
	| (last x) == (last y) = removeKind x ys
	| otherwise = y : removeKind x ys

removeMultiplePieces :: [String] -> [String] -> [String]
removeMultiplePieces [] s = s
removeMultiplePieces s [] = s
removeMultiplePieces (x:xs) y = removeMultiplePieces xs (removePiece x y)

removePiece :: String -> [String] -> [String]
removePiece _ [] = []
removePiece x (y:ys)
	| x == y = ys
	| otherwise = y : removePiece x ys

startsWithB :: String -> Bool
startsWithB [] = False
startsWithB (x:xs)
	| x == 'B' = True
	| otherwise = False

startsWithW :: String -> Bool
startsWithW [] = False
startsWithW (x:xs)
	| x == 'W' = True
	| otherwise = False

startsWithWAndException :: String -> [String]-> Bool
startsWithWAndException xs es
	| startsWithW xs == True = True
	| checkEleInTheList xs es == True = True
	| otherwise = False

checkEleInTheList :: (Eq a) => a -> [a] -> Bool
checkEleInTheList x = any (== x)

startsWithW [] = False
startsWithW (x:xs) es
	| x == 'W' == 0 = True
	| otherwise = False

getGameStatePieces :: GameState -> [String]
getGameStatePieces (GameState p _ _ ) = p

getGameStatePrvGuess :: GameState -> [String]
getGameStatePrvGuess (GameState _ g _) = g

getGameStatePrvResult :: GameState -> [Int]
getGameStatePrvResult (GameState _ _ r) = r

nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (s, g) (x1,x2,x3)
  | allRightColor == True
