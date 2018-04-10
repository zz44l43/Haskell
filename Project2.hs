module Proj2 (initialGuess, GameState) where
import Piece
import Data.List

data GameState = GameState {pieces :: [String], previousGuess :: [[String]], previousResults :: [[Int]]} deriving (Eq)
instance Show GameState where show (GameState p _ _) = show p

allRightColor:: [String] -> Int -> Int -> Bool
allRightColor s r x
  | (length s - r) == x = True
  | otherwise = False

allWrongColor:: [String] -> Int -> Bool
allWrongColor _ 0 = True
allWrongColor _ _ = False

initialGuess :: Int -> ([String],GameState)
initialGuess  x
  | x == 3 = (["BK", "BR", "WQ"], GameState ["BK","BQ","BR","BR","BB","BB","BN","BN","BP","BP","BP","BP","BP","BP","BP","BP","WK","WQ","WR","WR","WB","WB","WN","WN","WP","WP","WP","WP","WP","WP","WP","WP"] [] [])
  | x == 4 = (["BK", "BR", "WQ", "WK"], GameState ["BK","BQ","BR","BR","BB","BB","BN","BN","BP","BP","BP","BP","BP","BP","BP","BP","WK","WQ","WR","WR","WB","WB","WN","WN","WP","WP","WP","WP","WP","WP","WP","WP"] [] [])
  | x == 5 = (["BK", "BR", "BQ", "WQ", "WK"], GameState ["BK","BQ","BR","BR","BB","BB","BN","BN","BP","BP","BP","BP","BP","BP","BP","BP","WK","WQ","WR","WR","WB","WB","WN","WN","WP","WP","WP","WP","WP","WP","WP","WP"] [] [])


removeColorFromState :: [String] -> GameState -> GameState
removeColorFromState (x:xs) g
    | isBlack x == True = removeAllBlack [] g
    | otherwise = removeAllWhite [] g

isBlack :: String -> Bool
isBlack x
    | head x == 'B' = True
    | otherwise = False

removeAllBlack :: [String] -> GameState -> GameState
removeAllBlack s g = GameState (filter startsWithW (getGameStatePieces g) ++ s) (getGameStatePrvGuess g) (getGameStatePrvResult g)

removeAllWhite :: [String] -> GameState -> GameState
removeAllWhite s g = GameState (filter startsWithB (getGameStatePieces g) ++ s) (getGameStatePrvGuess g) (getGameStatePrvResult g)

removePieceFromState :: [String] -> GameState -> GameState
removePieceFromState [] g = g
removePieceFromState s g = GameState (removeMultiplePieces s (getGameStatePieces g)) (getGameStatePrvGuess g) (getGameStatePrvResult g)

removeKindFromState :: [String] -> GameState -> GameState
removeKindFromState [] g = g
removeKindFromState s g = GameState (removeMultipleKinds s (getGameStatePieces g)) (getGameStatePrvGuess g) (getGameStatePrvResult g)

removeMultipleKinds :: [String] -> [String] -> [String]
removeMultipleKinds [] s = s
removeMultipleKinds s [] = s
removeMultipleKinds (x:xs) y = removeMultipleKinds xs (removeKind x y)

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

getGameStatePieces :: GameState -> [String]
getGameStatePieces (GameState p _ _ ) = p

getGameStatePrvGuess :: GameState -> [[String]]
getGameStatePrvGuess (GameState _ g _) = g

getGameStatePrvResult :: GameState -> [[Int]]
getGameStatePrvResult (GameState _ _ r) = r

updateGameState :: ([String], GameState) -> (Int,Int,Int) -> GameState
updateGameState (p, g) (x1, x2, x3)
    | x1 == 0 && x2 == 0 && x3 == 0 = removePieceFromState p (removeKindFromState p (removeColorFromState p g))
    | x1 == 0 && x2 == 0 = removePieceFromState p (removeKindFromState p g)
    | x1 == 0 && x == 0 = removePieceFromState p (removeKindFromState p g)
    | otherwise = g

