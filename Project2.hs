module Proj2 (initialGuess, GameState) where
import Piece
import Data.List

data GameState = GameState {pieces :: [String], colorRatio :: (Int, Int), previousGuess :: [[String]], previousResults :: [(Int,Int,Int)], answers :: [String], size :: Int} deriving (Eq)
instance Show GameState where show (GameState p _ _ _ _ _) = show p

allRightColor:: [String] -> Int -> Int -> Bool
allRightColor s r x
  | (length s - r) == x = True
  | otherwise = False

allWrongColor:: [String] -> Int -> Bool
allWrongColor _ 0 = True
allWrongColor _ _ = False

initialGuess :: Int -> ([String],GameState)
initialGuess  x
  | x == 3 = (["BK", "BK", "BK"], GameState ["BK","BQ","BR","BR","BB","BB","BN","BN","BP","BP","BP","BP","BP","BP","BP","BP","WK","WQ","WR","WR","WB","WB","WN","WN","WP","WP","WP","WP","WP","WP","WP","WP"] (0,0) [] [] [] x)
  | x == 4 = (["BK", "BR", "WQ", "WK"], GameState ["BK","BQ","BR","BR","BB","BB","BN","BN","BP","BP","BP","BP","BP","BP","BP","BP","WK","WQ","WR","WR","WB","WB","WN","WN","WP","WP","WP","WP","WP","WP","WP","WP"] (0,0) [] [] [] x)
  | x == 5 = (["BK", "BR", "BQ", "WQ", "WK"], GameState ["BK","BQ","BR","BR","BB","BB","BN","BN","BP","BP","BP","BP","BP","BP","BP","BP","WK","WQ","WR","WR","WB","WB","WN","WN","WP","WP","WP","WP","WP","WP","WP","WP"] (0,0) [] [] [] x)

generateInitialGuessForBlack :: Int -> [String]
generateInitialGuessForBlack 0 = []
generateInitialGuessForBlack x = "BK" : (generateInitialGuessForBlack (x-1))

generateInitialGuessForWhite :: Int -> [String]
generateInitialGuessForWhite 0 = []
generateInitialGuessForWhite x = "WQ" : (generateInitialGuessForWhite (x-1))

removeColorFromState :: [String] -> GameState -> GameState
removeColorFromState (x:xs) g
    | isBlack x == True = removeAllBlack [] g
    | otherwise = removeAllWhite [] g

isBlack :: String -> Bool
isBlack x
    | head x == 'B' = True
    | otherwise = False

removeAllBlack :: [String] -> GameState -> GameState
removeAllBlack s g = GameState (filter startsWithW (getGameStatePieces g) ++ s) (getGameStateColorRatio g) (getGameStatePrvGuess g) (getGameStatePrvResult g) (getGameAnswerResult g) (getGameSize g)

removeAllWhite :: [String] -> GameState -> GameState
removeAllWhite s g = GameState (filter startsWithB (getGameStatePieces g) ++ s) (getGameStateColorRatio g) (getGameStatePrvGuess g) (getGameStatePrvResult g) (getGameAnswerResult g) (getGameSize g)

removePieceFromState :: [String] -> GameState -> GameState
removePieceFromState [] g = g
removePieceFromState s g = GameState (removeMultiplePieces s (getGameStatePieces g)) (getGameStateColorRatio g) (getGameStatePrvGuess g) (getGameStatePrvResult g) (getGameAnswerResult g) (getGameSize g)

removeKindFromState :: [String] -> GameState -> GameState
removeKindFromState [] g = g
removeKindFromState s g = GameState (removeMultipleKinds s (getGameStatePieces g)) (getGameStateColorRatio g) (getGameStatePrvGuess g) (getGameStatePrvResult g) (getGameAnswerResult g) (getGameSize g)

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

updateColorRatioBasedOnBlack :: Int -> (Int,Int,Int) -> GameState  -> GameState
updateColorRatioBasedOnBlack x (f1,f2,f3) g
    | getGameStateColorRatio g  == (0,0) = GameState (getGameStatePieces g) ((f1+f3), (getGameStateColorWhiteRatio g)) (getGameStatePrvGuess g) (getGameStatePrvResult g) (getGameAnswerResult g) (getGameSize g)
    | otherwise = g

updateColorRatioBasedOnWhite :: Int -> (Int,Int,Int) -> GameState  -> GameState
updateColorRatioBasedOnWhite x (f1,f2,f3) g
    | getGameStateColorRatio g  == (0,0) = GameState (getGameStatePieces g) ((getGameStateColorBlackRatio g), (f1+f3)) (getGameStatePrvGuess g) (getGameStatePrvResult g) (getGameAnswerResult g) (getGameSize g)
    | otherwise = g

addPreviousGuess :: [String] -> GameState -> GameState
addPreviousGuess s g = GameState (getGameStatePieces g) (getGameStateColorRatio g) (insert s (getGameStatePrvGuess g)) (getGameStatePrvResult g) (getGameAnswerResult g) (getGameSize g)

addPreviousResult :: (Int,Int,Int) -> GameState -> GameState
addPreviousResult s g = GameState (getGameStatePieces g) (getGameStateColorRatio g) (getGameStatePrvGuess g) (insert s (getGameStatePrvResult g)) (getGameAnswerResult g) (getGameSize g)

addAnswerToState :: String -> Int -> GameState -> GameState
addAnswerToState s i g = GameState (getGameStatePieces g) (getGameStateColorRatio g) (getGameStatePrvGuess g) (getGameStatePrvResult g) (addAnswer s i (getGameAnswerResult g)) (getGameSize g)

addAnswer :: String -> Int -> [String] -> [String]
addAnswer s 0 g = g
addAnswer s i g = s : addAnswer s (i-1) g

reverseColor :: String -> String
reverseColor s
    | head s == 'B' = 'W' : tail s
    | otherwise = 'B' : tail s

updateGameRound :: [String] -> (Int,Int,Int) -> GameState -> GameState
updateGameRound s i g = addPreviousResult i (addPreviousGuess s g)

getGameRound :: GameState -> Int
getGameRound g = length (getGameStatePrvGuess g)

getGameStatePieces :: GameState -> [String]
getGameStatePieces (GameState p _ _ _ _ _) = p

getGameStateColorRatio :: GameState -> (Int, Int)
getGameStateColorRatio (GameState _ r _ _ _ _) = r

getGameStateColorBlackRatio :: GameState -> Int
getGameStateColorBlackRatio (GameState _ (b,w) _ _ _ _) = b

getGameStateColorWhiteRatio :: GameState -> Int
getGameStateColorWhiteRatio (GameState _ (b,w) _ _ _ _) = w

getGameStatePrvGuess :: GameState -> [[String]]
getGameStatePrvGuess (GameState _ _ g _ _ _) = g

getGameStatePrvResult :: GameState -> [(Int,Int,Int)]
getGameStatePrvResult (GameState _ _ _ r _ _) = r

getGameAnswerResult :: GameState -> [String]
getGameAnswerResult (GameState _ _ _ _ a _) = a

getGameSize :: GameState -> Int
getGameSize (GameState _ _ _ _ _ s) = s

getGameStateWithGuess :: ([String],GameState) -> GameState
getGameStateWithGuess (s,g) = g

updateGameState :: ([String], GameState) -> (Int,Int,Int) -> GameState
updateGameState (p, g) (x1, x2, x3)
    | x1 == 0 && x2 == 0 && x3 == 0 = removePieceFromState p (removeKindFromState p (removeColorFromState p g))
    | x1 == 0 && x2 == 0 = removePieceFromState p (removeKindFromState p g)
    | x1 == 0 && x3 == 0 = removePieceFromState p (removeColorFromState p g)
    | otherwise = g

updateAndMakeGuessAfterFirstRound :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
updateAndMakeGuessAfterFirstRound (p, g) (x1, x2, x3)
    | x1 > 0  && x2 > 0 = ((generateInitialGuessForWhite (getGameSize rightPieceAndRightKindState)), rightPieceAndRightKindState)
    | x1 > 0 = ((generateInitialGuessForWhite (getGameSize rightPieceState)), rightPieceState)
    | x2 == 0 = ((generateInitialGuessForWhite (getGameSize removeKindState)),removeKindState)
    | otherwise = (p, g)
  where
    updatedBlackState = updateColorRatioBasedOnBlack (getGameSize g) (x1, x2, x3) g
    rightPieceState = addAnswerToState (head p) x1 updatedBlackState
    rightPieceAndRightKindState = addAnswerToState (reverseColor (head p)) x2 rightPieceState
    removeKindState = removeKindFromState p updatedBlackState

nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (p, g) (x1, x2, x3)
    | getGameRound updatedState == 1 = (["BK"], updateColorRatioBasedOnBlack (getGameSize g) (x1, x2, x3) updatedState)
  where
    updatedState = updateGameRound p (x1, x2, x3) g
