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

sameKind :: String -> String -> Bool
sameKind b w = tail b == (tail w)

getNumberOfKindForAColor :: String -> GameState -> Int
getNumberOfKindForAColor k g
    | head k == 'B' = length (filter (==k) (getGameStateBlackPieces g))
    | otherwise = length (filter (==k) (getGameStateWhitePieces g))
 

getDifferentKind :: String -> [String] -> String
getDifferentKind e [] = error "Oops! Different kind wrong"
getDifferentKind e (s:ss)
    |sameKind e s == True = getDifferentKind e ss
    |otherwise = s

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
updateColorRatioBasedOnBlack x (f1,f2,f3) g = GameState (getGameStatePieces g) ((f1+f3), (getGameStateColorWhiteRatio g)) (getGameStatePrvGuess g) (getGameStatePrvResult g) (getGameAnswerResult g) (getGameSize g)

updateColorRatioBasedOnWhite :: Int -> (Int,Int,Int) -> GameState  -> GameState
updateColorRatioBasedOnWhite x (f1,f2,f3) g = GameState (getGameStatePieces g) ((getGameStateColorBlackRatio g), (f1+f3)) (getGameStatePrvGuess g) (getGameStatePrvResult g) (getGameAnswerResult g) (getGameSize g)

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

getGameStateBlackPieces :: GameState -> [String]
getGameStateBlackPieces g = filter startsWithB (getGameStatePieces g)

getGameStateWhitePieces :: GameState -> [String]
getGameStateWhitePieces g = filter startsWithW (getGameStatePieces g)

getGameStateColorRatio :: GameState -> (Int, Int)
getGameStateColorRatio (GameState _ r _ _ _ _) = r

getTotalPieces :: GameState -> Int
getTotalPieces g = (getGameStateColorBlackRatio g) + (getGameStateColorWhiteRatio g)

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

getGameAnswerBlackResult :: GameState -> [String]
getGameAnswerBlackResult g = filter startsWithB (getGameAnswerResult g)

getGameAnswerWhiteResult :: GameState -> [String]
getGameAnswerWhiteResult g = filter startsWithW (getGameAnswerResult g)

getGameSize :: GameState -> Int
getGameSize (GameState _ _ _ _ _ s) = s

getGameStateWithGuess :: ([String],GameState) -> GameState
getGameStateWithGuess (s,g) = g

getGuess :: ([String],GameState) -> [String]
getGuess (s,g) = s

updateGameState :: ([String], GameState) -> (Int,Int,Int) -> GameState
updateGameState (p, g) (x1, x2, x3)
    | x1 == 0 && x2 == 0 && x3 == 0 = removePieceFromState p (removeKindFromState p (removeColorFromState p g))
    | x1 == 0 && x2 == 0 = removePieceFromState p (removeKindFromState p g)
    | x1 == 0 && x3 == 0 = removePieceFromState p (removeColorFromState p g)
    | otherwise = g

updateAndMakeGuessAfterFirstRound :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
updateAndMakeGuessAfterFirstRound (p, g) (x1, x2, x3)
    | x1 > 0  && x2 > 0 = ((generateInitialGuessForWhite (getGameSize rightPieceAndRightKindState)), rightPieceAndRightKindState)
    | x1 > 0  && x2 == 0 = ((generateInitialGuessForWhite (getGameSize rightPieceAndNoMoreKindState)), rightPieceAndNoMoreKindState)
    | x2 == 0 = ((generateInitialGuessForWhite (getGameSize wrongPieceAndNoMoreKindState)),wrongPieceAndNoMoreKindState)
    | x2 > 0 = ((generateInitialGuessForWhite (getGameSize wrongPieceAndRightKindState)),wrongPieceAndRightKindState)
    | otherwise = (generateInitialGuessForWhite (getGameSize noBlack), noBlack)
  where
    updatedBlackState = updateColorRatioBasedOnBlack (getGameSize g) (x1, x2, x3) g
    rightPieces = replicate x1 (head p)
    rightPieceState = removePieceFromState rightPieces (addAnswerToState (head p) x1 updatedBlackState)
    rightPieceAndRightKindState = addAnswerToState (reverseColor (head p)) x2 rightPieceState
    rightPieceAndNoMoreKindState = removePieceFromState [(reverseColor (head p))] rightPieceState
    wrongPiece = replicate x1 (head p)
    wrongPieceState = removePieceFromState wrongPiece updatedBlackState
    wrongPieceAndRightKindState = addAnswerToState (reverseColor (head p)) x2 wrongPieceState
    wrongPieceAndNoMoreKindState = removePieceFromState [(reverseColor (head p))] wrongPieceState
    removeKindState = removeKindFromState p updatedBlackState
    noBlack = removeAllBlack [] updatedBlackState

updateAndMakeGuessAfterSecondRound :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
updateAndMakeGuessAfterSecondRound (p, g) (x1, x2, x3)
    | x1 > 0  && x2 > 0 = ((generateGuess rightPieceAndRightKindState), rightPieceAndRightKindState)
    | x1 > 0  && x2 == 0 = ((generateGuess  rightPieceAndNoMoreKindState), rightPieceAndNoMoreKindState)
    | x2 == 0 = ((generateGuess wrongPieceAndNoMoreKindState),wrongPieceAndNoMoreKindState)
    | x2 > 0 = ((generateGuess wrongPieceAndRightKindState),wrongPieceAndRightKindState)
    | otherwise = (generateGuess noWhite, noWhite)
  where
    updatedWhiteState = updateColorRatioBasedOnWhite (getGameSize g) (x1, x2, x3) g
    rightPieces = replicate x1 (head p)
    rightPieceState = removePieceFromState rightPieces (addAnswerToState (head p) x1 updatedWhiteState)
    rightPieceAndRightKindState = addAnswerToState (reverseColor (head p)) x2 rightPieceState
    rightPieceAndNoMoreKindState = removePieceFromState [(reverseColor (head p))] rightPieceState
    wrongPiece = replicate x1 (head p)
    wrongPieceState = removePieceFromState wrongPiece updatedWhiteState
    wrongPieceAndRightKindState = addAnswerToState (reverseColor (head p)) x2 wrongPieceState
    wrongPieceAndNoMoreKindState = removePieceFromState [(reverseColor (head p))] wrongPieceState
    removeKindState = removeKindFromState p updatedWhiteState
    noWhite = removeAllWhite [] updatedWhiteState

updateAndMakeGuessAfterThirdRound :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
updateAndMakeGuessAfterThirdRound (p, g) (x1, x2, x3)
    | newFeedbackForCorrectPieces > 0  && x2 > 0 = ((generateGuess rightPieceAndRightKindState), rightPieceAndRightKindState)
    | newFeedbackForCorrectPieces > 0  && x2 == 0 = ((generateGuess  rightPieceAndNoMoreKindState), rightPieceAndNoMoreKindState)
    | x2 == 0 = ((generateGuess wrongPieceAndNoMoreKindState),wrongPieceAndNoMoreKindState)
    | x2 > 0 = ((generateGuess wrongPieceAndRightKindState),wrongPieceAndRightKindState)
    | otherwise = error "Oops"
  where
    newPieces = p \\ (getGameAnswerResult g)
    newFeedbackForCorrectPieces = x1 - length (getGameAnswerResult g)
    rightPieces = replicate newFeedbackForCorrectPieces (head newPieces)
    multipleRightPieces = replicate (getNumberOfKindForAColor (head newPieces) g) (head newPieces)
    rightPieceState = removePieceFromState multipleRightPieces (addAnswerToState (head newPieces) newFeedbackForCorrectPieces g)
    rightPieceAndRightKindState = addAnswerToState (reverseColor (head newPieces)) x2 rightPieceState
    rightPieceAndNoMoreKindState = removePieceFromState reverseColorKinds rightPieceState
    reverseColorPiece = reverseColor (head newPieces)
    reverseColorKinds = replicate (getNumberOfKindForAColor reverseColorPiece rightPieceState) reverseColorPiece
    wrongPiece = head newPieces
    multipleWrongPieces = replicate (getNumberOfKindForAColor wrongPiece g) wrongPiece
    wrongPieceState = removePieceFromState multipleWrongPieces g
    wrongPieceAndRightKindState = addAnswerToState (reverseColor wrongPiece) x2 wrongPieceState
    wrongPieceAndNoMoreKindState = removePieceFromState [(reverseColor wrongPiece)] wrongPieceState

generateGuess :: GameState -> [String]
generateGuess g
    | needBlack == True = blackGuess ++ (getGameAnswerResult g)
    | otherwise = generateGuessForWhiteOnly g ++ (getGameAnswerResult g)
  where
    blackGuess = generateGuessForBlackOnly g
    needBlack = (getGameStateColorBlackRatio g) - (length (getGameAnswerBlackResult g)) > 0

generateGuessForBlackOnly :: GameState -> [String]
generateGuessForBlackOnly g = replicate remindingGuess (head (getGameStateBlackPieces g))
  where
    totalPieces = getTotalPieces g
    correctPieces = length (getGameAnswerResult g)
    remindingGuess = totalPieces - correctPieces


generateGuessForWhiteOnly :: GameState -> [String]
generateGuessForWhiteOnly g = replicate remindingGuess (head (getGameStateWhitePieces g))
  where
    totalPieces = getTotalPieces g
    correctPieces = length (getGameAnswerResult g)
    remindingGuess = totalPieces - correctPieces

generateGuessForBlack :: GameState -> [String]
generateGuessForBlack g
    | (getGameStateColorBlackRatio g) - (length (getGameAnswerBlackResult g)) > 0 = replicate ((getGameStateColorBlackRatio g) - (length (getGameAnswerBlackResult g))) (head (getGameAnswerBlackResult g))
    | otherwise = []

generateGuessForWhite ::GameState -> [String]
generateGuessForWhite g
    | (getGameStateColorWhiteRatio g) - (length (getGameAnswerWhiteResult g)) > 0 = replicate ((getGameStateColorWhiteRatio g) - (length (getGameAnswerWhiteResult g))) (head (getGameAnswerWhiteResult g))
    | otherwise = []

generateGuessForWhiteWithException :: GameState -> String -> [String]
generateGuessForWhiteWithException g e
    | needMoreGuess == True = replicate howManyIsNeeded (getDifferentKind e (getGameAnswerWhiteResult g))
    | otherwise = []
  where
    howManyIsNeeded = (getGameStateColorWhiteRatio g) - (length ((getGameAnswerWhiteResult g)))
    needMoreGuess = (getGameStateColorWhiteRatio g) - (length (getGameAnswerWhiteResult g)) > 0
    
nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (p, g) (x1, x2, x3)
    | getGameRound updatedState == 1 = updateAndMakeGuessAfterFirstRound (p, updatedState) (x1,x2,x3)
    | getGameRound updatedState == 2 = updateAndMakeGuessAfterSecondRound (p, updatedState) (x1,x2,x3)
    | otherwise = updateAndMakeGuessAfterThirdRound(p,updatedState) (x1,x2,x3)
  where
    updatedState = updateGameRound p (x1, x2, x3) g