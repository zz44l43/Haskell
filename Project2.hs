{-|

Author: Zhi Zheng, 327965, <zhiz2@student.unimelb.edu.au>, The University of Melbourne
Purpose: Create a ChessGuess two-player logical guessing game.
Subject: Declarative Programming COMP90048

ChessGuess is a two-player logical guessing game created for this project. You will not find any information about the game anywhere else, but it is a simple game and this specification will tell you all you need to know.
For a ChessGuess game, one player will be the hider and the other is the guesser.
The hider begins by selecting the size of the game which can be from 0 to 32.
The hider then selects up to size chess pieces out of a chess set and hides them.
Each piece has a colour one of ’B’ black or ’W’ white and a kind: ’K’ king, ’Q’ queen, ’R’ rook, ’B’ bishop, ’N’ knight or ’P’ pawn.
We will represent pieces by length 2 strings, e.g. "BK" is a black king, and "WP" is a white pawn. Note that they are selected from a standard chess set so there is e.g. at most two black rooks that can be selected, or at most 8 white pawns.

-}

module Project2 (initialGuess, nextGuess, GameState) where
import Data.List

{-|
 Definition for the GameState object
 Pieces this is the amount of avaliable chess pieces that can be selected for each round of guess
 colorRatio determines how many correct pieces are required for each color.
 previousGuess provicded the historical guessed pieces
 previousGuess provicded the historical guessed feedback
 answers provided the known pieces for the corrected piece from previous rounds

-}

data GameState = GameState {pieces :: [String], colorRatio :: (Int, Int), previousGuess :: [[String]], previousResults :: [(Int,Int,Int)], answers :: [String], size :: Int} deriving (Eq)
instance Show GameState where show (GameState p _ _ _ _ _) = show p

{-|
The first round of guess that only guess the black king pieces so we can determines how black pieces are there in the game
Also initialized GameState with all the avaliable pieces
-}

initialGuess :: Int -> ([String],GameState)
initialGuess  x = ((replicate x "BK"), GameState ["BK","BQ","BR","BR","BB","BB","BN","BN","BP","BP","BP","BP","BP","BP","BP","BP","WK","WQ","WR","WR","WB","WB","WN","WN","WP","WP","WP","WP","WP","WP","WP","WP"] (0,0) [] [] [] x)

{-|
The first two rounds of guess are pretty much to determine how many black and white pieces are in the final correct answer
The second round will guess all the whites one and post-process the guess from the first round of guessing.
Starting from the second round the guess will be 1 kind at a time.
Since we know how many black and white pieces from the frist 2 rounds we can use the feedback to work wether there is such kind in the final correct answer.
For example,
In the third round we guess "BR" plus any correct answer we know so far (this can only be BK WK BQ and WQ from the first 2 rounds)
As the result at least we can know how many Root are there in the black and white. Once we know that we add the corresonding correct guess to the answer.
This algorithm will work well for any size of the game even in a game with size of 32 it should take at most 7 round.
And should never running more than 10s.
-}

nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (p, g) (x1, x2, x3)
    | getGameRound updatedState == 1 = updateAndMakeGuessAfterFirstRound (p, updatedState) (x1,x2,x3)
    | getGameRound updatedState == 2 = updateAndMakeGuessAfterSecondRound (p, updatedState) (x1,x2,x3)
    | otherwise = updateAndMakeGuessAfterThirdRound(p,updatedState) (x1,x2,x3)
  where
    updatedState = updateGameRound p (x1, x2, x3) g

{-|
Post process the first round of guess that is all the "BK "
From the feedback this round can know wether there is King in the final correct answer with first and second feedback values
Then we remove the King for both black and white from the avalaible pieces in the GameState.
The third value form the feedback combine with the first value in the feedback can determine how many black pieces are there inthe final correct answer
Then make the guess for the next round with all the "WQ" plus any of the correct answer we know.
-}

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
    wrongPiece = [(head p)]
    wrongPieceState = removePieceFromState wrongPiece updatedBlackState
    wrongPieceAndRightKindState = addAnswerToState (reverseColor (head p)) x2 wrongPieceState
    wrongPieceAndNoMoreKindState = removePieceFromState [(reverseColor (head p))] wrongPieceState
    removeKindState = removeKindFromState p updatedBlackState
    noBlack = removeAllBlack [] updatedBlackState

{-|
Similar to the previous function but opposite by determing how many white pieces are there in the final answer with different of kind, Queen.
Post process the first round of guess that is all the "WQ"
From the feedback this round can know wether there is Queen in the final correct answer with first and second feedback values
Then we remove the Queen for both black and white from the avalaible pieces in the GameState.
The third value form the feedback combine with the first value in the feedback can determine how many white pieces are there inthe final correct answer
Then make the guess for the next round with all the "WQ" plus any of the correct answer we know.
-}

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
    wrongPiece = [(head p)]
    wrongPieceState = removePieceFromState wrongPiece updatedWhiteState
    wrongPieceAndRightKindState = addAnswerToState (reverseColor (head p)) x2 wrongPieceState
    wrongPieceAndNoMoreKindState = removePieceFromState [(reverseColor (head p))] wrongPieceState
    removeKindState = removeKindFromState p updatedWhiteState
    noWhite = removeAllWhite [] updatedWhiteState

{-|
Once we know how many whites and blacks are in the final answer we can determine by going through each of kind each round until
each round we can determine whether the guessed kind is in the final correct answer or not (both number and color)
So we go through all the Root, Bishop, Knight, and Pawn in the worst case we can correclty guessed the final answer. (So it will never need >7 guess no matter what size of the game is)
-}

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

-- |generate guess for each round which composite of new guess value along with the known correct values

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

allRightColor:: [String] -> Int -> Int -> Bool
allRightColor s r x
  | (length s - r) == x = True
  | otherwise = False

allWrongColor:: [String] -> Int -> Bool
allWrongColor _ 0 = True
allWrongColor _ _ = False

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
