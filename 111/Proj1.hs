{-|
Declarative Programming COMP90048 
For this project, you will implement a two-player logical guessing game. 
Two players face each other, each with a complete standard deck of western playing cards (without jokers). 
One player will be the answerer and the other is the guesser. 
The answerer begins by selecting some number of cards from his or her deck without showing the guesser. 
These cards will form the answer for this game. 
The aim of the game is for the guesser to guess the answer.
By Zhi Zheng 327965
-}
module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List


match1 :: Card -> [Card] -> Bool
match1 _ [] = False
match1 c (y:ys)
	| c == y = True
	| otherwise = match1 c ys

-- |The 'feedback1' function: How many of the cards in the answer are also in the guess
feedback1 :: [Card] -> [Card] -> Int
feedback1 _ [] = 0;
feedback1 [] _ = 0;
feedback1 (x:xs) ys
	| match1 x ys == True = 1 + feedback1 xs ys
	| otherwise = feedback1 xs ys
	
readRank :: Card -> Rank
readRank (Card _ rank) = rank

findLowest ::  [Card] -> Rank
findLowest [x] = readRank x
findLowest (x:x2:xs)
	| readRank x > readRank x2 = findLowest (x2:xs)
	| otherwise = findLowest (x:xs)

-- |The 'feedback2' function: How many cards in the answer have rank lower than the lowest rank 
--  in the guess
feedback2 :: [Card] -> [Card] -> Int
feedback2 [] [] = 0;
feedback2 _ [] = 0;
feedback2 [] _ = 0;
feedback2 (x:xs) ys
	| (readRank x) < (findLowest ys) = 1 + feedback2 xs ys
	| otherwise = feedback2 xs ys

matchRank :: Card -> [Card] -> Bool
matchRank _ [] = False
matchRank c (y:ys)
	| readRank c == readRank y = True
	| otherwise = matchRank c ys

toRanks :: [Card] -> [Rank]
toRanks xs = map (readRank) xs

provideAnswer:: [Rank] -> [Rank] -> Int
provideAnswer [] [] =0
provideAnswer xs [] = 0
provideAnswer [] ys = 0
provideAnswer (x:xs) (y:ys)
	| x == y = 1 + provideAnswer xs ys
	| x < y = provideAnswer xs (y:ys)
	| otherwise = provideAnswer (x:xs) ys

-- |The 'feedback3' function: How many of the cards in the answer have the same rank as a card in the guess
feedback3 :: [Card] -> [Card] -> Int
feedback3 [] [] = 0
feedback3 [] ys = 0
feedback3 xs [] = 0
feedback3 xs ys = provideAnswer (sort (toRanks xs)) (sort (toRanks ys))

findHighest ::  [Card] -> Rank
findHighest [x] = readRank x
findHighest (x:x2:xs)
	| readRank x < readRank x2 = findHighest (x2:xs)
	| otherwise = findHighest (x:xs)

-- |The 'feedback4' function: How many cards in the answer have rank higher than the highest rank in the guess
feedback4 :: [Card] -> [Card] -> Int
feedback4 [] [] = 0
feedback4 [] ys = 0
feedback4 xs [] = 0
feedback4 (x:xs) ys
	| (readRank x) > (findHighest ys) = 1 + feedback4 xs ys
	| otherwise = feedback4 xs ys


readSuit :: Card -> Suit
readSuit (Card suit _) = suit
	
toSuit:: [Card] -> [Suit]
toSuit xs = map (readSuit) xs

provideSuiteAnswer :: [Suit] -> [Suit] -> Int
provideSuiteAnswer [] [] =0
provideSuiteAnswer xs [] = 0
provideSuiteAnswer [] ys = 0
provideSuiteAnswer (x:xs) (y:ys)
	| x == y = 1 + provideSuiteAnswer xs ys
	| x < y = provideSuiteAnswer xs (y:ys)
	| otherwise = provideSuiteAnswer (x:xs) ys	

-- |The 'feedback5' function: How many of the cards in the answer have the same suit as a card in the guess, only counting a card in the guess onc
feedback5 :: [Card] -> [Card] -> Int
feedback5 [] [] = 0
feedback5 [] ys = 0
feedback5 xs [] = 0
feedback5 xs ys = provideSuiteAnswer (sort (toSuit xs)) (sort (toSuit ys))

-- |takes a target and a guess (in that order), each represented as a list of Cards, and returns the five feedback numbers, as explained above, as a tuple.
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback xs ys = (feedback1 xs ys, feedback2 xs ys, feedback3 xs ys, feedback4 xs ys, feedback5 xs ys)

-- | The GameState object is constructed with reminding ranks, 
--	 suits and a counter of how many times guess had occured and using this to guess different values.
data GameState = GameState {rank :: [Rank], suit :: [Suit], numChangeRank :: Int, numChangeSuit :: Int}
	deriving (Eq)

instance Show GameState where
	show (GameState g c _ _) = show g ++ show c
	
{-|
 Takes the number of cards in the answer as input and returns a pair of an initial guess, which should be a list of the specified number of cards, and a game state. 
 The number of cards specified will be 2 for most of the test, and 3 or 4 for the remaining tests, as explained below.
 -}	
initialGuess :: Int -> ([Card],GameState)
initialGuess  x 
	| x == 2 = ([Card Club R2, Card Diamond R5], GameState [R2,R3,R4,R5,R6,R7,R8,R9,R10,Jack,Queen,King,Ace] [Club,Diamond,Heart,Spade] 0 0)
	| x == 3 = ([Card Club R2, Card Club R5, Card Club R6], GameState [R2,R3,R4,R5,R6,R7,R8,R9,R10,Jack,Queen,King,Ace] [Club,Diamond,Heart,Spade] 0 0)
	| x == 4 = ([Card Club R2, Card Club R5, Card Club R6, Card Club R7], GameState [R2,R3,R4,R5,R6,R7,R8,R9,R10,Jack,Queen,King,Ace] [Club,Diamond,Heart,Spade] 0 0)
	
getSuitFromGameState :: GameState -> [Suit]
getSuitFromGameState (GameState _ s _ _) = s
	
getRankFromGameState :: GameState -> [Rank]
getRankFromGameState (GameState r _ _ _ ) = r
	
getRankCountGS :: GameState -> Int
getRankCountGS (GameState _ _ r _) = r

getSuitCountGS :: GameState -> Int
getSuitCountGS (GameState _ _ _ s) = s
{-|
 This function will will produce the guess outcome by taking in a list of suit and rank and return a list of cards
 when the suit has only one valid value it will produce the outcome that has duplicate suit. This is where guard of length s == 1 is written for. 
 -}	
changeGuess :: [Suit] -> [Rank] -> [Card]
changeGuess [][] = []
changeGuess _ [] = []
changeGuess [] _ = []
changeGuess s r 
	|length s == 1 = Card (head s) (head r) : changeGuess [(head s)] (tail r)
	|otherwise = Card (head s) (head r) : changeGuess (tail s) (tail r)

suitRankSwap :: [Rank] -> [Suit]-> [Card]
suitRankSwap r s
    | length s == 2 = [Card (head s) (last r), Card (last s) (head r)]
    | length s == 3 = [Card (head s) (last r), Card (last s) (head r)]

{-|
 This function will filter out the avaliable ranks based on the cases that defined from the NextGuess.
 Basically is depend on the feedback's value such as samller, correct and greater.
 -}	
guessRank :: [Rank] -> [Rank] -> Int -> [Rank]
guessRank rs (r1:r2:rr) x
    | x == 1 = filter (<r1) rs ++ (filter (>r2) rs)
    | x == 2 = filter (<r1) rs
    | x == 3 = filter (>r2) rs
    | x == 4 = filter (<r2) rs \\ (r1:r2:rr)
    | x == 5 = filter (>r1) rs \\ (r1:r2:rr)
    | x == 6 = nub (intersect (filter (>r1) rs) (filter (<r2) rs)) \\ (r1:r2:rr)
    | x == 7 = nub (filter (<=r1) rs ++ [r2])
    | x == 8 = nub ([r1] ++ (filter (>=r2) rs))
    | x == 9 = nub (intersect (filter (>=r1) rs) (filter (<=r2) rs))
	
-- |the only functions below are for the situation where feedback shows only 1 suit is correct
onlySuit1 :: [Suit] -> [Suit]
onlySuit1 xs
    | length xs == 2 = [head xs, head xs]
    | otherwise = [xs!!2, xs!!3]

onlySuit2 :: [Suit] -> [Suit]
onlySuit2 xs = [xs!!0, xs!!3]

onlySuit3 :: [Suit] -> [Suit]
onlySuit3 xs = [xs!!1, xs!!2]

onlySuit4 :: [Suit] -> [Suit]
onlySuit4 xs = [xs!!1, xs!!3]

{-|
Pick a list of ranks from a narrowed down pool. 
Each time take middle two as in most of cases
but for lengh of pool is three it will take duplicate value in order to brutal force the final pool
-}
pickRank :: [Rank] -> [Rank]
pickRank (x1:[]) = [x1,x1]
pickRank (x1:x2:[]) = [x1,x2]
pickRank xs
	| length xs == 3 = [xs!!((length xs)`div`2), xs!!((length xs)`div`2)]
	| otherwise = [xs!!(((length xs)`div`2) -1), xs!!((length xs)`div`2)]
-- |the only functions below are for the situation where feedback shows only 1 suit is correct
selectRank :: [Rank] -> [Rank]
selectRank (x1:x2:[]) = [x1,x1]

isRankCorrectSuitIncorrect :: Int -> Int -> Int -> Bool
isRankCorrectSuitIncorrect x1 x2 n
	| x1 == n && x2 < n = True
	| otherwise = False
	
isOnlyOneRankCorrectNoGreater :: Int -> Int -> Bool
isOnlyOneRankCorrectNoGreater x1 x2
	| x1 == 0 && x2 == 1 = True
	| otherwise = False
{-|
 input a pair of the previous guess and game state, 
 and the feedback to this guess as a quintuple of counts of correct cards,
 low ranks, correct ranks, high ranks, and correct suits, 
 and returns a pair of the next guess and new game state.
 When the rank is correct but suit is incorrect the counter for the ranks within the game state will increase by one to add extra state.
 When there is only one rank is correct but no greater or smaller in ranks and the suit count is either 0 or 1 we will yield different pool of avaliable ranks for guess.
 Also defined the cases in the where clauses for calling the guessRank function to filter out the avaliable pool of ranks.
 
 -}
nextGuess :: ([Card], GameState) -> (Int,Int,Int,Int,Int) -> ([Card], GameState)
nextGuess (c, g) (x1,x2,x3,x4,x5)
    | isOnlyOneRankCorrectNoGreater x4 x3 == True && x2 == 1 
		= (changeGuess cardSuit r7, GameState r7s gsSuit 0 0)
    | isOnlyOneRankCorrectNoGreater x4 x3 == True && x2 == 0 && suitCountGS == 0 
		= (changeGuess cardSuit r9, GameState r9s gsSuit 0 suitCountIncrease)
    | isOnlyOneRankCorrectNoGreater x4 x3 == True && x2 == 0 && suitCountGS == 1 
		= (changeGuess cardSuit r10, GameState gsRank gsSuit 0 suitCountIncrease)
	| x4 == 1 && x3 == 1 && x2 == 0 = (changeGuess cardSuit r8, GameState r8s gsSuit 0 0)
	| x3 == n && x5 == n = (suitRankSwap cardRank cardSuit, g)
    | x3 == n && x5 == 0 = (changeGuess suit1 cardRank, GameState cardRank suit1 0 0)
    | isRankCorrectSuitIncorrect x3 x5 n == True && rankCountGS == 0 
		= (changeGuess suit2 cardRank, GameState cardRank gsSuit rankCountIncrease 0)
    | isRankCorrectSuitIncorrect x3 x5 n == True && rankCountGS == 1 
		= (changeGuess suit3 cardRank, GameState cardRank gsSuit rankCountIncrease 0)
    | isRankCorrectSuitIncorrect x3 x5 n == True && rankCountGS == 2 
		= (changeGuess suit4 cardRank, GameState cardRank gsSuit rankCountIncrease 0)
    | isRankCorrectSuitIncorrect x3 x5 n == True && rankCountGS == 3 
		= (changeGuess suit5 cardRank, GameState cardRank gsSuit rankCountIncrease 0)
    | x3 == 0 && x2 > 0 && x4 > 0 = (changeGuess cardSuit r1, GameState r1s gsSuit 0 0)
    | x3 == 0 && x2 == 2 && x4 == 0 = (changeGuess cardSuit r2, GameState r2s gsSuit 0 0)
    | x3 == 0 && x2 == 0 && x4 == n = (changeGuess cardSuit r3, GameState r3s gsSuit 0 0)
    | x3 == 0 && x2 == 0 && x4 > 0 = (changeGuess cardSuit r5, GameState r5s gsSuit 0 0)
	| x3 == 0 && x2 > 0 && x4 == 0 = (changeGuess cardSuit r4, GameState r4s gsSuit 0 0)
    | x3 == 0 && x2 == 0 && x4 == 0 = (changeGuess cardSuit r6, GameState r6s gsSuit 0 0)
	where
        n = length c
        rankCountGS = getRankCountGS g
        rankCountIncrease = 1 + rankCountGS
        suitCountGS = getSuitCountGS g
        suitCountIncrease = 1 + rankCountGS
        suit1 = gsSuit \\ cardSuit
        suit2 = onlySuit1 gsSuit
        suit3 = onlySuit2 gsSuit
        suit4 = onlySuit3 gsSuit
        suit5 = onlySuit4 gsSuit
        r1 = pickRank r1s
        r1s = guessRank gsRank cardRank 1
        r10 = selectRank gsRank
        r9 = pickRank r9s
        r9s = guessRank gsRank cardRank 9
        r8 = pickRank r8s
        r8s = guessRank gsRank cardRank 8
        r7 = pickRank r7s
        r7s = guessRank gsRank cardRank 7
        r6 = pickRank r6s
        r6s = guessRank gsRank cardRank 6
        r2 = pickRank r2s
        r2s = guessRank gsRank cardRank 2
        r3 = pickRank r3s
        r3s = guessRank gsRank cardRank 3
        r4 = pickRank r4s
        r4s = guessRank gsRank cardRank 4
        r5 = pickRank r5s
        r5s = guessRank gsRank cardRank 5
        cardSuit = toSuit c
        cardRank = toRanks c
        gsRank = getRankFromGameState g
        gsSuit = getSuitFromGameState g