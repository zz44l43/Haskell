module Proj2 (initialGuess, nextGuess, GameState) where
import Piece
import Data.List

data GameState = GameState {[String]}
	deriving (Eq)

allRightColor:: [String] -> Int -> Bool
allRightColor s x
  | length s == x = True
  | otherwise = False

allWrongColor:: [String] -> Int -> Bool
allWrongColor 

initialGuess :: Int -> ([String],GameState)
initialGuess  x
	| x == 3 = (["BK", "BR", "WQ"], GameState ["BK","BQ","BR","BR","BB","BB","BN","BN","BP","BP","BP","BP","BP","BP","BP","BP","WK","WQ","WR","WR","WB","WB","WN","WN","WP","WP","WP","WP","WP","WP","WP","WP"])
  | x == 4 = (["BK", "BR", "WQ", "WK"], GameState ["BK","BQ","BR","BR","BB","BB","BN","BN","BP","BP","BP","BP","BP","BP","BP","BP","WK","WQ","WR","WR","WB","WB","WN","WN","WP","WP","WP","WP","WP","WP","WP","WP"])
  | x == 5 = (["BK", "BR", "BQ", "WQ", "WK"], GameState ["BK","BQ","BR","BR","BB","BB","BN","BN","BP","BP","BP","BP","BP","BP","BP","BP","WK","WQ","WR","WR","WB","WB","WN","WN","WP","WP","WP","WP","WP","WP","WP","WP"])

nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (s, g) (x1,x2,x3)
  |length s == x3