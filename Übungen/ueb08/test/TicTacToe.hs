{-# LANGUAGE MultiParamTypeClasses #-}
module TicTacToe where

import Data.Maybe (isJust, fromJust,mapMaybe)
import qualified Data.Map as Map
import Data.Map(Map)
import Data.List(intercalate, find)

import Game

data Player = X -- Human player
            | O -- Computer player
	    deriving (Ord, Eq, Show)
	    
other :: Player -> Player
other X = O
other O = X

type Position = Int

type Board = Map Position Player

render :: Board -> String
render b =
  let show1 p = case Map.lookup p b of Just p-> show p ; Nothing-> " "
      row y = concatMap show1 [ 3*y+x | x<- [0..2]]
  in  "\n"++ intercalate "\n" (map row $ [0.. 2]) ++ "\n"

rows :: [[Position]]
rows = [ [ p | p <- [0..8], p `mod` 3 == i ] | i <- [0..2] ] 

cols :: [[Position]]
cols = [ [ p | p <- [0..8], p `div` 3 == i ] | i <- [0..2] ] 

diags :: [[Position]]
diags =  [[0, 4, 8], [2, 4, 6]]

winner :: Board-> Player-> Maybe [Position]
winner b p =
  find (all (\i-> Map.lookup i b == Just p)) $ rows ++ cols ++ diags

wins :: Board -> Player -> Bool
wins b p = isJust (winner b p)

full :: Board-> Bool
full b = and [Map.member p b | p<- [0..8] ]

moves :: Board-> [Position]
moves b = filter (\p-> Map.notMember p b) [0.. 8]

data TTT = TTT { board :: Board
               , next  :: Player
	       , depth :: Int
	       }

data Action = Action Player Position deriving Show

instance Game TTT Action where
   terminal t =
         full (board t) || wins (board t) X || wins (board t) O || depth t <= 0
   utility  t | wins (board t) X = 1
              | wins (board t) O = -1
              | otherwise = 0
   actions t  =  map (Action (next t)) (moves (board t))
   result t (Action p q) = TTT { board = Map.insert q p (board t),
                                 next  = other p,
				 depth = depth t- 1}

fromString :: String-> Board
fromString s 
  | length (concat (lines s)) /= 9  = error $ show s ++ " has wrong length."
  | otherwise =
      let c2p i = case (concat (lines s)) !! i of
                    'X' -> Just (i, X)
                    'O' -> Just (i, O)
                    _   -> Nothing
      in  Map.fromList $ mapMaybe c2p [0..8] 

select :: Board-> Int-> ([Position], Float)
select b d = let (ps, a)= minimax (TTT {board= b, next= X, depth= d})
             in  (map (\ (Action _ p)-> p) ps, a)


