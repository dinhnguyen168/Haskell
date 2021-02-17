{-# LANGUAGE MultiParamTypeClasses #-}
module ComputerPlayer (selectMove) where

import Board
import Game

-- Die Implementation dieser Klasse geschieht analog zur Implementierung vom TicTacToe-Spiel.

data Action = Action Player Position deriving Show

data State = State {
  board :: Board,
  next :: Player,
  depth :: Int
}

instance Game State Action where
  terminal s = full (board s) || wins (board s) X || wins (board s) O || depth s <= 0
  utility s 
    | wins (board s) X = 1
    | wins (board s) O = -1
    | otherwise = 0
  actions s = map (Action (next s)) $ moves (board s)
  result s (Action player position) = State{ board= move (board s) player position, next= otherP player, depth= (depth s) - 1 }

selectMove :: Board-> Int-> ([Position], Float)
selectMove b d = let
  (as, f) = minimax (State { board= b, next= X, depth= d}) in
    (map (\(Action _ p)-> p) as, f)
