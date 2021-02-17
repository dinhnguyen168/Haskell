module Main where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Board as B
import qualified ComputerPlayer as C
import qualified TicTacToe as T


main = defaultMain $ testGroup "8. Übungsblatt" [exercise1, exercise21, exercise22]

exercise1 = testGroup "Aufgabe 8.1" $
  let b  = B.fromString "    O  \n  X O X\nOXXOXXO\nXOOXOOX\n"
      b1 = B.fromString "    X  \n    O  \n  X OOX\nOXXOXXO\nXOOXOOX\n"
      b3 = B.fromString "   XO  \n  XOO X\nOXXOXXO\nXOOXOOX\n"
  in
  [ testCase "All moves in b" $
    B.moves b @?= [(0,2),(1,2),(2,3),(3,2),(4,4),(5,2),(6,3)]
  , testCase "Result of two moves" $
    B.move (B.move b B.X (4, 4)) B.O (5, 2) @?= b1
  , testCase "b is not full" $ B.full b3 @?= False
  , testCase "X has won in b3" $ B.wins b3 B.X @?= True
  , testCase "O has not won in b3" $ B.wins b3 B.O @?= False
  ]

exercise21 = testGroup "Aufgabe 8.2 (TicTacToe)" $
  let t0 = T.fromString "XO X   O "
      t1 = T.fromString "OX X    O"
      t2 = T.fromString "XO       "
      t3 = T.fromString "XO O     "
      t4 = T.fromString "        O"
      tx = T.fromString "         "
  in
  [ testCase "Spot a direct win" $
    T.select t0 2 @?= ([6], 1.0)
  , testCase "Spot a win in two moves" $
    T.select t1 4 @?= ([4], 1.0)
  , testCase "Spot a win in three moves" $
    T.select t2 10 @?= ([3,4,6], 1.0)
  , testCase "Avoid a loss in two moves" $
    T.select t3 10 @?= ([4, 5, 7], 0.0)
  , testCase "Avoid a loss in three moves" $
    T.select t4 10 @?= ([4], 0.0)
  , testCase "Exhaust the whole search space" $
    T.select tx 10 @?= ([0,1,2,3,4,5,6,7,8], 0.0)
  ]

exercise22 = testGroup "Aufgabe 8.2 (four.connect)" $
  [ testCase "Spot a winning move in four steps" $
    C.selectMove (B.fromString "  O  \n  X  \n  XX \n OXO ") 5 @?= ([(1,1)], 1.0)
  ]
