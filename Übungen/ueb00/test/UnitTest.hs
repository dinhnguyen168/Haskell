module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Sum

main = defaultMain $ testGroup "0. Übungsblatt" [exercise0]

exercise0 = testGroup "Summe" $ [
  testCase "summe 3" $
     summe 3 @?= 6,
  testCase "summe 0" $ 
     summe 0 @?= 0
  ]
