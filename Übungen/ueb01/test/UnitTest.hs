module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Triangle
import Primes

main = defaultMain $
 testGroup "1. Übungsblatt" [ exercise1, exercise21, exercise22, exercise23 ]

exercise1 = testGroup "Triangle" $ [
  testCase "triangle 5" $
     triangle 5 @?= "*\n**\n***\n****\n*****\n",
  testCase "triangle2 5" $ 
     triangle2 5 @?= "    *\n   **\n  ***\n ****\n*****\n"
  ]

exercise21 = testGroup "Slow primes" $ [
    testCase "slowprime 5461" $ slowprime 5461 @?= False
  , testCase "slowprime 5399" $ slowprime 5399 @?= True
  , testCase "slowprime 1" $ slowprime 1 @?= False
  ]

exercise22 = testGroup "Fast primes" $ [
    testCase "fastprime 5461" $ fastprime 5461 @?= True
  , testCase "fastprime 5397" $ fastprime 5397 @?= False
  , testCase "fastprime 1" $ fastprime 1 @?= False
  ]

exercise23 = testGroup "Counting primes" $ [
    testCase "count_false 100" $ count_false 100 @?= 1
  , testCase "count_false 1000" $ count_false 1000 @?= 4
  ]

