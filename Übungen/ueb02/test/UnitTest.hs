module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Coffee
import Bill

main = defaultMain $ testGroup "2. Übungsblatt" [exercise21, exercise22]

exercise21 = testGroup "Kalter Kaffee" $ [
  let b = order_getraenk (order_getraenk start CafeCreme L) Cappuccino M
  in testCase "zwischensumme" $
     zwischensumme b @?= 430,
  let b1 = order_gebaeck start Croissant
      b2 = order_getraenk b1 Cappuccino L
      b3 = order_getraenk b2 Cappuccino M
      b4 = order_gebaeck b3 Croissant
      b5 = order_getraenk b4 Cappuccino L
  in testCase "sonderangebot (zwei Cappuccino L, ein Cappuccino M, zwei Croissants)" $ 
     rabatt b5 @?= 30,
  let b1 = order_gebaeck start (Cookie Schokolade)
      b2 = order_gebaeck b1 (Cookie Vanille)
      b3 = order_gebaeck b2 (Cookie Schokolade)
  in testCase "sonderangebot (zwei Cookie Schoko, ein Cookie Vanille)" $ 
     rabatt b3 @?= 95
  ]

exercise22 = testGroup "Die Rechnung bitte!" $ [
  testCase "center 10" $
     center 10 "abc" @?= "   abc    \n",
  testCase "formatPosten 20" $
     formatPosten 20 "Foobaz" 1302 @?= "Foobaz      13.02 EU\n",
  testCase "rechung ..." $
     rechnung "RECHNUNG #372" "Die Firma dankt!" 20
       (Posten "Foo" 1302 (Posten "Baz" (-92) (Posten "Hugo" 0 Leer))) @?=
     "   RECHNUNG #372    \n                    \nFoo         13.02 EU\nBaz         -0.92 EU\nHugo         0.00 EU\n====================\nSumme:      12.10 EU\n                    \n  Die Firma dankt!  \n"
  ]
