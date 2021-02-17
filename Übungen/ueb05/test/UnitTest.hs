module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Bezirk 
import Statistics

main = defaultMain $ testGroup "5. Übungsblatt" [exercise1, exercise2]

exercise1 = testGroup "Übung 5.1: Kalter Kaffee 2.0" $ [
  testCase "neuerBezirk" $
     neuerBezirk "testbez1" @?= (Bezirk "testbez1" []),
  testCase "neuerUnterbezirk" $
     neuerUnterbezirk ns2 old @?= ns2,
  testCase "neueFiliale" $
     neueFiliale ns1 "Aurich" 9 99 @?= ns1,
  testCase "entferneFiliale" $
    entferneFiliale "Aurich" ns1 @?= ns0,
  testCase "entferneBezirk (1)" $
    entferneBezirk "Hannover"  ns4 @?= Just (ns3),
  testCase "entferneBezirk (2)" $
    entferneBezirk "Niedersachsen" ns4 @?= Nothing
  ]

exercise2 = testGroup "Übung 5.2: Kaffee statista" $ [
  testCase "maxUmsatz" $
    maxUmsatz ns @?= ("Oldenburg-Zentrum", 5321),
  testCase "minMitarbeiter" $
    minMitarbeiter ns @?= ("Aurich", 3),
  testCase "sumUmsatz" $
    sumUmsatz ns @?= 12236,
  testCase "maxProfit" $
    maxProfit ns @?= ("Hannover-Hbf", 864.4),
  testCase "tabelle" $
    tabelle mitarbeiter "Mitarbeiter" 10 ns @?=
       "Mitarbeiter für den Bezirk Niedersachsen\n\nBezirk/Filiale           | Mitarbeiter\nFil. Aurich              | #### 3\nBez. Oldenburg           | ######### 7\n  Fil. Oldenburg-Zentrum | ######### 7\nFil. Jever               | ######### 7\nBez. Hannover            | ########## 8\n  Fil. Hannover-Hbf      | ###### 5\n  Fil. Messe             | #### 3\nFil. Westochtersum       | ######## 6\n\nMitarbeiter gesamt: 31\n"
  ]


-- Testdaten

neueFilialen :: Bezirk-> [(String, Int, Int)]-> Bezirk
neueFilialen = foldl (\b (nm, p, u) -> neueFiliale b nm p u)

bre = neueFilialen (neuerBezirk "Bremen")
        [ ("Neustadt", 3, 2343)
        , ("Horn", 4, 5234)
        , ("Viertel", 10, 79)
        ]

han = neueFilialen (neuerBezirk "Hannover")
        [ ("Hannover-Hbf", 5, 4322)
        , ("Messe", 3, 218)
        ]

old = neueFilialen (neuerBezirk "Oldenburg")
        [ ("Oldenburg-Zentrum", 7, 5321)
        ]

ns0 = neuerBezirk "Niedersachsen"
ns1 = neueFiliale ns0 "Aurich" 3 50
ns2 = neuerUnterbezirk ns1 old
ns3 = neueFiliale ns2 "Jever" 7 2302
ns4 = neuerUnterbezirk ns3 han
ns = neueFiliale ns4 "Westochtersum" 6 23

d0  = neuerBezirk "D"
d   = neuerUnterbezirk (neueFiliale (neuerUnterbezirk d0 bre) "Berlin-Hbf" 12 3742) ns
