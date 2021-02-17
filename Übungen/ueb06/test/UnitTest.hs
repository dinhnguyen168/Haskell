module Main where

import Test.Tasty
import Test.Tasty.HUnit

import TextTree
import TextIndex

main = defaultMain $ testGroup "6. Übungsblatt" [exercise1, exercise2]

exercise1 = testGroup "Übung 6.1: Von Texten und Bäumen" $ [
  testCase "addOccurrence" $
     addOccurrence (WEntry "Hund" []) 5 @?= WEntry "Hund" [5],
  testCase "insertWord" $ 
     insertWord "Hund" 4 Empty @?= 
         WNode {rightbranch = Empty, entry = WEntry {word = "Hund", occurrences = [4]}, leftbranch = Empty},
  testCase "wordsWithLine" $
     wordsWithLine text2 @?= wordList,
  testCase "wordsListToTree" $
     wordListToTree wordList Empty @?= tTree1,
  testCase "textToTTree"$
     textToTTree text2 @?= tTree1
  ]

exercise2 = testGroup "Übung 6.2: Text-Origami" $ [
  testCase "foldTTree" $
     foldTTree (\a b c-> a+length(occurrences b)+c) 0 tTree1 @?= 6,
  testCase "sizeOfVocabulary" $
     sizeOfVocabulary (textToTTree text2) @?= 5,
  testCase "wordFrequency" $
     wordFrequency tTree1 @?= "drei 1\neins 2\nfuenf 1\nvier 1\nzwei 1\n",     
  testCase "whereDoesThisWordOccur" $ 
     whereDoesThisWordOccur (textToTTree text2) "vier" @?= [2],
  testCase "makeIndex" $ 
     makeIndex tTree1 @?= "drei[2]\neins[1,3]\nfuenf[3]\nvier[2]\nzwei[1]\n"
  ]

text2 = "eins zwei\ndrei vier\nfuenf eins"
wordList = [("eins",1),("zwei",1),("drei",2),("vier",2),("fuenf",3),("eins",3)] 
tTree1 = WNode {rightbranch = WNode {rightbranch = Empty, entry = WEntry {word = "drei", occurrences = [2]}, leftbranch = Empty}, entry = WEntry {word = "eins", occurrences = [1,3]}, leftbranch = WNode {rightbranch = WNode {rightbranch = WNode {rightbranch = Empty, entry = WEntry {word = "fuenf", occurrences = [3]}, leftbranch = Empty}, entry = WEntry {word = "vier", occurrences = [2]}, leftbranch = Empty}, entry = WEntry {word = "zwei", occurrences = [1]}, leftbranch = Empty}}