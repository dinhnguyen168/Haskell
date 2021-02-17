module Triangle where

import Prelude hiding (repeat)

-- Aus der Vorlesung
repeat :: Int-> String-> String
repeat n s = if n<= 0 then "" else s ++ repeat (n-1) s

-- | Funktion, die ein linksbuendiges Dreieck als Zeichenkette ausgibt.
-- Nimmt die Anzahl der Ebenen der Pyramide als Argument. Ist diese Zahl kleiner gleich 0, so wird ein leerer String ausgegeben.
-- Die Funktion f gibt dabei die einzelnen Zeilen der Ausgabe aus und ruft dann rekursiv die Erstellung der naechsten Zeile auf. 
triangle :: Int-> String
triangle n = f (n-1) where 
  f :: Int -> String
  f m 
    | m < 0     = "" 
    | otherwise = repeat (n-m) "*" ++ "\n" ++ f(m-1)

-- | Funktion, die ein rechtsbuendiges Dreieck als Zeichenkette ausgibt.
-- Nimmt die Anzahl der Ebenen der Pyramide als Argument. Ist diese Zahl kleiner gleich 0, so wird ein leerer String ausgegeben.
-- Die Funktion f gibt dabei die einzelnen Zeilen der Ausgabe aus und ruft dann rekursiv die Erstellung der naechsten Zeile auf. 
triangle2 :: Int-> String
triangle2 n = f (n-1) where 
  f :: Int -> String
  f m
    | m < 0     = "" 
    | otherwise = repeat m " " ++ repeat (n-m) "*" ++ "\n" ++ f(m-1)
