module Banner where

import Data.Bits((.&.),(.|.),shiftL)
import Data.List(transpose)
import Data.Char(toUpper)

import CharMap -- Importiert die Bitmaps

import Data.Maybe

-- | Eine Funktion, die eine zwei ganze Zahlen nimmt und diese in eine Liste von Bools, analog zur Binaerdarstellung, umrechnet (d.h. ein True steht fuer eine 1 in der Binaerdarstellung und ein False fuer eine 0).
-- Dabei steht das erste zu uebergebene Argument fuer die Laenge, die die Liste bzw. die Binaerdarstellung haben soll, waehrend das zweite Argument die umzuwandelnde Zahl ist.
-- Die Darstellung ist von rechts nach links zu lesen.
-- Die Funktion ist nur fuer nichtnegative Zahlen im zweiten Argument anwendbar.
-- Im Falle negativer Zahlen z im zweiten Argument gibt die Funktion die Binaerdarstellung als Boolliste der Zahl 2^len - z, wobei len dem Wert des ersten Argumentes entspricht.
decode :: Int-> Integer-> [Bool]
decode len number = map (\x -> number .&. shiftL 1 x /= 0) [0..(len-1)]

-- | Funktion, die eine Binaerdarstellung als Boolliste in eine Zahl zurueck umwandelt.
encode :: [Bool] -> Integer
encode = (foldr (\x y -> 2*y + x) 0) . (map (\y -> if y then 1 else 0))

-- | Funktion, einen Char vergroessert als 8x8-Character-String ausgibt, wobei das erste uebergebene Char dem zur Darstellung verwenden Zeichen und das zweite dem darzustellenden Zeichen entspricht.
banner1 :: Char-> Char-> [String]
banner1 replChar char = map ((map (\x -> if x then replChar else ' ')) . (decode 8)) (fromMaybe [] (lookup char CharMap.charMap))

-- | Funktion, die zu einem uebergenen Character und einem uebergenen String einen String ausgibt, der den uebergenen String in Form der Ausgabe von der Funktion "banner1" unter Anwendung auf den uebergebenen Char ausgibt.
banner :: Char-> String-> String
banner ch = unlines . (map concat) . transpose  . (map (banner1 ch)) -- Die Funktion funktioniert genauso, wie die Beschreibung auf dem Aufgabenblatt darstellt.

