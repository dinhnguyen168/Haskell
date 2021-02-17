module Convert where

import Data.Char(chr, ord)

-- | Convert b-adic number into an integer.
-- Den Fall, dass die Zahlen in der Integer-Liste groesser als die Basis ist, muessen wir nach der Aussage von Tobias Brandt nicht betrachten.
convertFromBase :: Integer-> [Integer]-> Integer
convertFromBase b l = f l where
    f :: [Integer] -> Integer
    f list -- Nach Aufgabenstellung ist b > 1 gewaehrleistet, weshalb keine weiteren Faelle beruecksichtigt werden muessen.
        | list == [] = 0 -- liefert 0 bei leerer Liste, da es auf diese Weise einfacher ist und dieser Fall nicht in der Aufgabenstellung spezifiziert ist.
        | otherwise  = head list + b*(f (tail list)) -- z. B. ist 3+8*(2 + 8*3) = 3 + 2*(8^1) + 3*(8^2)

-- | Convert an integer into a b-adic number.
convertToBase :: Integer-> Integer-> [Integer]
convertToBase b num = f b num where
    f :: Integer -> Integer -> [Integer]
    f base number -- Nach Aufgabenstellung ist b > 1 gewaehrleistet, weshalb keine weiteren Faelle beruecksichtigt werden muessen.
        | number == 0 = [] -- Abbruchbedingung: Zaehler und damit die ganze Zahl ist 0
        | otherwise   = snd y : f base (fst y) -- Nutzung von "quotRem", "fst" und "snd" sind analog zu den Kommentaren aus dem Tutorium von Tobias Brandt
        where y = quotRem number base -- Vermeidung einer doppelten Berechnung

-- | Convert a string (one chunk of the message) to an integer
stringAsNumber :: String-> Integer
-- stringAsNumber string = convertFromBase 256 (map (toInteger . ord) string) -- Mir ist nicht bekannt, ob wir map verwenden duerfen oder nicht. Zur Verdeutlichung lasse ich diesen Term hier drin, denn die Funktion f in den meisten folgenden Implementierungen entspricht dem, was "map" tut.
stringAsNumber string = convertFromBase 256 (f string) where
    f:: String -> [Integer]
    f str -- Rekursive Funktion, die rekursiv den String in eine Liste aus Integern umwandelt. Die einzelnen Zahlen entsprechen dabei dir Werte der Characters
        | str == "" = []
        | otherwise = toInteger (ord (head str)) : f (tail str)

-- | Convert an integer back to a string (one chunk)
numberAsString :: Integer-> String
-- numberAsString number = map (chr . fromInteger) (convertToBase 256 number)
numberAsString number = f (convertToBase 256 number) where
    f :: [Integer] -> String
    f list -- Rekursive Funktion, die eine Liste von Integern in einen String/Liste von Charactern umwandelt.
        | list == [] = ""
        | otherwise  = chr (fromInteger( head list)) : f (tail list)

