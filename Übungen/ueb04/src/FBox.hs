module FBox where

-- | Funktion, die rekursiv prueft, ob die Laenge aller Elemente der an Check zu uebergebenden Liste gleich sind.
check :: [[a]]-> Bool
check []     = True
check (x:xs) = foldr (&&) True (map (\y -> length x == length y) xs) -- Man nimmt das erste Element der Liste und gleich dies mit allen anderen ab. Zumm Schluss werden alle Vergleiche verundet.


-- | Funktion, die den uebergebenen String an den \n splittet und so eine Liste von Strings bildet.
-- Die Funktion wird sowohl in fbox als auch eq_lines gebraucht, weshalb ich sie global erstellt habe.
splitString :: String -> [String]
splitString []   = []
splitString str = ((\x -> fst x : (splitString . (drop 1) . snd) x) . (span ('\n' /=))) str
    
-- | Funktion, die die maximlae Laenge der Elemente(=Listen) einer Liste widergibt.
-- Somit wird als Eingabe eine Liste von Listen ben"otigt.
-- Die Funktion wird in fbox und in eq_length gebraucht, weshalb ich sie global angelegt habe.
maxLength :: [[a]] -> Int
maxLength = foldr (\x y -> max (length x) y) 0

-- | Funktion, die um einen String mit Zeilen gleicher Laenge eine Box eines zu uebergebenenden Characters ausgibt.
-- Sollte der uebergebende String keine Zeilen gleicher Laenge enthalten, so wird ein Fehler ausgegeben.
fbox :: Char-> String-> String
fbox char str
    | not(check k) = error "Zeilen sind nicht gleich lang!" -- Ausgabe eines Fehlers, wenn die Zeilen nicht gleich lang sind.
    | otherwise = replicate (m + 4) char ++ "\n" ++ printLines k ++ replicate (m + 4) char  ++ "\n" -- Ausgabe des Strings mit der Umrandung
    where
        k = splitString str -- Der nach '\n' gesplittete String.
        m = maxLength k -- Berechne die Laenge des Strings, indem geguckt wird, was das Maximum der Laengen aller Strings der Liste ist.
        
        -- | Funktion, die fuer die Ausgabe der Zeilen des Strings zustaendig ist.
        printLines :: [String] -> String 
        printLines  = foldr (\x -> (([char] ++ " " ++ x ++ " " ++ [char] ++ "\n") ++)) ""
        
        
-- | Funktion, die eine Liste von Listen auf eine Liste mit Elementen (Listen) der selben Laenge bringt, indem das als erste Argument uebergebene Element zu den Elementen hinzugefuegt wird, bis diese die maximale Laenge aller Elemente der anfangs uebergebenen Liste erreicht.
eq_length :: a-> [[a]]-> [[a]]
eq_length fillEl list = map (\x -> x ++ replicate (y-length x) fillEl) list where -- Auf jede Zeile wird eine Funktion angewendet, die an jeden String das uebergene Character haengt, bis dessen Laenge die maximale Laenge aller Strings der uebergebenen Liste erreicht.
    y = maxLength list -- Berechnung der maximalen Anzahl an Zeichen in einer Zeile. Vermeidung von Mehrfachberechnung durch Speichern in einer Variable.

-- | Funktion, die einen String auf die selbe Laenge bringt.
eq_lines :: String-> String
eq_lines = unlines . (eq_length ' ') . splitString -- String nach Zeilen splitten, dann eq_length anwenden und schliesslich das Ergebnis mit '\n' zu einem String zusammenbinden.