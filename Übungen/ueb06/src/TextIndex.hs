module TextIndex where
    
import TextTree
       
instance Functor TTree where
    -- | fmap Definition von TTree analog zu den anderen Beispielen aus der Vorlesung.
    fmap func Empty                    = Empty
    fmap func (WNode right entry left) = WNode (fmap func right) (func entry) (fmap func left)

-- | Funktion, die den Baum faltet.
-- Implementierung ist analog zu den Beispielen in der Vorlesung.
foldTTree :: (b -> a -> b -> b) -> b -> TTree a -> b
foldTTree _ wentry Empty                       = wentry
foldTTree func wentry (WNode right entry left) = func (foldTTree func wentry right) entry (foldTTree func wentry left)

-- | Funktion, die die Anzahl an Knoten eines Baums berechnet.
sizeOfVocabulary :: TTree WEntry -> Int
sizeOfVocabulary Empty                    = 0 -- Wenn kein Knoten vorhanden ist, muss der Wert nicht erhoeht werden.
sizeOfVocabulary (WNode right entry left) = 1 + (sizeOfVocabulary right) + (sizeOfVocabulary left) -- Die Anzahl an Knoten entspricht 1 fuer den derzeitigen Knoten, plus die Anzahl an Knoten des rechten Teilbaums plus die Anzahl an Knoten des linken Teilbaums.

-- | Funktion, die die einzelnen Wort-Eintraege vom kleinsten zum grossten Begriff mit jeweiliger Anzahl an Vorkommen ausgibt.
wordFrequency :: TTree WEntry -> String
wordFrequency Empty                            = ""
wordFrequency (WNode rightTree entry leftTree) = wordFrequency rightTree ++ word entry ++ " " ++ show (length (occurrences entry)) ++ "\n" ++ wordFrequency leftTree 

-- | Funktion, die die Zeilen widergibt, an denen ein bestimmtest Wort in einem Baum vorkommt.
-- Daher wird das entsprechende Wort sowie ein Baum als Eingabe benoetigt.
-- Da es effizienter ist, nicht den ganzen Baum beim Suchen durchlaufen zu muessen, habe ich die Implementierung ohne fmap oder foldTTree erstellt.
whereDoesThisWordOccur :: TTree WEntry -> String -> [Int]
whereDoesThisWordOccur Empty _ = []
whereDoesThisWordOccur (WNode right entry left) wordStr
    | wordStr == word entry = occurrences entry
    | wordStr < word entry  = whereDoesThisWordOccur right wordStr
    | wordStr > word entry  = whereDoesThisWordOccur left wordStr

-- | Funktion, die ausgibt, die alle Worter im Baum vom kleinsten zum Groessten mit den jeweiligen Zeilennummern, in denen dieses Wort vorkommt, ausgibt.
makeIndex :: TTree WEntry -> String
makeIndex tree = f (wordFrequency tree) where
    -- Funktion, die einen String Wort fuer Wort durchlauft und mitHilfe der zuvor definierten Funktionen wordFrequency und whereDoesThisWordOccur den oben beschriebenen Index ausgibt.
    f :: String -> String
    f []  = []
    f str =  y ++ show (whereDoesThisWordOccur tree y) ++ "\n" ++ f (drop 1 (snd textSplit)) where -- drop 1 kommt daher, da das Leerzeichen oder das \n, falls eines davon existiert, entfernt wird.
        textSplit = span (\x -> x /= '\n') str -- Der String eingeteilt in die derzeitige Zeile und den restlichen String.
        y = takeWhile (\x -> x /= ' ') (fst textSplit) -- Das naechste Wort des Strings.
