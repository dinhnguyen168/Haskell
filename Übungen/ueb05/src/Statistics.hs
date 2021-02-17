module Statistics where

import Bezirk
import Data.Ord
import Data.List(maximumBy, minimumBy)

-- Allgemeine Hilfsfunktionen

formatL :: Int-> String-> String
formatL n str = take n (str++ spaces n)

formatR :: Int-> String-> String
formatR n str = take n (spaces (n- length str)++ str)

spaces n = replicate n ' '

-- Jetzt seit ihr dran!

-- | Funktion, die alle Filialen eines Bezirks zurueckgibt.
-- Rekursiv werden zu den jeweiligen Bezirken auch die Unterbezirke nach Filialen durchsucht.
filialen :: Bezirk -> [Filiale]
filialen b = concat (map f (unterBezirke b))
    where
    f :: Bezirkseinheit -> [Filiale] -- Bezirkeinheiten, die Bezirke enthalten, werden rekursiv durchsucht, waehrend Filialen zurueckgegeben werden.
    f (Bezirkseinheit bez) = filialen bez
    f (Filialeinheit f)    = [f]


-- | Funktion, die den maximalen Umsatz eines Bezirks sowie den Namen der dazugehoerigen Filiale zurueckgibt.
-- Daher muss der auszuwertende Bezirk uebergeben werden.
maxUmsatz :: Bezirk -> (String, Int)
maxUmsatz bez = f (filialen bez)
    where
    f :: [Filiale] -> (String, Int) -- Funktion, die aus einer Liste von Filialen die Filiale zurueckgibt, die am meisten Umsatz hat.
    f fil
        | fil == [] = ("", 0) -- Es muss der Fall abgedeckt werden, dass die Filialenliste leer ist, denn in diesem Fall wuerde der maximumBy-Befehl einen Fehler ausgeben, weshalb das Anlegen dieser Funktion f notwendig ist.
        | otherwise = ((\fi -> (filialName fi, umsatz fi)) . (maximumBy (\a b -> compare (umsatz a) (umsatz b)))) fil -- Erst wird das Maximum berechnet, danach das Ergebnis in Form eines Tupels gebracht.

-- | Funktion, die den minimalen Umsatz eines Bezirks sowie den Namen der dazugehoerigen Filiale zurueckgibt.
-- Daher muss der auszuwertende Bezirk uebergeben werden.
minUmsatz :: Bezirk -> (String, Int)
minUmsatz bez = f (filialen bez)
    where
    f :: [Filiale] -> (String, Int)
    f fil
        | fil == [] = ("", 0) -- Auch hier muss der Fehlerfall beruecksichtigt werden, weshalb das Einfuehren dieser Funktion f notwendig ist.
        | otherwise = ((\fi -> (filialName fi, umsatz fi)) . (minimumBy (\a b -> compare (umsatz a) (umsatz b)))) fil -- Erst wird das Minimum berechnet, danach das Ergebnis in Form eines Tupels gebracht.

-- | Funktion, die die maximalen Anzahl an Mitarbeitern mit dem entsprechenden Filialnamen eines Bezirks zurueckgibt.
-- Daher muss der auszuwertende Bezirk uebergeben werden.
maxMitarbeiter :: Bezirk -> (String, Int)
maxMitarbeiter bez = f (filialen bez)
    where
    f :: [Filiale] -> (String, Int)
    f fil
        | fil == [] = ("", 0) -- Auch hier muss der Fehlerfall beruecksichtigt werden, weshalb das Einfuehren dieser Funktion f notwendig ist.
        | otherwise = ((\fi -> (filialName fi, mitarbeiter fi)) . (maximumBy (\a b -> compare (mitarbeiter a) (mitarbeiter b)))) fil -- Erst wird das Maximum berechnet, danach das Ergebnis in Form eines Tupels gebracht.


-- | Funktion, die die minimale Anzahl an Mitarbeitern mit dem entsprechenden Filialnamen eines Bezirks zurueckgibt.
-- Daher muss der auszuwertende Bezirk uebergeben werden.
minMitarbeiter :: Bezirk -> (String, Int)
minMitarbeiter bez = f (filialen bez)
    where
    f :: [Filiale] -> (String, Int)
    f fil
        | fil == [] = ("", 0) -- Auch hier muss der Fehlerfall beruecksichtigt werden, weshalb das Einfuehren dieser Funktion f notwendig ist.
        | otherwise = ((\fi -> (filialName fi, mitarbeiter fi)) . (minimumBy (\a b -> compare (mitarbeiter a) (mitarbeiter b)))) fil -- Erst wird das Minimum berechnet, danach das Ergebnis in Form eines Tupels gebracht.


-- | Funktion, die den Umsatz eines Bezirks berechnen.
-- Da nur Filialen einen Umsatz erzielen koennen, reicht es, nur die Umsaetze aller Filialen aufzuaddieren.
sumUmsatz :: Bezirk -> Int
sumUmsatz bez = foldr (\fil e -> e + umsatz fil) 0 (filialen bez)

-- | Funktion, die den maximalen Profit  eines Bezirks ausgibt.
-- Die Berechnung des Profits funktioniert analog zur Angabe in der Aufgabenstellung.
maxProfit  :: Bezirk -> (String, Double)
maxProfit bez = f (filialen bez)
    where
    f :: [Filiale] -> (String, Double) -- Funktion, die analog zu den bisherigen Funktionen, den maximalen Profit aus der Menge von Filialen ausgibt.
    f fil
        | fil == [] = ("", 0)  -- Ebenso hier muss der Fehlerfall beruecksichtigt werden, weshalb das Einfuehren dieser Funktion f notwendig ist.
        | otherwise = ((\fi -> (filialName fi, profit fi)) . (maximumBy (\a b -> compare (profit a) (profit b)))) fil
        
    -- Funktion, die den Profit einer Filiale berechnen soll. 
    -- Im Falle, dass die Anzahl an Mitarbeitern gleich 0 ist, wird als Profit 0 ausgegeben.
    -- Ich habe folgende Quelle benutzt, um herauszufinden, wie man zwei Integers teilen kann und daraus eine Fliesskommazahl erhaelt: https://stackoverflow.com/questions/3275193/whats-the-right-way-to-divide-two-int-values-to-obtain-a-float
    -- Ich bin mir nicht sicher, ob wir die genutzten Quellen irgendwo angeben sollen ausser im Dokument selbst.
    profit :: Filiale -> Double
    profit f
        | mitarbeiter f == 0 = 0
        | otherwise          = fromIntegral(umsatz f) / fromIntegral(mitarbeiter f)


-- | Funktion, die zu einer Bezirkseinheit Werte in Abhaengigkeit zur uebergebenen Funktion angewandt auf alle (Unter-)Filialen/-Bezirke der Bezirkseinheit als Tupel in Form (Name der Filiale/des Bezirks in eingeruckter Form, Wert).
-- | Die Werte von Bezirken entspricht der Summe der darin enthaltenen Filialen.
statData :: (Filiale -> Int) -> Int -> Bezirkseinheit -> [(String, Int)]
statData func offsetLeft bez = f offsetLeft bez -- Fallunterscheidung notwendung, um zu gucken, ob am Anfang eine Filialeinheit oder eine Bezirkseinheit uebergeben wurde, da bei der Filialeinheit keine unterBezirke vorhanden sind.
    where
    -- Funktion, die die derzeitige Bezirkseinheit und rekursiv alle Unterbezirke ausgibt
    f :: Int-> Bezirkseinheit -> [(String, Int)]
    f num (Bezirkseinheit b) = (replicate (num*2) ' '  ++ "Bez. " ++ bezirksName b, foldr (\fil e -> e+func fil) 0 (filialen b)) : (concat . (map (f (num+1)))) (unterBezirke b)
    f num (Filialeinheit f)  = [(replicate (num*2) ' ' ++ "Fil. " ++ filialName f, func f)]
    

-- | Funktion, die die derzeitige Bezirkseinheit und rekursiv alle Unterbezirke ausgibt.
-- Der erste zu uebergebende Parameter ist die Funktion, die festlegt, wie ein Wert zu einer Filiale berechnet wird. Das zweite Argument ist der Name der Berechnungsmethode.
-- Das dritte Argument entspricht der Anzahl der maximal auszugebenden '#'-Characters. Das letzte Argument ist das Bezirk, zu dem die Tabelle berechnet werden soll.
tabelle :: (Filiale-> Int)-> String-> Int-> Bezirk-> String
tabelle kpi_select kpi_name width b = kpi_name ++ " für den Bezirk " ++ bezirksName b ++ "\n\n" ++ formatL maxWidthNames "Bezirk/Filiale" ++ " | " ++ kpi_name ++ "\n" ++ outputLines ++ "\n" ++ kpi_name ++ " gesamt: " ++ show sumOfValues ++ "\n"
    where
    calculatedData = concat (map (statData kpi_select 0) (unterBezirke b)) -- Berechne die Daten mit der Hilfsfunktion statData.
    maxWidthNames  = foldr (\x y -> max (length (fst x)) y) 14 calculatedData  -- Berechne die maximale Breite der Stringausgabe der linken Seite der Tabelle. (Weil die Einrueckungen bereits in statData eingefuegt werden, entspricht dies einfach der Berechnung der maximalen Laenge der Strings, man aus statData als ersten Tupeleintrag erhaelt. Das neutrale Element ist die Laenge des Strings "Bezirk/Filiale".
    maxValue       = foldr (\x y -> max (snd x) y) 0 calculatedData -- Berechnung des Maximums der durch die uebergebende Funktion bestimmten Werte aller Bezirke, die per statData berechnet wurden. Dies wird dann gebraucht, um die Anzahl an auszugebenden '#'-Charactern zu berechnen.
    outputLines    = foldr (\x e -> formatL maxWidthNames (fst x) ++ " | " ++ if maxValue == 0 then "" else replicate (round(fromIntegral(width*(snd x)) / fromIntegral(maxValue))) '#' ++ " " ++ show (snd x) ++ "\n" ++ e) "" calculatedData -- Diese Variable speichert die Ausgabe aller Zeilen. Damit es etwas uebersichtlicher ist, habe ich dies in einer Variable gespeichert. Der moegliche Fehlerfall, dass maxValue = 0, ist ebenso abgedeckt, wodurch kein Teilen durch 0 stattfinden kann.
    sumOfValues    = foldr (\x y -> (kpi_select x)+ y) 0 (filialen b) -- Berechnung der Summe aller durch die Funktion bestimmten Werte. Es reicht hierbei, analog zur Implementierung der Funktion sumUmsatz, die Summe ueber die Filialen zu berechnen.
