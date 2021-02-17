module Bezirk where

import Data.Maybe(mapMaybe)
import Data.List(intersect)

data Bezirk = Bezirk { bezirksName :: String
                     , unterBezirke :: [Bezirkseinheit]
                     } deriving (Show, Eq)

data Bezirkseinheit = Bezirkseinheit Bezirk
                    | Filialeinheit Filiale
                    deriving (Show, Eq)

data Filiale = Filiale { filialName ::String
                       , mitarbeiter :: Int
                       , umsatz::Int
                       } deriving (Show, Eq)

-- | Funktion, die die Namen des Bezirks sowie aller Unterbezirke als Stringliste ausgibt.
alleBezirke :: Bezirk -> [String]
alleBezirke b = (bezirksName b) : (mapMaybe f (unterBezirke b))
    where
    -- Damit der obige Term nicht zu lang wird, habe ich die Funktion f nicht als Lambda-Ausdurck hingeschrieben.
    f :: Bezirkseinheit -> Maybe String -- Herausfilterung der Unterbezirke aus dem uebergebenen Bezirk und rekursiver Aufruf der Funktion fuer alle Unterbezirke.
    f (Filialeinheit _) = Nothing
    f (Bezirkseinheit m) = Just (concat (alleBezirke m))


-- | Funktion, die die Namen aller Filialen eines zu uebergebenen Bezirks als Liste ausgibt.
alleFilialen :: Bezirk -> [String]
alleFilialen bez = map f (unterBezirke bez)
    where
    -- Damit der obige Term nicht zu lang wird, habe ich die Funktion f nicht als Lambda-Ausdurck hingeschrieben.
    f :: Bezirkseinheit -> String -- Funktion, die bei Bezirkseinheiten rekursiv guckt, ob diese Filialeinheit als Unterbezirke entahlten. Ist dies der Fall, so werden die Namen der Filialen, die zu den jeweiligen Filialeinheiten gehoeren, zurueckgegeben.
    f (Bezirkseinheit b) = concat (alleFilialen b)
    f (Filialeinheit fil)  = filialName fil

-- | Funktion, die einen neuen Bezirk eines zu uebergebenen Namens widergibt.
neuerBezirk :: String -> Bezirk
neuerBezirk name = Bezirk name []

-- | Funktion, die einen neuen Unterbezirk erstellt, falls dieser nicht schon vorhanden ist.
neuerUnterbezirk :: Bezirk -> Bezirk -> Bezirk
neuerUnterbezirk x y
    | intersect (alleBezirke y) (alleBezirke x) /= []  = x -- Falls es einen Bezirksnamen bei einer Verschmelzung beider Bezirke doppelt geben wuerde, so wird einfach der erste Bezirk zurueckgegeben.
    | otherwise = Bezirk (bezirksName x) (unterBezirke x ++ [Bezirkseinheit y]) -- Andernfalls werden beide Unterbezirke verschmolzen.

-- | Funktion, die eine neue Filiale mit einem String als Namen, einer ganzen Zahl als Anzahl der Mitarbeiter und einer weiteren ganzen Zahl als Umsatz zu einem ebenfalls zu uebergebenen Bezirk hinzufuegt.
neueFiliale :: Bezirk -> String-> Int-> Int-> Bezirk
neueFiliale b str num1 num2
    | intersect [str] (alleFilialen b) /= [] = b  -- An dieser Stelle sieht es deutlich uebersichtlicher aus, intersect zu nutzen anstelle dies zum Beispiel mit foldr zu implementieren.
    | otherwise = Bezirk (bezirksName b) (unterBezirke b ++ [Filialeinheit (Filiale str num1 num2)]) -- Falls es die Filiale noch nicht gibt, so fuege sie hinzu. (Entsprechend mit einer Filialeinheit drumgewrapt.)

-- | Funktion, die alle Filialen eines zu uebergebenen Namens eines Bezirks loescht.
entferneFiliale :: String -> Bezirk-> Bezirk
entferneFiliale str bez = Bezirk (bezirksName bez) (mapMaybe f (unterBezirke bez)) -- Eine Filiale kann sich nur in den Unterbezirken bzw. Bezirkseinheiten des Bezirks befinden, weshalb auch nur diese untersucht werden muessen.
    where
    f :: Bezirkseinheit -> Maybe Bezirkseinheit -- Funktion, die rekursiv eine Bezirkseinheit auf eine Filialeinheit mit einer Filiale des zu loeschenden Namens untersucht. Wurde eine solche gefunden, wird Nothing stattdessen ausgegeben. Ist dem nicht der Fall, wird einfach Just vom Objekt selbst ausgegeben.
    f (Bezirkseinheit b)  = Just (Bezirkseinheit (entferneFiliale str b)) -- Im Falle einer Bezirkseinheit fuehre die Funktion auf den dazugehoerigen Unterbezirk aus, aber benutze die selbe Struktur, sodass dieser Teil beim rekursiven Aufbau erhalten bleibt.
    f (Filialeinheit fil)
        | filialName fil == str = Nothing -- Falls es sich bei der Filialeinheit um die handelt, die eine zu loeschende Filiale beinhaltet, so wird ein Nothing uebergeben, wodurch dieses Objekt dann durch das mapMaybe "entfernt" wird.
        | otherwise             = Just (Filialeinheit fil) -- Ist es eine Filialeinheit mit Namen, der nicht dem zu loeschen Namen entspricht, gebe diesen einfach zurueck. (Natuerlich als Maybe-Objekt, da wir mapMaybe nutzen moechten.)

-- | Funktion, die alle Bezirke mit einem Namen, die dem uebergebenen String gleichen, eines zu uebergenen Bezirks loescht.
entferneBezirk :: String -> Bezirk -> Maybe Bezirk
entferneBezirk str bez
    | str == bezirksName bez = Nothing -- Falls der uebergene Bezirk selbst geloescht werden soll, wird ein Nothing zurueckgegeben.
    | otherwise = Just (f bez) -- Andernfalls werden die Unterbezirke untersucht.
    where
    f :: Bezirk -> Bezirk -- Diese Funktion ist, aehnlich zum Entfernen einer Filiale, fuer das Entfernen der Bezirke zustaendig. Weil ein Maybe-Objekt zurueckgegeben werden muss und die Funktionen fromMaybe oder fromJust nicht eingebunden sind, gibt es, soweit ich weiss, ausser mapMaybe keine Moeglichkeit, ein Maybe in ein normales Objekt umzuwandeln. Daher ist diese Funktion notwendig.
    f bezirk = Bezirk (bezirksName bezirk) (mapMaybe g (unterBezirke bezirk))
  
    g :: Bezirkseinheit -> Maybe Bezirkseinheit -- Die Implementierung dieser Funktion ist analog zu der Hilfsfunktion "f" aus der Implementierung der entferneFiliale-Funktion.
    g (Bezirkseinheit b)
        | bezirksName b == str = Nothing
        | otherwise            = Just(Bezirkseinheit (f b))
    g (Filialeinheit fil)      = Just (Filialeinheit fil) 
