module Bill where

import Prelude hiding (repeat)

-- From Strings
repeat :: Int-> String-> String
repeat n s = if n <= 0 then ""
             else s ++ repeat (n-1) s

-- From the Shoppe
spaces :: Int-> String
spaces n = repeat n " "

formatL :: Int-> String-> String
formatL i s = spaces (i- length s) ++ s

showEuro :: Int-> String
showEuro i =
  (if i<0 then "-" else "") ++ 
  show (abs i `div` 100) ++ "."++
  show ((abs i `div` 10) `mod` 10) ++
  show (abs i `mod` 10)++ " EU"


-- Jetzt seit ihr dran!

{-
    | Funktion, die einen String in mitten eines neuen Strings einer angegebenen Laenge durch Einfuegen von Leerzeichen plaziert. Am Ende des Ausgabestrings wird ein Zeilenumbruch plaziert.
    Ist die angegebene Laenge kleiner als der angegebene String, so wird dieser String auf die passende Laenge gekuerzt. (Der Zeilenumbruch wird bei der Laengenberechnung nicht beruecksichtigt, wie auch im Beispiel auf dem Uebungszettel.)
    Dabei sorgt das Auslagern von strLen und y dafuer, dass diese Werte nicht doppelt berechnet werden muessen und die Funktion dadurch effizienter ist.
-}
center :: Int-> String-> String
center n s
    | y >= 0    = spaces y ++ s ++ spaces (n-strLen-y) ++ "\n"
    | otherwise = take n s ++ "\n"
    where
    strLen :: Int
    strLen = length s
    y :: Int
    y = (n-strLen) `div` 2

{-
    | Funktion, die einen String und einen Wert in Cent annimmt, den String dann linksbuendig sowie den Wert in Euro umgewandelt rechtsbuendig durch Einfuegen von Leerzeichen ausgibt.
    Der erste Parameter steht dabei fuer die Laenge, den der String haben soll.
    Ist die Laengenangabe kleiner als die Laenge der Ausgabe des Strings plus der Laenge der Ausgabe des Eurowertes mit Einheit, so wird die gesamte Zeile auf die angegebene Laenge zugeschnitten.
    Das Auslagern von "euro" soll eine doppelte Berechnung des Wertes vermeiden.
-}
formatPosten  :: Int-> String-> Int-> String
formatPosten n name betrag = take n (name ++ spaces (max 0 (n-length name-length euro)) ++ euro) ++ "\n"
    where
    euro :: String
    euro = showEuro betrag

{-
    Datentyp, der eine Rechnung symbolisieren soll. Eine Rechnung kann entweder leer sein oder Werte enthalten.
    Der Datentyp ist rekursiv gestaltet, sodass im spaeteren Verlauf ein rekursives Durchgehen durch alle Rechnungen ermoeglicht wird.
    Eintraege der Rechnung nennen wir hier "Posten", die einen Namen, einen ganzzahligen Wert als Preis in Cent und die darunterliegende Rechnung nehmen.
-}
data Rechnung = Leer | Posten String Int Rechnung
                deriving (Eq, Show)

{-
    | Funktion, die zu einer Rechnung alle Preise zusammenrechnet.
    Als Parameter muss dementsprechend eine Rechnung uebergeben werden.
-}
summe :: Rechnung-> Int
summe Leer                    = 0
summe (Posten name betrag re) = betrag + summe re

{-
    | Funktion, die basierend auf einer zu uebergebenden Kopf- sowie Fusszeile, einer Ausgabebreite sowie einer Rechnung, die Rechnung als formatierten String ausgibt.
-}
rechnung :: String-> String-> Int-> Rechnung-> String 
rechnung header footer w re = center w header ++ repeat w " " ++ "\n" ++ outputPosten re ++ repeat w "=" ++ "\n" ++ formatPosten w "Summe:" (summe re) ++ repeat w " " ++ "\n" ++ center w footer -- Falls die Rechnung Leer ist, resultiert dies in keinen Problemen, da formatPosten in dem Fall einen leeren String ausgibt.
    where
    outputPosten :: Rechnung -> String -- rekursives formatieren der einzelnen Eintraege der Rechnung. Dies passiert analog zu den Aufgaben auf dem letzten Uebungszettel.
    outputPosten Leer                                  = ""
    outputPosten (Posten name betrag naechsteRechnung) = formatPosten w name betrag ++ outputPosten naechsteRechnung
        
     
{-
    Hinweis: Alles, was ich hier nicht kommentiert habe, erachte ich als trivial.
    Des Weiteren moechte ich wissen, ob es irgendwelche Konventionen, sowohl auf die Dokumentation als auch auf den Code bezogen, gibt, die fuer die Abgaben eingehalten werden sollten.
-}


     
{-

Zum interaktiven Testen:

r = Posten "Foo" 1302 (Posten "Baz" (-92) (Posten "Hugo" 0 Leer))
putStrLn(rechnung "RECHNUNG #3.2" "Die Firma dankt!" 20 r)

-}
