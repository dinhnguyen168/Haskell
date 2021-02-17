module Primes where

-- Vorgegeben: die ganzzahlige Wurzel (d.h. das  i s.t. i^2 <= x < (i+1)^2
isqrt :: Integer-> Integer
isqrt x = truncate(sqrt(fromInteger x))

-- | Fermat'scher Primzahltest
-- Nimmt als Argument eine ganze nichtnegative Zahl p an und liefert, ob 2^(p-1) kongruent 1 modulo p ist.
--
-- Korrektur: Hier wird durch 0 geilt wenn p = 0     -1
fastprime :: Integer-> Bool
fastprime p  = (2^(p-1)) `mod` p == 1



-- | Regulärer Primzahltest
-- Die Funktion nimmt eine ganze Zahl n entgegen und liefert zurueck, ob es sich bei dieser Zahl um eine Primzahl handelt.
-- Die Funktion f nimmt eine ganze Zahlen entgegen und guckt, ob n ein Teiler von der Eingabezahl p ist. Des Weiteren wird rekursiv geguckt, ob alle folgenden relevanten Zahlen Teiler von n sind.
-- Es handelt sich dann bei n um eine Primzahl, wenn es keinen Teiler gibt, der weder 1 noch n selbst ist.
slowprime :: Integer-> Bool
slowprime n
    | n <= 1    = False
    | otherwise = f 2
    where
    f ::  Integer -> Bool
    f p
        | p > isqrt n = True 
        | otherwise   = n `mod` p /= 0 && f (p+1)



-- | Zählt die Fälle, wo der Fermat-Test falsch liegt
-- Nimmt als Argument eine ganze Zahl n entgegen und prueft, bei wie vielen ganzen Zahlen von 1 bis n der Fermat'sche Primzahltest ein falsches Ergebnis liefert.
-- Die Funktion f rechnet dabei fuer einen gegebenen ganzzahligen Startwert m, ob der zugehoerige fastprime-Wert auch den zu m gehoerigen slowprime-Wert entspricht und darauf aufbauend wird eine rekursive Summe gebildet.
count_false :: Integer-> Integer
count_false n = f 1 where 
  f :: Integer -> Integer
  f m 
    | m > n     = 0
    | otherwise = 
    if fastprime m /= slowprime m then 
      1 + f (m+1)
    else 
      f (m+1)

