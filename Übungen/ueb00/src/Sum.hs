module Sum where

-- |Die Funktion summe rechnet rekursiv die Summe aller i mit i in der Menge aller Elemente von 1 bis n.
-- Die Funktion nimmt eine ganze Zahl entgegen und gibt ebenso eine ganze Zahl aus.
-- Die Implementierung geschieht analog zur Implementierung der Fakultaetsfunktion aus der Vorlesung.
summe :: Integer -> Integer
summe n = if n <= 0 then 0 else n + summe (n - 1)

