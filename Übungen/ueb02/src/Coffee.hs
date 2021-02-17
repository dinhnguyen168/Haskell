module Coffee where

{-
    Datentyp, der die moeglichen Getraenke aufzaehlt.
-}
data Getraenk = CafeCreme | LatteMachiato | Milchkaffee | Cappuccino | Espresso | Kakao
              deriving (Eq, Show)

{-
    Datentyp, der die moeglichen Groessen eines Getraenks enthaelt. (Nicht auf ein bestimmtes Getraenk spezifiziert.)
-}
data Groesse = S | M | L | XL
              deriving (Eq, Show)

{-
    Datentyp, der die verfuegbaren Gebaecke auflistet. Bei den Muffins und Cookies muss naeher spezifiziert werden, um welche Sorte es sich handelt.
-}
data Gebaeck = Muffin Muffin | Croissant | Cookie Cookie
              deriving (Eq, Show)

{-
    Auflistung der verfuegbaren Muffinsorten.
-}
data Muffin = Blaubeer | SaltedCaramel | CheesecakeCream | DoubleChocolate
              deriving (Eq, Show)

{-
    Auflistung der verfuegbaren Kekssorten.
-}
data Cookie = Vanille | Schokolade
              deriving (Eq, Show)

{-
    | Funktion, die den Preis eines bestimmten Getraenks einer bestimmten Groesse in Cent als ganzzahligen Wert ausgibt.
    Nimmt also ein Getraenk und eine Groesse entgegen, zu dem der Preis ausgegeben werden soll.
-}
getraenk_preis :: Getraenk-> Groesse-> Int
getraenk_preis CafeCreme M     = 190
getraenk_preis CafeCreme L     = 210
getraenk_preis LatteMachiato M = 230
getraenk_preis LatteMachiato L = 260
getraenk_preis Milchkaffee M   = 180
getraenk_preis Milchkaffee L   = 200
getraenk_preis Milchkaffee XL  = 220
getraenk_preis Cappuccino M    = 220
getraenk_preis Cappuccino L    = 250
getraenk_preis Espresso S      = 180
getraenk_preis Espresso M      = 190
getraenk_preis Espresso L      = 250
getraenk_preis Kakao M         = 290
getraenk_preis Kakao L         = 310
getraenk_preis Kakao XL        = 350
getraenk_preis _ _             = 0 -- Wenn die Kombination aus Groesse und Getraenk nicht existiert, so setzen wir den Preis auf 0.

{-
    | Funktion, die den Preis eines bestimmten Gebaecks in Cent als ganzzahligen Wert ausgibt.
    Nimmt also ein Gebaeck entgegen, zu dem der Preis ausgegeben werden soll.
-}
gebaeck_preis :: Gebaeck-> Int
gebaeck_preis (Muffin Blaubeer)        = 160
gebaeck_preis (Muffin SaltedCaramel)   = 180
gebaeck_preis (Muffin CheesecakeCream) = 170
gebaeck_preis (Muffin DoubleChocolate) = 190
gebaeck_preis Croissant                = 165
gebaeck_preis (Cookie Vanille)         = 95
gebaeck_preis (Cookie Schokolade)      = 115


{-
    Rekursiver Datentyp, der die Bestellungen enthaelt.
    Eine Bestellung kann leer sein, es kann sich dabei um eine Bestellung eines Getraenks handeln oder um die Bestellung eines Gebaecks.
    Zu einer Getraenkebestellung wird das zu bestellende Getraenk, die Groesse und, da es sich um einen rekursiven Datentyp handelt, eine bereits vorhandene Bestellung, die aber auch einer leeren Bestellung entsprechen kann, benoetigt.
    Bei einer GebaeckBestellung wird nur das dazugehoerige Gebaeck sowie die eine Bestellung benoetigt.
-}
data Bestellung =
      LeereBestellung
    | GetraenkBestellung Bestellung Getraenk Groesse
    | GebaeckBestellung Bestellung Gebaeck
    deriving (Eq, Show)

-- Bestellungen

{-
    Bestellung, die als Anfang der rekursiven Bestellungsauflisten dient.
-}
start :: Bestellung
start = LeereBestellung

{-
    | Funktion, die zu einer bestehenden Bestellung ein Getraenk einer bestimmten Groesse hinzufuegt und die daraus resultierende Gesamtbestellung ausgibt.
-}
order_getraenk :: Bestellung-> Getraenk-> Groesse-> Bestellung
order_getraenk LeereBestellung ge gr = GetraenkBestellung start ge gr -- Falls die aufbauende Bestellung eine LeereBestellung ist, baue die Bestellung auf der Startbestellung auf.
order_getraenk be ge gr              
    | getraenk_preis ge gr == 0 = be -- Die Bestellung veraendert sich nicht, wenn Kombination aus Getraenk und Groesse nicht existiert. (In diesem Fall haben wir als Erkennungsmerkmal den Preis auf 0 gesetzt, weil keine existierende Kombination einen Preis von 0 hat.)
    | otherwise                 = GetraenkBestellung be ge gr

{-
    | Funktion, die zu einer bestehenden Bestellung ein Gebaeck hinzufuegt und die daraus resultierende Gesamtbestellung ausgibt.
-}
order_gebaeck :: Bestellung-> Gebaeck-> Bestellung
order_gebaeck LeereBestellung ge = GebaeckBestellung start ge -- Falls die aufbauende Bestellung eine LeereBestellung ist, baue die Bestellung auf der Startbestellung auf.
order_gebaeck be ge              = GebaeckBestellung be ge

{-
    | Funktion, die zu einer Bestellung die resultierenden Kosten in Cent als ganzzahligen Wert ohne Beruecksichtigung von Rabaten ausgibt.
    Hierbei muss unterschieden werden, um was fuer eine Bestellung es sich handelt, da die verschiedenen Bestellungstypen eine unterschiedliche Anzahl an Parametern haben.
    Es werden alle Bestellungen der Gesamtbestellung rekursiv durchgegangen.
-}
zwischensumme :: Bestellung-> Int
zwischensumme LeereBestellung               = 0
zwischensumme (GebaeckBestellung be ge)     = gebaeck_preis ge + zwischensumme be 
zwischensumme (GetraenkBestellung be ge gr) = getraenk_preis ge gr + zwischensumme be

{-
    | Funktion, die angibt, wie oft ein angegebenes Gebaeck in einer Bestellung vorkommt.
-}
anzahl_gebaeck :: Bestellung-> Gebaeck-> Int
anzahl_gebaeck LeereBestellung _                  = 0 
anzahl_gebaeck (GebaeckBestellung be geb1) geb2 -- Falls es sich um eine Gebaeckbestellung handelt, muss geprueft werden, ob es sich dabei um das richtige Gebaeck handelt.
    | geb1 == geb2 = 1 + anzahl_gebaeck be geb2
    | otherwise    = anzahl_gebaeck be geb2
anzahl_gebaeck (GetraenkBestellung be _ _) ge     = anzahl_gebaeck be ge
    
{-
    | Funktion, die angibt, wie oft ein angegebenes Getraenk einer angegebenen Groesse in einer Bestellung vorkommt.
-}
anzahl_getraenk :: Bestellung-> Getraenk-> Groesse-> Int
anzahl_getraenk LeereBestellung _ _                  = 0 
anzahl_getraenk (GetraenkBestellung be getr1 gr1) getr2 gr2 -- Falls es sich um eine Getraenkebestellung handelt, muss geprueft werden, ob es sich dabei um das richtige Getraenk in der richtigen Groesse handelt.
    | getr1 == getr2 && gr1 == gr2 = 1 + anzahl_getraenk be getr2 gr2
    | otherwise                    = anzahl_getraenk be getr2 gr2
anzahl_getraenk (GebaeckBestellung be _) getr gr     = anzahl_getraenk be getr gr

{-
    | Funktion, die angibt, wie viele Muffins in einer Bestellung vorkommen.
-}
anzahl_muffins :: Bestellung-> Int  -- Fallunterscheidung, ob es sich bei der derzeitigen Gebaeckbestellung um die Bestellung eines Muffins handelt.
anzahl_muffins LeereBestellung                    = 0
anzahl_muffins (GebaeckBestellung be (Muffin mu)) = 1 + anzahl_muffins be
anzahl_muffins (GebaeckBestellung be _)           = anzahl_muffins be
anzahl_muffins (GetraenkBestellung be _ _)        = anzahl_muffins be

{-
    | Funktion, die den Gesamtpreis einer Bestellung unter Beruecksichtigung von Sonderangeboten in Cent als ganze Zahl berechnet. Die Definition ist analog zur Beschreibung in der Aufgabenstellung.
-}
bezahlen :: Bestellung-> Int
bezahlen be = zwischensumme be - rabatt be 

-- Sonderangebote 

{-
    | Funktion, die den Rabatt einer Bestellung ausrechnet.
    Multipliziert die einzelnen Rabatte jeweils mit der Anzahl der Vorkommen der einzelnen Sonderangebote.
-}
rabatt :: Bestellung-> Int
rabatt be = 15 * (special_happy_breakfast_l be) + 5 * (special_happy_breakfast_m be) + 115 * (special_cookies_c be) + 95 * (special_cookies_v be) + 50 * (special_kaffee_kuchen be)

{-
    | Funktion, die die Anzahl des Vorkommens des Happy-Breakfast-L-Sonderangebots einer angegebenen Bestellung berechnet.
    Die Implementierung dieser Funktion und der nachfolgenden Funktionen laesst sich fast eins zu eins aus den Hinweisen zu dieser Aufgabe entnehmen.
-}
special_happy_breakfast_l :: Bestellung-> Int
special_happy_breakfast_l be = min (anzahl_gebaeck be Croissant) (anzahl_getraenk be Cappuccino L)

{-
    | Funktion, die die Anzahl des Vorkommens des Happy-Breakfast-M-Sonderangebots einer angegebenen Bestellung berechnet.
-}
special_happy_breakfast_m :: Bestellung-> Int
special_happy_breakfast_m be = min ((anzahl_gebaeck be Croissant)-(special_happy_breakfast_l be)) (anzahl_getraenk be Cappuccino M)

{-
    | Funktion, die die Anzahl des Vorkommens des Kaffee- und Kuchen-Sonderangebots einer angegebenen Bestellung berechnet.
-}
special_kaffee_kuchen :: Bestellung-> Int
special_kaffee_kuchen be = min (anzahl_muffins be) ((anzahl_getraenk be CafeCreme L)+(anzahl_getraenk be CafeCreme M))

{-
    | Funktion, die die Anzahl des Vorkommens des 3-Schokoladen-Cookie-Sonderangebots einer angegebenen Bestellung berechnet.
-}
special_cookies_c :: Bestellung-> Int
special_cookies_c be = (anzahl_gebaeck be (Cookie Schokolade)) `div` 3

{-
    | Funktion, die die Anzahl des Vorkommens des Cookie Binge Vanille-Sonderangebots einer angegebenen Bestellung berechnet.
-}
special_cookies_v :: Bestellung-> Int
special_cookies_v be = (anzahl_gebaeck be (Cookie Vanille) + (anzahl_gebaeck be (Cookie Schokolade)) `mod` 3) `div` 3


{-

Zum interaktiven Testen:
	
b1 = order_gebaeck start (Cookie Vanille)
b2 = order_getraenk b1 Cappuccino L
b21 = order_getraenk b2 Cappuccino L
b3 = order_gebaeck b21 (Cookie Schokolade)
b4 = order_getraenk b3 CafeCreme M 
b5 = order_gebaeck b4 Croissant
b6 = order_gebaeck b5 (Cookie Vanille)
b7 = order_gebaeck b6 Croissant

-}
