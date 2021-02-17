module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import OptimizedGrphDB
-- import Planner
import Data.Maybe(isJust)

setOptions =
   localOption (Timeout (50^6) "5s") .
   localOption (QC.QuickCheckTests 1000)

main = defaultMain $ setOptions $ testGroup "7. Übungsblatt" [exercise7_3]

exercise7_3 = testGroup "Aufgabe 7.3" $ [
    QC.testProperty "Beispieleigenschaft 1: Nach einer Modifikation ist der Knoten immer noch im Graph" $
        \x-> for_random_node $ \n1 -> node_elem n1 (modify_node g2 n1 x), findPathsTest,findNodesTest, modifyNodeTest,modifyLinkTest,test2 -- ,test3
    ]
    
    
-- Die Weglaenge wurde auf 10 beschraenkt, damit die Ausfuehrung nicht zu lange dauert.
findPathsTest = QC.testProperty "find_paths - Die 3 Eigenschaften auf dem Uebungsblatt" prop where
    prop :: Int -> Property
    prop val = val >= -1 && val <= 10 ==> for_random_node $ \n1 -> filter not (map (isPath . links) (find_paths g2 val n1 bPred aPred)) === [] where
        -- Praedikat, welches auf eine Liste von Linkbezeichner angewendet werden soll. Hier ein Beispiel, das von value abhaengt.
        bPred :: [Int] -> Bool
        bPred x = sum x >= (length x) * val
        
        -- Praedikat, das auf das Ziel angewendet werden soll
        aPred :: Int -> Bool
        aPred x = x < val
        
        -- Funktion, die die drei Eigenschaften fuer einen Pfad ueberprueft.
        isPath :: [Link Int Int] -> Bool
        isPath list = bPred (map lprop list) && isPath' list where -- True, wenn Kanten-Praedikat erfuellt und die ersten beiden Eigenschaften sowie das Nodes-Praedikat gelten.
            -- Testen der ersten beiden Eigenschaften und das Nodes-Praedikat.
            isPath' :: [Link Int Int] -> Bool
            isPath' linkList
                | linkList == []       = True
                | length linkList == 1 = validLink fstEl && (aPred . nprop . link_trg) fstEl -- Beim letzten Element des Pfades, ergo beim Ziel, muss die aPred-Funktion erfuellt sein.
                | otherwise            = validLink fstEl && link_trg fstEl == link_src sndEl && isPath' (tail linkList)
                where
                -- Funktion, die testet, ob die Knoten und Kanten eines Links wirklich im Graphen enthalten sind.
                validLink :: Link Int Int -> Bool
                validLink lnk = (node_elem (link_src lnk) g2) && (node_elem (link_trg lnk) g2) && (link_elem lnk g2)
                
                fstEl = head linkList -- erstes Listenelement
                sndEl = head $ tail linkList -- zweites Listenelement

modifyNodeTest = QC.testProperty "modify_node - Das Aendern eines Knotens aendert keinen anderen Knoten und keine Kanten." prop where
    prop :: Int -> Property
    prop newVal = for_random_node $ \n -> show (modify_node (modify_node g2 n newVal) n (nprop n)) === show g2 -- Wenn eine Aenderung vorgenommen wird und diese danach zurueck geaendert wird, so sollte dies der Identitaet entsprechen.
    
modifyLinkTest = QC.testProperty "modify_link - Das Aendern einer Kante aendert keine andere Kante und keinen Knoten." prop where
    prop :: Int -> Property
    prop newVal = [] === (filter not $ map (\lnk -> show (modify_link (modify_link g2 lnk newVal) lnk (lprop lnk)) == show g2) (find_links g2 (\_ -> True))) -- Wenn eine Aenderung vorgenommen wird und diese danach zurueck geaendert wird, so sollte dies der Identitaet entsprechen. Weil es hier keine Funktion zur Generierung von zufaelligen Links gibt, werden hier alle getestet.
    
    
findNodesTest = QC.testProperty "find_nodes - Wird die Bedingung geprueft?" prop where
    prop :: Int -> Property
    prop val = val >= 0 && val <= 12 ==> filter not (map (func. nprop) (find_nodes g2 func)) === [] where func = (\x -> x <= val) -- Ausfuehrung am Beispiel einer von value abhaengigen Beispielfunktion.


test2 = QC.testProperty "Der Quellknoten mit dem Label 9 liefert niemals einen Pfad, außer dem leeren." prop where
  prop :: Int-> Property
  prop pathLength = pathLength < 15 && pathLength >= 0 ==> for_random_node $ \n1-> conjoin (map traverse (map (links) (find_paths g2 pathLength (head $ find_nodes g2 (\i-> i == 9)) (const True) (\i-> i == nprop n1)))) where
    traverse :: [Link Int Int]-> Bool
    traverse path
      | null path = True
      | otherwise = False




-- Erzeugt einen zufälligen Knoten des Graphen
random_node :: Gen (Node Int)
random_node = do i<- elements [0..9]; return $ head $ find_nodes g2 (i ==)

-- Erzeugt einen Testfall für einen zufälligen Knoten des Graphen
for_random_node :: Testable a=> (Node Int-> a)-> Property
for_random_node = forAll random_node 

-- Erzeugt einen Testfall für zwei zufällige Knoten des Graphen
for_random_nodes :: Testable a=> (Node Int-> Node Int-> a)-> Property
for_random_nodes prop = forAll random_node (\n-> forAll random_node (prop n))


--------------------------------------------------------------------------------
-- Bespielgraph

g2 :: G Int Int 
g2 = foldr (\ (src,tgt) g -> add_link' g 
                                        ( head $ find_nodes g (==src) 
                                        , src*10 + tgt
                                        , head $ find_nodes g (==tgt)))
           unconnected 
           connections
    where 
        unconnected = foldr (flip add_node') empty [0..9]
        connections = [ (0,1)
                      , (0,2) 
                      , (0,5)
                      , (1,0)
                      , (1,2)
                      , (1,9)
                      , (2,6)
                      , (2,8)
                      , (3,6)
                      , (3,4)
                      , (4,1)
                      , (4,9)
                      , (5,2)
                      , (5,3)
                      , (5,6)
                      , (6,7)
                      , (6,9)
                      , (7,3)
                      , (7,8)
                      , (8,4)
                      , (8,7)
                      ]
