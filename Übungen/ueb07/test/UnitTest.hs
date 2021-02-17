module Main where

import Test.Tasty
import Test.Tasty.HUnit

import qualified SimpleGrphDB as G1
import qualified OptimizedGrphDB as G2
import Planner
import Data.Maybe(isJust)
import qualified Data.Set as Set


main = defaultMain $ testGroup "7. Übungsblatt" [exercise7_1, exercise7_2]

exercise7_1 = testGroup "Aufgabe 7.1" $ [
  testCase "affordablePaths" $
    affordablePaths g1 10 "L1" 30 "L2" @?= [(["R01"],7),(["R02","R04"],8),(["R03","R12","R08","R04"],27)],
  testCase "deadEndLocations" $
    deadEndLocations g1 @?= [Location {locationName = "L2", isOpen = False}],  
  testCase "closeLocations - close all" $
    G1.find_nodes (closeLocations g1 $ G1.find_nodes g1 (const True)) isOpen @?= [],
  testCase "closeLocations - close none" $
    G1.find_nodes (closeLocations g1 $ G1.find_nodes g1 (const False)) isOpen @?= G1.find_nodes g1 isOpen,
  testCase "increaseCost - affordablePaths (all)" $  
    affordablePaths (increaseCost g1 0 10) 10 "L1" 30 "L2" @?= [(["R01"],17),(["R02","R04"],28)],
  testCase "increaseCost - die Namen der Straßen mit mehr als 100€ Kosten und mehr als 20km Länge auf g1 sind identisch zu den die Namen der Straßen mit mehr als 200€ Kosten und mehr als 20km Länge wenn alle Kosten um 100€ erhöht werden" $   
    map (roadName . G1.lprop) (G1.find_links g1 (\(_,r,_) -> cost r > 100 && roadLength r > 20))
        @?= map (roadName . G1.lprop) (G1.find_links (increaseCost g1 20 100) (\(_,r,_) -> cost r > 200 && roadLength r > 20))
  ]

exercise7_2 = testGroup "Aufgabe 7.2" $ [
  testCase "find_nodes - added node" $
    map G2.nprop (G2.find_nodes (G2.add_node' g2 100) (==100)) @?= [100],
  testCase "find_nodes - duplicate value" $
    map G2.nprop (G2.find_nodes (G2.add_node' g2 9) (const True)) @?= (map G2.nprop (G2.find_nodes g2 (const True))) ++ [9],
  testCase "find_nodes - nonexisting node" $
    map G2.nprop (G2.find_nodes g2 (==1000)) @?= [],
  testCase "find_links - added edge" $
    map G2.lprop (G2.find_links (G2.add_link' g2 (getNodeOfG2 2, 23, getNodeOfG2 3)) (\ (s,l,t)->s==2 && l==23 && t==3)) @?= [23],
  testCase "find_links - parallel edge" $
    map G2.lprop (G2.find_links (G2.add_link' g2 (getNodeOfG2 5, 52, getNodeOfG2 2)) (\ (s,l,t)->s==5 && t==2)) @?= [52,52],      
  testCase "find_links - nonexisting edge" $
    map G2.lprop (G2.find_links g2 (\(s,l,t)->s==0 && t==7)) @?= [],      
  testCase "modify_node - existing node has new value but same id" $
    G2.find_nodes (G2.modify_node g2 (getNodeOfG2 2) 200) (==200)  @?= G2.find_nodes g2 (==2),
  testCase "modify_node - modifying nonexisting node does nothing" $
    let (nu, _) = G2.add_node g2 200 
    in  G2.modify_node g2 nu 1000 @?= g2,
  testCase "modify_link - existing link has new value but same id" $
    G2.find_links (G2.modify_link g2 (getLinkOfG2 19) 1019) ((==1019) . snd3)
      @?= G2.find_links g2 ((==19) . snd3),
  testCase "modify_node - modifying nonexisting node does nothing" $
    let (Just nu, _) = G2.add_link g2 (getNodeOfG2 1, 10, getNodeOfG2 2)
    in G2.modify_link g2 nu 1000 @?= g2,  
  testCase "find_paths - all paths from node 5 of length at most 3 using only evenly numbered links" $
    Set.fromList (map (map G2.lprop . G2.links) (G2.find_paths g2 3 (getNodeOfG2 5) (all even) (const True))) 
      @?= Set.fromList [[],[56],[52],[52,28],[52,28,84],[52,26]]  
  ]  

--------------------------------------------------------------------------------

-- Copy from Beispielgraphen.hs
g1 :: G1.G Location Road
g1 = foldr (\ (src,tgt,n,t,l) g -> G1.add_link' g 
                                               (head $ G1.find_nodes g ((==src) . locationName)
                                               , Road n t l
                                               , head $ G1.find_nodes g ((==tgt) . locationName))) 
           unconnected 
           roads
    where 
        unconnected = foldr (flip G1.add_node') G1.empty locations
        locations = [ Location "L0" True
                    , Location "L1" True
                    , Location "L2" False
                    , Location "L3" True
                    , Location "L4" False
                    , Location "L5" False
                    , Location "L6" False
                    , Location "L7" True ]
        roads = [ ("L0","L1","R00",92,309)
                , ("L1","L2","R01",7,100)
                , ("L1","L3","R02",3,87)
                , ("L1","L6","R03",12,76)
                , ("L3","L2","R04",5,105)
                , ("L3","L4","R05",21,72)
                , ("L4","L5","R06",35,89)
                , ("L4","L6","R07",2,12)
                , ("L5","L3","R08",1,9)
                , ("L5","L7","R09",41,50)
                , ("L6","L1","R10",15,132)
                , ("L6","L4","R11",27,207)
                , ("L6","L5","R12",9,7)
                , ("L7","L1","R13",13,56)
                , ("L1","L6","R14",71,40)
                ]

-- example graph for use with OptimizedGraphQL
g2 :: G2.G Int Int 
g2 = foldr (\ (src,tgt) g -> G2.add_link' g 
                                         ( head $ G2.find_nodes g (==src)
                                         , src*10 + tgt
                                         , head $ G2.find_nodes g (==tgt)))
           unconnected 
           connections
    where 
        unconnected = foldr (flip G2.add_node') G2.empty [0..9]
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

-- returns the first node of g2 of a given value
-- throws an exception if no such node exists
getNodeOfG2 :: Int -> G2.Node Int 
getNodeOfG2 n = case G2.find_nodes g2 (==n) of
  (v:_) -> v
  _     -> error $ "g2 does not contain a node of value " ++ show n

-- returns the first link of g2 of a given value
-- throws an exception if no such node exists
getLinkOfG2 :: Int -> G2.Link Int Int 
getLinkOfG2 n = case G2.find_links g2 (\(_,x,_) -> x==n) of
  (v:_) -> v
  _     -> error $ "g2 does not contain a link of value " ++ show n

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x  
