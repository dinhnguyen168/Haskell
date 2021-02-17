module Beispielgraphen where

import SimpleGrphDB
-- import OptimizedGrphDB

import Planner(Location(..), Road(..),closeLocations)    
import Data.List

g1 :: G Location Road
g1 = foldr (\ (src,tgt,n,t,l) g -> add_link' g 
                                           ( head $ find_nodes g ((==src) . locationName)
                                           , Road n t l
                                           , head $ find_nodes g ((==tgt) . locationName)))
           unconnected 
           roads
    where 
        unconnected = foldr (flip add_node') empty locations
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


makeComplete :: Int -> G Int String
makeComplete n = foldl' (\g (src,tgt) -> add_link' g 
                                                  ( getNode g src
                                                  , show src ++ ":" ++ show tgt
                                                  , getNode g tgt))
                        unconnected 
                        [(x,y) | x<-[1..n], y<-[1..n]]
    where 
        unconnected = foldl' add_node' empty [1..n]
        getNode g n = head $ find_nodes g (==n)

