module Planner where

import SimpleGrphDB
-- import OptimizedGrphDB

data Location = Location { locationName :: String
                         , isOpen :: Bool
                         } deriving (Show,Eq,Ord)

data Road = Road { roadName :: String
                 , cost :: Int
                 , roadLength :: Int
                 } deriving (Show,Eq,Ord) 

-- Funktion, die alle Pfade jeweils in Form eines Tupels von Stringliste der Namen der Orte und der Kosten widergibt.
affordablePaths :: G Location Road -> Int -> String -> Int -> String -> [([String],Int)]
affordablePaths g maxLen startName maxCost targetName  = zip setOfRoadName sumOfCost
    where   setOfRoadName = map (map roadName)$ findAllRoads -- Menge der Road-Namen

            sumOfCost = map sum $ map (map cost)$ findAllRoads -- Aufsummierung der Kosten

            findAllRoads = map (map lprop) $ map links $ findPartsFromStartToEndNode -- Suchen der Liste aller Wege.

            findPartsFromStartToEndNode = find_paths  g maxLen  findNodeWithStartName pred_l pred_r -- Finde alle Pfade, die das Praedikat von maxCost und das von targetName erfuellen.
            pred_l = \x -> sum (map cost x) < maxCost       -- Praedikat von maxCost/location
            pred_r = \y -> locationName y == targetName     -- Praedikat von targetName/road

            findNodeWithStartName = head $ find_nodes g $ \x -> locationName x == startName

-- Funktion, die alle Locations bestimmt, von denen kein Pfad aus zu einer anderen Location fuehrt.
deadEndLocations :: G Location Road -> [Location]
deadEndLocations g = map nprop $ filter (\x -> length (outgoing g x) == 0) (find_nodes g $ const True) -- Ueberpruefen, ob die Anzahl an ausgehenden Kanten leer ist.

-- Schliessen einer Liste von Locations eines Graphen.
closeLocations :: G Location Road -> [Node Location] -> G Location Road
closeLocations g list = foldr modify_n g list -- Modifizieren der Nodes, die sich in der uebergebenen Liste befinden.
    where modify_n n g = modify_node g n (Location ((locationName.nprop) n) False)
    
-- Erhoehung der Kosten von Roads eines Graphen, die eine Laenge haben, die groesser als die uebergebene Laenge ist um einen zu uebergebenen Wert.
increaseCost :: G Location Road -> Int-> Int -> G Location Road
increaseCost g len amount = foldr f g $ find_links g (\(_, road, _) -> (roadLength road) > len) where
    -- Aendern der einzelnen Links und Ausgabe des daraus resultierenden Graphen.
    f :: Link Location Road -> G Location Road -> G Location Road
    f link result = modify_link result link (Road ((roadName . lprop) link) ((cost . lprop) link + amount) ((roadLength .lprop) link))
