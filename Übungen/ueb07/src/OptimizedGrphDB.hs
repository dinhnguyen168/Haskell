module OptimizedGrphDB
  ( G
  , Node
  , nprop
  , Link
  , lprop
  , link_src
  , link_trg
  , Path
  , links
  , empty
  , add_node, add_node'
  , add_link, add_link'
  , modify_node
  , modify_link
  , node_elem
  , link_elem
  , find_nodes
  , find_links
  , outgoing
  , find_paths
  ) where

import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map

-- | The type of graphs, nodes labelled with 'a' and edges labelled with 'b'
data G a b = Graph { idToNode :: Map Int (Node a),
                   -- TODO: Verwaltung der Kanten-IDs
                     idToEdge :: Map Int (Link a b), -- analog zur Implementierung der Nodes
                   -- TODO: Verwaltung von noch verfügbaren IDs
                     idsUsed :: Set Int -- Abspeichern der bereits benutzten Ids und als neue Id dient dass Maximum aller darin enthaltenen Ids plus 1. Dies ermoeglicht, im Gegensatz zum einfachen Speichern von 2 Countern (einen fuer Nodes und einen fuer Edges), dass bei einer evtl. Implementierung einer Loeschfunktion keine Probleme auftauchen. Dies stellt somit eine Verallgemeinerung dar.
                   , outgoingEdges :: Map Int (Set Int)
                   } deriving (Eq, Show)
                   

-- | Nodes in the graph, labelled with 'a'
data Node a    = Node { nodeLab :: a
                      , nodeId  :: Int
                      } deriving Show


instance Eq (Node a) where
    n1 == n2 = nodeId n1 == nodeId n2 -- Gleichheit bei gleicher Id

nprop :: Node a-> a
nprop = nodeLab

data Link a b  = Link { linkSrc :: Node a
                      , linkLab :: b
                      , linkTrg :: Node a
                      , linkId  :: Int 
                      } deriving Show

instance Eq (Link a b) where
    l1 == l2 = linkId l1 == linkId l2 -- Gleichheit bei gleicher Id

link_src :: Link a b-> Node a
link_src = linkSrc

link_trg :: Link a b-> Node a
link_trg = linkTrg

lprop :: Link a b-> b
lprop = linkLab

data Path a b  = Path { links :: [Link a b] } deriving (Eq,Show)

-- Erzeugen eines leeren Graphen.
empty :: G a b
empty = Graph Map.empty Map.empty Set.empty Map.empty

-- Fuegt einen Knoten in den Baum hinein. Selbst, wenn der Knoteninhalt gleich dem eines anderen Knotens ist, wird der Knoten als neuer Knoten hinzugefuegt.
add_node :: G a b -> a-> (Node a, G a b)
add_node g a = (newNode, g {idToNode = Map.insert newNodeId newNode (idToNode g), idsUsed = Set.insert newNodeId (idsUsed g), outgoingEdges = Map.insert newNodeId Set.empty (outgoingEdges g)})
    where
    newNodeId = ((1+) . (\x -> case x of (Just a) -> a; Nothing -> 0) . Set.lookupMax . idsUsed) g -- Erzeuge neue NodeId
    newNode = Node a newNodeId -- erzeuge neuen Node
    

add_node' :: (Ord a) => G a b -> a-> G a b
add_node' g = snd. add_node g


-- Wird ein Link hinzugefuegt, der zwei Knoten verbindet, die bereits enthalten sind, so wird dennoch ein neuer Link zwischen den zwei Knoten erstellt.
-- (Bei allgemeinen Graphen ist es moeglich, dass mehrere, evtl. gleiche, Verbindungen zwischen zwei Knoten bestehen.
--  In dieser Implementierung haetten diese Links aber verschiedene Ids, um sie unterscheiden zu koennen.)
add_link :: G a b -> (Node a, b, Node a)-> (Maybe (Link a b), G a b)
add_link g (n1@(Node c1 id1), b, n2@(Node c2 id2))
    | containsNode1 && containsNode2 = (Just lnk, g {idsUsed = newIdsUsed, idToEdge = newIdToEdge, outgoingEdges = newOutgoingEdges}) -- Beide Nodes existieren, also muss der Link eingefuegt werden.
    | otherwise = (Nothing, g)
    where
    newLinkId = ((1+) . (\x -> case x of (Just a) -> a; Nothing -> 0) . Set.lookupMax . idsUsed) g -- Erzeuge neue LinkId
    containsNode1 = Map.lookup (nodeId n1) (idToNode g) /= Nothing -- Ist der srcNode enthalten?
    containsNode2 = Map.lookup (nodeId n2) (idToNode g) /= Nothing -- Ist der trgNode enthalten?
    lnk = Link n1 b n2 newLinkId -- new Link
    
    newIdsUsed = Set.insert newLinkId (idsUsed g) -- neue Id wird abgespeichert
    newIdToEdge = Map.insert newLinkId lnk (idToEdge g) -- neuer Link wird gespeichert.
    newOutgoingEdges = Map.insertWith (\newVal oldVal -> Set.union newVal oldVal) id1 (Set.singleton newLinkId) (outgoingEdges g) -- Linkid muss auch als outgoingEdge zum Source-Knoten gespeichert werden.

add_link'  :: (Ord a, Ord b)=> G a b -> (Node a, b, Node a)-> G a b
add_link' g = snd. add_link g

-- Wert modifizieren, falls dieser im Graphen existiert. Andernfalls wird der Graph unveraendert zurueckgegeben.
modify_node :: G a b-> Node a-> a-> G a b
modify_node g node el = g {idToNode = Map.adjust (\_ -> Node el (nodeId node)) (nodeId node) (idToNode g)}

-- Wert modifizieren, falls dieser im Graphen existiert. Andernfalls wird der Graph unveraendert zurueckgegeben.
modify_link :: G a b-> Link a b-> b-> G a b
modify_link g lnk el = g {idToEdge = Map.adjust (\link -> link {linkLab = el}) (linkId lnk) (idToEdge g)}

-- Pruefen, ob ein Knoten im Graphen vorhanden ist.
node_elem :: Node a-> G a b-> Bool
node_elem node g = Map.member (nodeId node) (idToNode g)

-- Pruefen, ob eine Kante im Graphen vorhanden ist.
link_elem :: (Ord a, Ord b)=> Link a b-> G a b-> Bool
link_elem lnk g = Map.member (linkId lnk) (idToEdge g)

-- Finden aller Knoten, die die zu uebergebende Eigenschaft erfuellen.
find_nodes :: G a b-> (a-> Bool)-> [Node a]
find_nodes g func = filter (func . nodeLab) $ Map.elems (idToNode g)


-- --------------------
-- In den folgenden Funktionen tritt das Problem auf, dass, durch das Speichern von Links und damit auch des Ziel- sowie Quellknotens, die Werte in den Nodes, die gespeichert wurden

-- Rueckgabe aller 
outgoing :: G a b-> Node a-> [Link a b]
outgoing g node = case Map.lookup (nodeId node) $ outgoingEdges g of
  Just set-> map (\id-> assemble_link_from_id g id) $ Set.toList set
  Nothing-> []

--Dadurch, dass in dieser Implementierung die Links nur die KnotenID immer richtig abspeichern, muss hier jeweils das Label des Knoten noch ermittelt werden.
find_links :: G a b-> ((a, b, a)-> Bool) -> [Link a b]
find_links g f = map (\(_, (Link _ _ _ id))-> assemble_link_from_id g id) $  Map.toList . Map.filter (\(Link (Node a _) b (Node c _) _)-> f (a, b, c)) $ idToEdge g

-- Implementierung, die fast genau der Implementierung aus SimpleGrphDB entspricht. Damit auch die Reihenfolge der Ausgabeelemente von der SimpleGrphDB- und der OptimizedGrphDB- Variante dieser Funktion gleich ist, haben wir an den Anfang noch ein "reverse" hinzugefuegt.
find_paths :: G a b-> Int-> Node a-> ([b]-> Bool)-> (a-> Bool) -> [Path a b]
find_paths g maxLen start pred_link pred_node = reverse [Path ls | ls<- find_paths_from g maxLen start, pred_link (map linkLab ls), pred_node (nodeLab (if null ls then start else linkTrg (last ls)))]

-- Funktion, die genau der Funktion aus SimpleGrphDB entspricht.
find_paths_from :: G a b-> Int-> Node a-> [[Link a b]]
find_paths_from g maxLen start 
  | maxLen < 0 = []
  | maxLen == 0 = [[]]
  | otherwise = [] : concatMap (\link-> map (link:) (find_paths_from g (maxLen - 1) (linkTrg link))) (outgoing g start)

-- Hilfsfunktion, die einen Knoten anhand seiner ID aus dem Graphen herausholt.
find_node_by_id :: G a b-> Int-> Node a
find_node_by_id g id = case Map.lookup id $ idToNode g of
  Just node -> node
  Nothing   -> undefined -- Dieser Fall ist in der Anwendung nicht moeglich.

-- Hilfsfunktion, die einen Link mit korrekten Knoten anhand seiner ID erstellt. Damit ist dies eine L"osung zu dem oben dargestellten Problem. (Die Werte der Nodes eines Links werden von den abgespeicherten Knoten genommen. Dies funktioniert durch Zugriff ueber die Id.)
assemble_link_from_id :: G a b-> Int-> Link a b
assemble_link_from_id g id = case Map.lookup id $ idToEdge g of
  Just (Link n1 label n2 _)-> (Link (find_node_by_id g (nodeId n1)) label (find_node_by_id g (nodeId n2)) id)
  Nothing-> undefined
  
  
  
  
-- g2 :: G Int Int 
-- g2 = foldr (\ (src,tgt) g -> add_link' g 
                                     -- ( head $ find_nodes g (==src) 
                                     -- , src*10 + tgt
                                     -- , head $ find_nodes g (==tgt)))
        -- unconnected 
        -- connections
 -- where 
     -- unconnected = foldr (flip add_node') empty [0..9]
     -- connections = [ (0,1)
                   -- , (0,2) 
                   -- , (0,5)
                   -- , (1,0)
                   -- , (1,2)
                   -- , (1,9)
                   -- , (2,6)
                   -- , (2,8)
                   -- , (3,6)
                   -- , (3,4)
                   -- , (4,1)
                   -- , (4,9)
                   -- , (5,2)
                   -- , (5,3)
                   -- , (5,6)
                   -- , (6,7)
                   -- , (6,9)
                   -- , (7,3)
                   -- , (7,8)
                   -- , (8,4)
                   -- , (8,7)
                   -- ]