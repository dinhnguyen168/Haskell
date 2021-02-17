module SimpleGrphDB 
  (  -- * Datatypes
    G
  , Node
  , nprop
  , node_elem
  , Link
  , lprop
  , link_elem
  , link_src
  , link_trg
  , Path
  , links
    -- * Graph manipulating functions
  , empty
  , add_node, add_node'
  , add_link, add_link'
  , modify_node
  , modify_link
    -- * Query functions
  , find_nodes
  , find_links
  , outgoing
  , find_paths
  )  where


-- | The type of graphs, nodes labelled with 'a' and edges labelled with 'b'
data G a b = Graph { nodes :: [a]
                   , edges :: [(a, b, a)]
                   } deriving (Eq,Show)

-- | Nodes in the graph, labelled with 'a'
data Node a    = Node { nprop :: a -- ^ The label of the node
                      } deriving (Eq,Show)

-- | Checks wether a node is contained in the graph
node_elem :: Eq a=> Node a-> G a b-> Bool
node_elem (Node a) g = a `elem` nodes g

-- | Edges in the graph, labelled with 'b'
data Link a b  = Link { src :: Node a 
                      , lprop :: b -- ^ The label of the edge
                      , trg :: Node a
                      } deriving (Eq,Show)

-- | Checks wether a link is contained in the graph
link_elem :: (Eq a, Eq b)=> Link a b-> G a b-> Bool
link_elem (Link (Node s) l (Node t)) g =
 s `elem` nodes g && t `elem` nodes g && (s, l, t) `elem` edges g

-- | Source and target node of a given link
link_src, link_trg :: Link a b-> Node a
link_src = src
link_trg = trg

-- helper function to create a Link 
link :: (a, b, a)-> Link a b
link (s, l, t) = Link (Node s) l (Node t)

-- | Paths in the graph
data Path a b  = Path { links :: [Link a b] -- ^ Select the links making up the path 
                      } deriving (Eq,Show)

-- | Create the empty graph.
empty :: G a b
empty = Graph [] []

-- | Add a node to the graph. Return the new node and the graph
add_node :: (Eq a)=> G a b-> a-> (Node a, G a b)
add_node g a 
  | a `elem` nodes g = (Node a, g)
  | otherwise        = (Node a, g { nodes= a: nodes g })

-- | Add a link between two nodes. If the source or target are not in the graph
--  return 'Nothing' and an unchanged graph, otherwise return the new link and graph.
add_link :: (Eq a, Eq b)=> G a b -> (Node a, b, Node a)-> (Maybe (Link a b), G a b)
add_link g (na@(Node a), l, nb@(Node b))
  | a `notElem` nodes g || b `notElem` nodes g = (Nothing, g)
  | (a, l, b) `elem` edges g = (Just lnk, g)
  | otherwise                = (Just lnk, g { edges = (a, l, b): edges g })
      where lnk = Link na l nb

-- | Variation of `add_note` which only returns the graph.
add_node' :: (Eq a) => G a b -> a-> G a b
add_node' g = snd . add_node g

-- | Variation of 'add_link' which only returns the graph.
add_link'  :: (Eq a, Eq b)=> G a b -> (Node a, b, Node a)-> G a b
add_link' g = snd . add_link g



-- | Modify a given node. If the node is not in the graph, return the graph
--   unchanged. 
modify_node :: (Eq a)=> G a b-> Node a-> a-> G a b
modify_node g (Node n) p 
  | n `elem` nodes g =
     g { nodes = map replNode $ nodes g
       , edges = map (\ (s,l,t) -> (replNode s, l, replNode t)) $ edges g }
  | otherwise  = g
  where replNode x = if x == n then p else x
  -- inefficient as it requires checking all links and is also
  -- problematic as multiple nodes with the same data are handled
  -- as the same node


-- | Modify a given link. If the link, its source or target, are not in the 
--   graph, return the graph unchanged.
modify_link :: (Eq a, Eq b)=> G a b-> Link a b-> b-> G a b
modify_link g (Link (Node s) l (Node t)) p
  | (s, l, t) `elem` edges g = g { edges = map (\lnk -> if lnk == (s,l,t) then (s,p,t) else lnk) $ edges g }
  | otherwise = g
     -- analogous problems to modify_node


-- Query functions.

-- | Find all nodes satisfying a given property
find_nodes :: G a b-> (a-> Bool)-> [Node a]
find_nodes g p = map Node $ filter p (nodes g)

-- | Find all links satisfying a given property
find_links :: G a b-> ((a, b, a)-> Bool) -> [Link a b]
find_links g p = map link $ filter p (edges g)

-- | Find all outgoing links from a given node.
outgoing :: (Eq a, Eq b)=>  G a b-> Node a-> [Link a b]
outgoing g (Node a) = map link $ filter (\ (a0, _, _)-> a0 == a) (edges g)

-- Find all paths up to a given length from a given node
paths_from :: (Eq a, Eq b)=> G a b-> Int-> Node a-> [[Link a b]]
paths_from g k n 
  | k < 0  = []
  | k == 0 = [[]]
  | otherwise =  [] : concatMap (\l-> map (l:) (paths_from g (k-1) (trg l)))
                                (outgoing g n)
                
-- | The general query operation: find all paths starting from a given node,
--   satisfying given predicates on links and target.
find_paths :: (Eq a, Eq b)=> G a b-> Int-> Node a-> ([b]-> Bool) -> (a-> Bool)-> [Path a b]
find_paths g k v pred_l pred_t =
   [Path ls | ls <- paths_from g k v
            , pred_l (map lprop ls)
            , pred_t (nprop (if null ls then v else trg (last ls)))
            ]
           
