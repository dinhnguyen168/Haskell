module Visualizer where

import Planner(Location(..), Road(..))    
import OptimizedGrphDB
import Text.Dot

import Control.Applicative
import Control.Monad

import System.Environment

import qualified Data.Map as M

-- | creates a .dot representation of a graph
graphToDot :: (Ord a, Ord b) 
      => (a -> [(String,String)])   -- ^ Attributes for each node prop
      -> (b -> [(String,String)])   -- ^ Attributes for each link prop
      -> G a b                      -- ^ Graph to create a .dot representation for
      -> Dot ()    
graphToDot nodeAttrFn linkAttrFn g = do
    let nodes = map nprop $ find_nodes g (const True)
    let edges = map lprop $ find_links g (const True)
    nodeTab <- sequence [ do nd <- node $ nodeAttrFn a
                             return (a,nd)
                        | a <- nodes]
    let fm = M.fromList nodeTab
    sequence_ [ edge (fm M.! src) (fm M.! (nprop $ link_trg lnk)) (linkAttrFn $ lprop lnk) 
              | src <- nodes
              , lnk <- outgoing g $ head $ find_nodes g (==src) ]
    return ()


-- | specialisation of graphToDot for (G Location Road)
locationRoadGraphToDot :: G Location Road
                       -> Dot ()    
locationRoadGraphToDot = graphToDot locAttrFn roadAttrFn
    where
        locAttrFn l = [("label", "(" ++ locationName l ++ "," ++ show (isOpen l) ++ ")")]
        roadAttrFn r = [("label", "(" ++ roadName r ++ "," ++ show (cost r) ++ "," ++ show (roadLength r) ++ ")")]

-- | writes the .dot representation of a (G Location Road) to a file
writeLocationRoadGraphToFile :: FilePath -> G Location Road -> IO ()
writeLocationRoadGraphToFile file = writeFile file . showDot . locationRoadGraphToDot 
