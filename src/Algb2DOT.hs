-----------------------------------------------------------------------------------------
{-| Module      : Algb2DOT
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Algb2DOT(

-- =============================================================================
-- exports
-- =============================================================================
    
    algbExp2dot    


) where


-- =============================================================================
-- imports
-- =============================================================================

import LangAlgb(AlgbExp (..))

import Algb2DAG(algbExp2DAGCustom, AlgbExpDAGMemorizer)

import Algb2DOTChildNodeIDs(getChildNodeIDs)
import Algb2DOTLabels(getLabel)
import Algb2DOTColors(getColorByAlgbExp)

import Data.List (intersperse)



-- =============================================================================
-- functions
-- =============================================================================



----------------------------------------------------------------------
----------------------------------------------------------------------
algbExp2dot :: AlgbExp -> AlgbExpDAGMemorizer -> Int -> (String, String)
----------------------------------------------------------------------
----------------------------------------------------------------------

algbExp2dot ae algbExpDAGMemorizer tiID = (s1, s2)
    where
    
        
        
        s1 =
            (concat . intersperse "\n") (
            	    
                               ["ordering=out"]
                            ++ ["node [shape=box]"]
                            ++ ["node [height=0.1]"]
                            ++ ["node [width=0.2]"]            
                            ++ ["node [fontsize=10]"]
                            ++ ["node [style=filled]"]
                            ++ ["node [color=gray]"]
                            ++ ["edge [fontsize=9]"]
                            ++ ["edge [dir=back]"]

                     
                            ++ [s0] 
                
                    )
        
                    
        (s0, s2) = _algbExp2dot ae algbExpDAGMemorizer
            where
                
                ----------------------------------------------------------------------
                ----------------------------------------------------------------------
                _algbExp2dot :: AlgbExp -> AlgbExpDAGMemorizer -> (String, String)
                ----------------------------------------------------------------------
                ----------------------------------------------------------------------
                
                _algbExp2dot ae algbExpDAGMemorizer = (s1, s2)
                    where
                        
                        (_id, aes) = algbExp2DAGCustom  ae algbExpDAGMemorizer 
                  
                        l = reverse (zip [0..] aes)
                        
                        s2 = dotNodeID tiID (fst(head l))
                        
                        s1 = concat (intersperse "\n" dotNodeStrings)
                            where
                                
                                
                                dotNodeStrings = map _algbExpNode2dotNode l
                                    where
                                
                                        ----------------------------------------------------------------------
                                        ----------------------------------------------------------------------
                                        _algbExpNode2dotNode :: (Int, AlgbExp) -> String
                                        ----------------------------------------------------------------------

                                        -- TI1_b[label="TI1_b"];   TI1_b -> TI1_d;  
                                        
                                        _algbExpNode2dotNode (_id, NIL) = "" 
                                        
                                        
                                        _algbExpNode2dotNode (id, ae) = 
                                            dotNode tiID id text childNodeIDs
                                            where
                                                
                                                childNodeIDs = getChildNodeIDs ae 
                                                
                                                text = "[" ++ labelString ++ "," ++ colorString ++ "]"
                                                    where
                                                        
                                                        labelString = "label=\"" ++ (getLabel ae) ++ "\""
                                                        
                                                        colorString = "color=\"" ++ hexColor ++ "\""
                                                            where
                                                                
                                                                (_colorName, (_rgbColor, hexColor)) = getColorByAlgbExp ae
                                                        
                                                
                                                               
                                         
                                                
                                        

        
----------------------------------------------------------------------
----------------------------------------------------------------------
-- convert a node to a line of DOT-Syntax representing the node + the edges to the DAG-child-nodes
----------------------------------------------------------------------
dotNode :: Int -> Int -> String -> [Int] -> String

-- Int:     node ID of TableInfos Node
-- Int:     node ID of AlgebraExpression Node
-- String:  nodes textual representation
-- [Int]:   node IDs of DAG-child-nodes
--
-- String:  a line of DOT-Syntax representing the node + the edges to the DAG-child-nodes	
----------------------------------------------------------------------

dotNode tiNodeID aeNodeID text cs = (dotNodeID tiNodeID aeNodeID) ++ text ++ 
              (concat . intersperse ";" . 
               map (((dotNodeID tiNodeID aeNodeID) ++ " -> ") ++) . map (dotNodeID tiNodeID)) cs ++ 
              ";" 
              
                  






                  
dotNodeID :: Int -> Int -> String
dotNodeID tiID id = "TI" ++ show tiID ++ "_" ++ show id                               