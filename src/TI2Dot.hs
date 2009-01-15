-----------------------------------------------------------------------------------------
{-| Module      : Algb2Dot
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module TI2Dot (

-- =============================================================================
-- exports
-- =============================================================================
    ti2dot

) where


-- =============================================================================
-- imports
-- =============================================================================

import Core2Algb(TableInfos (..))
import LangAlgb(AttrName)

import Algb2DAG(AlgbExpDAGMemorizer)
import TI2DAG(TIDAGMemorizer, ti2DAGCustom)
import Algb2DOT(algbExp2dot)


import Data.List (intersperse)



-- =============================================================================
-- functions
-- =============================================================================





----------------------------------------------------------------------
----------------------------------------------------------------------
ti2dot :: TableInfos -> TIDAGMemorizer -> AlgbExpDAGMemorizer -> String
----------------------------------------------------------------------
----------------------------------------------------------------------

ti2dot ti tiDAGMemorizer algbExpDAGMemorizer = (concat . intersperse "\n") (
	    ["digraph G {"]
	    
	   -- ++ ["node [fontsize=10];"]
	   -- ++ ["edge [fontsize=9];"]
        
        ++ [_ti2dot ti tiDAGMemorizer algbExpDAGMemorizer] 
        ++ ["}"])
        
    
    
    
    where
    
        ----------------------------------------------------------------------
        ----------------------------------------------------------------------
        _ti2dot :: TableInfos -> TIDAGMemorizer -> AlgbExpDAGMemorizer -> String
        ----------------------------------------------------------------------
        ----------------------------------------------------------------------
        
        _ti2dot ti tiDAGMemorizer algbExpDAGMemorizer = concat (intersperse "\n" dotNodeStrings)
            where

                (_id, tis) = ti2DAGCustom ti tiDAGMemorizer
                
                dotNodeStrings = map _tiNode2dotNode (reverse (zip [0..] tis))
                    where
                
                        ----------------------------------------------------------------------
                        ----------------------------------------------------------------------
                        _tiNode2dotNode :: (Int, TableInfos) -> String
                        ----------------------------------------------------------------------

                        
                        _tiNode2dotNode (id, TI(ae, cols, subs)) = 
                            nodeString ++ "   " ++ edgesString ++ "\n" ++ algebraTreeString
                            where
                                
                                -- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                                -- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                                -- TI1[shape = record, label = "<q> q | <item1> item1 | <item2> item2 | <item3> item3"];
                                nodeString = prefixString ++ labelString ++ postfixString
                                    where
                                        
                                        prefixString = (dotNodeID id) ++ "[shape = record, "
                                        
                                        postfixString = "];"
                                        
                                        labelString = "label = " ++ "\"" ++ queryString ++ colsString ++ "\""
                                            where
                                                queryString = "<q> q" ++ (show id) ++ " | "
                                                
                                                colsString = concat (intersperse (" | ") (map f2 cols))
                                                    where
                                                
                                                        f2 :: AttrName -> String
                                                        f2 n = "<" ++ n ++ ">" ++ " " ++ n

                            
                                -- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                                -- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                                edgesString = concat (map f subs)
                                    where
                                        
                                        -- TI1:item3 -> TI2;
                                        f (attrName, TIDAGNodeID id') = 
                                            (dotNodeID id) ++ ":" ++ attrName ++ " -> " ++ (dotNodeID id') ++ ";"
                                
                                
                                
                                dotNodeID :: Int -> String
                                dotNodeID i = "TI" ++ show i


                                -- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                                -- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                                algebraTreeString = s1
                                    where
                                        
                                        (s2, s0) = algbExp2dot ae algbExpDAGMemorizer id
                                        
                                        s1 = 
                                            (concat . intersperse "\n") (
        	                                    
	                                            ["subgraph " ++ "cluster" ++ dotNodeID id ++ " {"]
                                                
                                                ++ [s2] 
                                                
                                                ++ ["}"]
                                                
                                                ++ [(dotNodeID id) ++ ":q -> " ++ s0 ++ ";"]
                                                
                                                )