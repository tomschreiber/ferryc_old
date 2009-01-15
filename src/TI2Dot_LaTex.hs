-----------------------------------------------------------------------------------------
{-| Module      : Algb2Dot
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module TI2Dot_LaTex (

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

import TI2DAG(ti2PseudoDAG)
import Algb2DOT_LaTex(algbExp2dot)


import Data.List (intersperse)



-- =============================================================================
-- functions
-- =============================================================================





----------------------------------------------------------------------
----------------------------------------------------------------------
ti2dot :: TableInfos -> String
----------------------------------------------------------------------
----------------------------------------------------------------------

ti2dot ti = (concat . intersperse "\n") (
	    ["digraph G {"]
	    ++ ["d2tdocpreamble=\"" ++ d2tdocpreamble ++ "\";"] 
	   
	    ++ ["node [fontsize=10];"]
	    ++ ["edge [fontsize=9];"]
	    
       

	    ++ [_ti2dot ti] 
     ++ ["}"])
        
    
    
    
    where
    
        d2tdocpreamble = 
               "\\\n"
            ++ "\\usepackage{amsmath}\\\n"
            ++ "\\usepackage{amssymb}\\\n"
            
            ++ "\\DeclareMathOperator{\\serialize}{\\\n"
            ++ "  \\begin{tikzpicture}[baseline=0.5ex,scale=0.5,x=1mm,y=5mm,inner sep=1pt,thin]\\\n"
            ++ "    \\useasboundingbox (-2,0) rectangle (1.0,1);\\\n"
            ++ "    \\draw[-o] (0,0) -- (0,1);\\\n"
            ++ "  \\end{tikzpicture}\\\n"
            ++ "}"


            ++ "\\DeclareMathOperator{\\tableref}{ \\\n"
            ++ "  \\begin{tikzpicture}[baseline=-1.10ex,join=bevel,scale=0.15] \\\n"
            ++ "    \\useasboundingbox (-0.2,0) rectangle (2.6,1); \\\n"
            ++ "    \\draw[line width=0.1pt] (0,0) to [controls=+(90:0.5) and +(90:0.5)] (2,0); \\\n"
            ++ "    \\draw[line width=0.1pt] (0,0) .. controls +(-90:0.5) and +(-90:0.5) .. (2,0) -- (2,-0.4) .. controls +(-90:0.5) and +(-90:0.5) .. (0,-0.4) -- (0,0);  \\\n"
            ++ "    \\draw[line width=0.1pt] (0,0) .. controls +(-90:0.5) and +(-90:0.5) .. (2,0) -- (2,-0.8) .. controls +(-90:0.5) and +(-90:0.5) .. (0,-0.8) -- (0,0);  \\\n"
            ++ "    \\draw[line width=0.1pt] (0,0) .. controls +(-90:0.5) and +(-90:0.5) .. (2,0) -- (2,-1.2) .. controls +(-90:0.5) and +(-90:0.5) .. (0,-1.2) -- (0,0);  \\\n"
            ++ "    \\draw[fill=white,line width=0.1pt] 	(0.8,-1.8) rectangle (2.2,-0.9); \\\n"     
            ++ "    \\draw[line width=0.1pt] (1.27,-1.8) -- (1.27,-0.9); \\\n" 
            ++ "    \\draw[line width=0.1pt] (1.74,-1.8) -- (1.74,-0.9); \\\n"   
            ++ "  \\end{tikzpicture} \\\n"
            ++ "} \\\n"
    

           
        ----------------------------------------------------------------------
        ----------------------------------------------------------------------
        _ti2dot :: TableInfos -> String
        ----------------------------------------------------------------------
        ----------------------------------------------------------------------
        
        _ti2dot ti = concat (intersperse "\n" dotNodeStrings)
            where

                (_id, tis) = ti2PseudoDAG ti
                
                dotNodeStrings = map _tiNode2dotNode (reverse (zip [0..] tis))
                    where
                
                        ----------------------------------------------------------------------
                        ----------------------------------------------------------------------
                        _tiNode2dotNode :: (Int, TableInfos) -> String
                        ----------------------------------------------------------------------

                        
                        _tiNode2dotNode (id, TI(ae, cols, subs, _tcols)) = 
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
                                            --(dotNodeID id) ++ ":" ++ attrName ++ " -> " ++ (dotNodeID id') ++ "[style=\"-triangle 45, line width=0.7pt\"]" ++ ";"
                                            (dotNodeID id) ++ ":" ++ attrName ++ " -> " ++ (dotNodeID id') ++ "[style=\"-triangle 45\"]" ++ ";"
                                
                                
                                
                                dotNodeID :: Int -> String
                                dotNodeID i = "TI" ++ show i


                                -- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                                -- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                                algebraTreeString = s1
                                    where
                                        
                                        (s2, s0) = algbExp2dot ae id
                                        
                                        s1 = 
                                            (concat . intersperse "\n") (
        	                                    
	                                            ["subgraph " ++ "cluster" ++ dotNodeID id ++ " {"]
                                                
                                                ++ [s2] 
                                                
                                                ++ ["}"]
                                                
                                                -- ++ [(dotNodeID id) ++ ":q -> " ++ s0 ++ "[style=\"-triangle 45, line width=0.7pt\"]" ++ ";"]
                                                ++ [(dotNodeID id) ++ ":q -> " ++ s0 ++ "[style=\"-triangle 45\"]" ++ ";"]
                                                
                                                )