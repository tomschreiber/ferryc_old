{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
-----------------------------------------------------------------------------------------
{-| Module      : Utils.Int
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module XPathUtils (

-- =============================================================================
-- exports
-- =============================================================================
    evalXPathFromDocumentNodeCtx, 
    evalXPathFromTableInfosNodeCtx,
    evalXPathFromAlgebraNodeCtx,
    
    getUniqueXPATHPrefix_forAlgebraNode,
    getUniqueXPATHPrefix_forTableInfosNode,
    
    getNodeCount,
    getNthNode,
    
    getAttributeValueFromElementNode,
    getAttributeValueFromAttributeNode,
    getElementValue,
    
    getElementChildrenFromElementNode,
    
    getIntValue
    
     
    
) where

-- =============================================================================
-- imports
-- =============================================================================

import Text.XML.HXT.Arrow
import Text.XML.HXT.DOM.XmlTree (XmlTree)
import Data.Tree.NTree.TypeDefs (NTree (..))
import Text.XML.HXT.DOM.TypeDefs (XNode (..))




-- =============================================================================
-- functions
-- =============================================================================


----------------------------------------------------------------------
----------------------------------------------------------------------
getUniqueXPATHPrefix_forAlgebraNode :: XmlTree -> String
----------------------------------------------------------------------
----------------------------------------------------------------------
getUniqueXPATHPrefix_forAlgebraNode algebraNode = 
    "/logical_query_plan/node[@id='" ++ (getAttributeValueFromElementNode algebraNode "id")   ++ "']"  


----------------------------------------------------------------------
----------------------------------------------------------------------
getUniqueXPATHPrefix_forTableInfosNode :: XmlTree -> String
----------------------------------------------------------------------
----------------------------------------------------------------------
getUniqueXPATHPrefix_forTableInfosNode tableInfosNode = 
    "/query_plan/table_infos[@id='" ++ (getAttributeValueFromElementNode tableInfosNode "id")   ++ "']"




----------------------------------------------------------------------
----------------------------------------------------------------------
evalXPathFromDocumentNodeCtx :: XmlTree -> String -> [XmlTree]
----------------------------------------------------------------------
----------------------------------------------------------------------
evalXPathFromDocumentNodeCtx documentNode xpathExpression =
    _evalXPathFromDocCtx xpathExpression documentNode
        where      

            _evalXPathFromDocCtx xpath = runLA (
                    proc xmlDocumemt -> do
                    nodeElements       <- getXPathTrees xpath   -<  xmlDocumemt
                    returnA -<  nodeElements
                    ) 
 


----------------------------------------------------------------------
----------------------------------------------------------------------
evalXPathFromAlgebraNodeCtx :: XmlTree -> XmlTree -> XmlTree -> String -> [XmlTree]
----------------------------------------------------------------------
---------------------------------------------------------------------- 
evalXPathFromAlgebraNodeCtx documentNode tableInfosNode algebraNode xpathExpression =
    evalXPathFromDocumentNodeCtx documentNode _xpathExpression
        where
        
            uniqueXPATHPrefix_forTableInfosNode =
                getUniqueXPATHPrefix_forTableInfosNode tableInfosNode   
                
            uniqueXPATHPrefix_forAlgebraNode =
                getUniqueXPATHPrefix_forAlgebraNode algebraNode
                
            _xpathExpression =  
                uniqueXPATHPrefix_forTableInfosNode ++ uniqueXPATHPrefix_forAlgebraNode ++ xpathExpression       
 
----------------------------------------------------------------------
----------------------------------------------------------------------
evalXPathFromTableInfosNodeCtx :: XmlTree -> XmlTree -> String -> [XmlTree]
----------------------------------------------------------------------
---------------------------------------------------------------------- 
evalXPathFromTableInfosNodeCtx documentNode tableInfosNode xpathExpression =
    evalXPathFromDocumentNodeCtx documentNode _xpathExpression
        where
        
            uniqueXPATHPrefix_forTableInfosNode =
                getUniqueXPATHPrefix_forTableInfosNode tableInfosNode   
            
                
            _xpathExpression =  
                uniqueXPATHPrefix_forTableInfosNode ++ xpathExpression        

----------------------------------------------------------------------
----------------------------------------------------------------------
getNodeCount :: [XmlTree] -> Int
----------------------------------------------------------------------
----------------------------------------------------------------------
getNodeCount xmlTrees = 
    length xmlTrees 
       
 
----------------------------------------------------------------------
----------------------------------------------------------------------
getNthNode :: [XmlTree] -> Int -> XmlTree
----------------------------------------------------------------------
---------------------------------------------------------------------- 
getNthNode xmlTrees n = 
    xmlTrees !! n  


----------------------------------------------------------------------
----------------------------------------------------------------------
getAttributeValueFromElementNode :: XmlTree ->String -> String
----------------------------------------------------------------------
----------------------------------------------------------------------  
getAttributeValueFromElementNode node attributeName = 
    let 
        values = _getAttributeValueFromElementNode attributeName node
    in
        case (length values) of
            0 -> []
            _ -> head values
    
   
        where 
            
            _getAttributeValueFromElementNode name = runLA (
                    proc xmlDocumemt -> do
                    v       <- getAttrValue name     -<  xmlDocumemt
                    returnA -<  v
                    )              


----------------------------------------------------------------------
----------------------------------------------------------------------
getAttributeValueFromAttributeNode :: XmlTree -> String
----------------------------------------------------------------------
----------------------------------------------------------------------  
getAttributeValueFromAttributeNode node  = 
    case node of 
        (NTree (XAttr (_infos)) [NTree (XText text) []]) -> text
        

----------------------------------------------------------------------
----------------------------------------------------------------------
getElementValue :: XmlTree -> String
----------------------------------------------------------------------
---------------------------------------------------------------------- 
getElementValue node =
    case node of
        --NTree (XTag (_infos1) [NTree (_infos2) [NTree (_infos3) []]]) [NTree (XText text) []] -> text
        NTree (XTag (_infos1) _tree1) [NTree (XText text) []] -> text
        
----------------------------------------------------------------------
----------------------------------------------------------------------
getElementChildrenFromElementNode :: XmlTree -> String -> [XmlTree]
----------------------------------------------------------------------
----------------------------------------------------------------------
getElementChildrenFromElementNode node childName = 
    _getElementChildrenFromElementNode childName node 
        where 
            
            _getElementChildrenFromElementNode name = runLA (
                    proc xmlDocumemt -> do
                    v       <- hasName name   <<< isElem    <<<  getChildren     -<  xmlDocumemt
                    returnA -<  v
                    ) 
   
   
----------------------------------------------------------------------
----------------------------------------------------------------------   
getIntValue :: String -> Int
----------------------------------------------------------------------
---------------------------------------------------------------------- 
getIntValue s = (read s)::Int


