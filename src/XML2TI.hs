-----------------------------------------------------------------------------------------
{-| Module      : Utils.Int
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module XML2TI (

-- =============================================================================
-- exports
-- =============================================================================

  importQueryPlan  

) where

-- =============================================================================
-- imports
-- =============================================================================
import LangAlgb (
        AttrName, 
        JoinComparisonKind (..), 
        AlgbExp (..), 
        AValue (..), 
        SortDirection (..), 
        AType (..),
        SortInfos,
        ProjInfos,
        JoinPredicates,
        Tuple,
        SchemaInfos,
        ATypedValue
        )


import AttributeNameTransformation (AttributeNameTransformer)

import Core2Algb (
    TableInfos (..),
    Subs 
    )

import Text.XML.HXT.Arrow
import Text.XML.HXT.DOM.XmlTree (XmlTree)
import Data.Tree.NTree.TypeDefs (NTree (..))
import Text.XML.HXT.DOM.TypeDefs (XNode (..))

import XPathUtils (
            evalXPathFromDocumentNodeCtx,
            
             
            getUniqueXPATHPrefix_forAlgebraNode,
            getUniqueXPATHPrefix_forTableInfosNode,
             
            getNodeCount,
            getNthNode,
            
            getAttributeValueFromElementNode,
            getAttributeValueFromAttributeNode,
            getElementValue,
            
            getElementChildrenFromElementNode,
            
            getIntValue
        )

import XML2Alg (importAlgebraPlan)

import Data.List (transpose)


-- =============================================================================
-- types
-- =============================================================================

type NodeStore = [(Int,TableInfos)]
type DocumentNode = XmlTree
type TableInfosNode = XmlTree
type XPath = String
type XPathPrefix = String
type CTX = (XPathPrefix, DocumentNode, NodeStore)






-- =============================================================================
-- functions
-- =============================================================================


----------------------------------------------------------------------
----------------------------------------------------------------------
importQueryPlan :: DocumentNode -> TableInfos
----------------------------------------------------------------------
----------------------------------------------------------------------
importQueryPlan documentNode = tableInfos

    where
        
        tableInfosElements =
            evalXPathFromDocumentNodeCtx documentNode
            "//table_infos"
            
        ctx = 
            ("", documentNode, []) 
        
        (_, _, tableInfosList) =
            foldl createAndStoreTableInfos ctx tableInfosElements         

        tableInfos 
            = case tableInfosList of
                (_nodeID,tableInfos_):_t -> tableInfos_
       

----------------------------------------------------------------------
----------------------------------------------------------------------
createAndStoreTableInfos ::      -- DocumentNode, NodeStore
                                CTX
                                -- Currently to be imported Node 
                            ->  TableInfosNode 
                                -- DocumentNode, NodeStore (increased by a new Algebra-Node)
                            ->  CTX
----------------------------------------------------------------------
----------------------------------------------------------------------

createAndStoreTableInfos (_, documentNode,  nodeStore) tableInfosNode = 
    ("", documentNode, (nodeID, newTableInfos):nodeStore) 
            
        where
        
            xpathPrefix = getUniqueXPATHPrefix_forTableInfosNode tableInfosNode
        
            nodeID =
                getNodeID  (xpathPrefix, documentNode,  nodeStore) tableInfosNode
                
            
            algbExp =
                getAlgbExp (xpathPrefix, documentNode,  nodeStore) tableInfosNode 
            
            cols =
                getCols (xpathPrefix, documentNode,  nodeStore) tableInfosNode                      
           
          
            subs =
                getSubs (xpathPrefix, documentNode,  nodeStore) tableInfosNode 

            
            
            
            newTableInfos =
                TI (algbExp, cols, subs)    

-- =============================================================================
-- common auxiliary-functions
-- =============================================================================



----------------------------------------------------------------------
----------------------------------------------------------------------
getAlgbExp :: CTX -> TableInfosNode -> AlgbExp 
----------------------------------------------------------------------
----------------------------------------------------------------------
getAlgbExp (xpathPrefix, documentNode, _) tableInfosNode = algbExp

    where
     
         algbExp = importAlgebraPlan (documentNode, tableInfosNode) xpathPrefix     
        -- algbExp = EMPTY_TBL  [("iter", AT_NAT), ("pos", AT_NAT), ("item", AT_INT)]     


----------------------------------------------------------------------
----------------------------------------------------------------------
getCols :: CTX -> TableInfosNode -> [AttrName] 
----------------------------------------------------------------------
----------------------------------------------------------------------
getCols (xpathPrefix, documentNode, _) tableInfosNode = cols

    where
        
        columnElements =
            evalXPathFromDocumentNodeCtx documentNode (xpathPrefix ++ "/cols/column")
            
        cols =
            map f columnElements
                where
                    
                    f columnElement = attrName
                        where
                            
                            attrName =
                                (getAttributeValueFromElementNode columnElement "name")
                           

----------------------------------------------------------------------
----------------------------------------------------------------------
                                 -- [(AttrName, TableInfos)]
getSubs :: CTX -> TableInfosNode -> Subs 
----------------------------------------------------------------------
----------------------------------------------------------------------
getSubs (xpathPrefix, documentNode, nodeStore) tableInfosNode = subs

    where
        
        subtableReferenceElements =
            evalXPathFromDocumentNodeCtx documentNode (xpathPrefix ++ "/subs/subtable_reference")
            
        subs =
            map f subtableReferenceElements
                where
                    
                    f subtableReferenceElement = (attrName, tableInfos)
                        where
                            
                            attrName =
                                (getAttributeValueFromElementNode subtableReferenceElement "column_name")
                            
                            subTableID =
                                getIntValue (getAttributeValueFromElementNode subtableReferenceElement "ref")
                                
                            tableInfos = case lookup subTableID nodeStore of
                                Just ti  -> ti
                                Nothing  -> error "mist"
        
                                     

                           
----------------------------------------------------------------------
----------------------------------------------------------------------
getNodeID :: CTX -> TableInfosNode -> Int
----------------------------------------------------------------------
----------------------------------------------------------------------
getNodeID (xpathPrefix,  documentNode, _) tableInfosNode =
    getIntValue(getAttributeValueFromAttributeNode(getNthNode(evalXPathFromDocumentNodeCtx documentNode (xpathPrefix ++ "/@id")) 0)) 
        
    
    
    
    
    
 
  