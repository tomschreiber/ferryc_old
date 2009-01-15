-----------------------------------------------------------------------------------------
{-| Module      : Utils.Int
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module XML2Alg (

-- =============================================================================
-- exports
-- =============================================================================

  importAlgebraPlan ,
  importAlgebraPlan2  

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
        ATypedValue,
        TBLAttributeInfos,
        KeyInfos,
        FunctionType1TO1 (..),
        FunctionTypeAGGR (..)
        )


import XML2AlgUtils (
            conv_2LA_functionType1TO1,
            conv_2LA_functionTypeAGGR 
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

import AttributeNameTransformation (AttributeNameTransformer)


import Data.List (transpose)


-- =============================================================================
-- types
-- =============================================================================

type NodeStore = [(Int,AlgbExp)]
type DocumentNode = XmlTree
type TableInfosNode = XmlTree
type ElementNode = XmlTree
type XPath = String
type XPathPrefix = String
type CTX = (XPathPrefix, (DocumentNode, TableInfosNode), NodeStore)






-- =============================================================================
-- functions
-- =============================================================================


----------------------------------------------------------------------
----------------------------------------------------------------------
importAlgebraPlan2 :: DocumentNode -> AlgbExp
----------------------------------------------------------------------
----------------------------------------------------------------------
importAlgebraPlan2 documentNode = 
    
    importAlgebraPlan (documentNode, documentNode) ""




----------------------------------------------------------------------
----------------------------------------------------------------------
importAlgebraPlan :: (DocumentNode, TableInfosNode) -> XPathPrefix -> AlgbExp
----------------------------------------------------------------------
----------------------------------------------------------------------
importAlgebraPlan (documentNode, tableInfosNode) xpathPrefixForTableInfosNode = algbExp

    where
        
        algebraNodeElements =
            evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForTableInfosNode ++
            "/logical_query_plan/node")
            
        ctx = 
            (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), []) 
        
        (_, (_,_), algbExpressions) =
            foldl createAndStoreAlgOpNode ctx algebraNodeElements         

        algbExp 
            = case algbExpressions of
                (_nodeID,algExp_):_t -> algExp_
                

----------------------------------------------------------------------
----------------------------------------------------------------------
createAndStoreAlgOpNode ::      -- DocumentNode, NodeStore
                                CTX
                                -- Currently to be imported Node 
                            ->  ElementNode 
                                -- DocumentNode, NodeStore (increased by a new Algebra-Node)
                            ->  CTX
----------------------------------------------------------------------
----------------------------------------------------------------------

createAndStoreAlgOpNode (xpathPrefixForTableInfosNode, (documentNode,tableInfosNode),  nodeStore) elementNode =
            
 let
    xpathPrefixForAlgebraNode = xpathPrefixForTableInfosNode ++ (getUniqueXPATHPrefix_forAlgebraNode elementNode)           
    laOpKind = getPFLAOpKind (xpathPrefixForAlgebraNode, (documentNode,tableInfosNode),  nodeStore) elementNode
 in            
            
            case laOpKind of
                
                
             -- ********************************************************************************
             -- ********************************************************************************
                "rownum"                ->
                
                    {-
                    <content>
                       <column name="COLNAME" new="true"/>
                      (<column name="COLNAME" function="sort" position="[0..n]" direction="DIRECTION" new="false"/>)+
                      (<column name="COLNAME" function="partition" new="false"/>)?
                    </content>
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode, (documentNode,tableInfosNode),  nodeStore) elementNode
                        
                        childNode = 
                            getChildNode (xpathPrefixForAlgebraNode, (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        
                        resultAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode, (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='true']/@name" 
                    
                        sortInfos =
                            getSortInfos(xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@function='sort']"     
                            
                        partitioningAttrName =
                            getOptionalAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@function='partition']/@name"
                    
                    
                        newAlgNode =
                            ROWNUM (resultAttrName, sortInfos,  partitioningAttrName) childNode    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore)        
                        
                   
             -- ********************************************************************************
             -- ********************************************************************************
                "rowid"                ->
                
                    {-
                    <content>
                       <column name="COLNAME" new="true"/>
                    </content>
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                        
                        childNode = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        
                        resultAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='true']/@name" 
                    
                        
                        newAlgNode =
                            ROWID resultAttrName childNode    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore) 


             -- ********************************************************************************
             -- ********************************************************************************
                "rowrank"                ->
                
                    {-
                     <content>
                       <column name="COLNAME" new="true"/>
                      (<column name="COLNAME" function="sort" position="[0..n]" direction="DIRECTION" new="false"/>)+
                     </content>
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode, (documentNode,tableInfosNode),  nodeStore) elementNode
                        
                        childNode = 
                            getChildNode (xpathPrefixForAlgebraNode, (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        
                        resultAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode, (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='true']/@name" 
                    
                        sortInfos =
                            getSortInfos(xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@function='sort']"     
                        
                    
                        newAlgNode =
                            ROWRANK (resultAttrName, sortInfos) childNode    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore)        
               
                        
             
             
             -- ********************************************************************************
             -- ********************************************************************************
                "rank"                ->
                
                    {-
                     <content>
                       <column name="COLNAME" new="true"/>
                      (<column name="COLNAME" function="sort" position="[0..n]" direction="DIRECTION" new="false"/>)+
                     </content>
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode, (documentNode,tableInfosNode),  nodeStore) elementNode
                        
                        childNode = 
                            getChildNode (xpathPrefixForAlgebraNode, (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        
                        resultAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode, (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='true']/@name" 
                    
                        sortInfos =
                            getSortInfos(xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@function='sort']"     
                        
                    
                        newAlgNode =
                            RANK (resultAttrName, sortInfos) childNode    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore)        
               
             
             
             
             
             -- ********************************************************************************
             -- ********************************************************************************
                "project"                ->
                
                    {-
                    <content>
                     (<column name="COLNAME" old_name="COLNAME" new="true"/>
                    | <column name="COLNAME" new="false"/>)*
                    </content>
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                        
                        childNode = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        
                        projInfos = 
                            getProjInfos (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column"
                        
                        
                        newAlgNode =
                            PROJ projInfos childNode    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore)           
                        
             -- ********************************************************************************
             -- ********************************************************************************
                "select"                ->
                
                    {-
                     <content>
                       <column name="COLNAME" new="false"/>
                     </content>
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                        
                        childNode = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        
                        resultAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false']/@name" 
                    
                        
                        newAlgNode =
                            SEL resultAttrName childNode    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore)                             



             -- ********************************************************************************
             -- ********************************************************************************
                "pos_select"                ->
                
                    {-
                    <content>
                       <position>POSITION</position>
                      (<column name="COLNAME" function="sort" position="[0..n]" direction="DIRECTION" new="false"/>)+
                      (<column name="COLNAME" function="partition" new="false"/>)?
                    </content>
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode, (documentNode,tableInfosNode),  nodeStore) elementNode
                        
                        childNode = 
                            getChildNode (xpathPrefixForAlgebraNode, (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        
                        position = 
                            e2int (xpathPrefixForAlgebraNode, (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/position" 
                    
                        sortInfos =
                            getSortInfos(xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@function='sort']"     
                            
                        partitioningAttrName =
                            getOptionalAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@function='partition']/@name"
                    
                    
                        newAlgNode =
                            POS_SELECT (position, sortInfos,  partitioningAttrName) childNode    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore)        
               


             -- ********************************************************************************
             -- ********************************************************************************
                "cross"                ->
                
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                        
                        childNode1 = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        childNode2 = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 1
                        
                        
                        newAlgNode =
                            CROSS childNode1 childNode2    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore)                             



             -- ********************************************************************************
             -- ********************************************************************************
                "eqjoin"                ->
                
                    {-
                     <content>
                       <column name="COLNAME" new="false" position="1"/>
                       <column name="COLNAME" new="false" position="2"/>
                     </content> 
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                        
                        childNode1 = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        childNode2 = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 1
                        
                        
                        leftAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false' and @position='1']/@name" 
                        
                        rightAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false' and @position='2']/@name" 
                        
                        
                        newAlgNode =
                            EQJOIN (leftAttrName, rightAttrName) childNode1 childNode2    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore)                             



             -- ********************************************************************************
             -- ********************************************************************************
                "semijoin"                ->
                
                    {-
                     <content>
                       <column name="COLNAME" new="false" position="1"/>
                       <column name="COLNAME" new="false" position="2"/>
                     </content> 
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                        
                        childNode1 = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        childNode2 = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 1
                        
                        
                        leftAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false' and @position='1']/@name" 
                        
                        rightAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false' and @position='2']/@name" 
                        
                        
                        newAlgNode =
                            SEMIJOIN (leftAttrName, rightAttrName) childNode1 childNode2    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore)                             




             -- ********************************************************************************
             -- ********************************************************************************
                "thetajoin"                ->
                
                    {-
                     <content>
                       (<comparison kind="KIND">
                          <column name="COLNAME" new="false" position="1"/>
                          <column name="COLNAME" new="false" position="2"/>
                        </comparison>)*
                     </content>   
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                        
                        childNode1 = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        childNode2 = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 1
                        
                        
                        joinPredicates = 
                            getJoinPredicates (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                            "/content/comparison"
                        
                        
                        newAlgNode =
                            THETAJOIN joinPredicates childNode1 childNode2    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore)                             



             -- ********************************************************************************
             -- ********************************************************************************
                "union"                ->
                
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                        
                        childNode1 = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        childNode2 = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 1
                        
                        
                        newAlgNode =
                            DISJUNION childNode1 childNode2    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore)     



             -- ********************************************************************************
             -- ********************************************************************************
                "difference"                ->
                
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                        
                        childNode1 = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        childNode2 = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 1
                        
                        
                        newAlgNode =
                            DIFFERENCE childNode1 childNode2    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore)     



             -- ********************************************************************************
             -- ********************************************************************************
                "distinct"                ->
                
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                        
                        childNode1 = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        
                        
                        newAlgNode =
                            DISTINCT childNode1    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore)     



             -- ********************************************************************************
             -- ********************************************************************************
                "table"                ->
                
                    {-
                     <content>
                       (<column name="COLNAME" new="true"/>
                         (<value type="DATATYPE">VALUE</value>)+
                        </column>)+
                     </content>

                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                       
                        
                        tuples = 
                            getTuples (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                            "/content/column[@new='true']"
                        
                        schemaInfos = 
                            getSchemaInfos (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                            "/content/column[@new='true']"
                        
                        
                        newAlgNode =
                             LIT_TBL tuples schemaInfos    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore)                             


             -- ********************************************************************************
             -- ********************************************************************************
                "empty_tbl"                ->
                
                    {-
                    <content>
                      (<column name="COLNAME" new="true"/>)*
                    </content> 
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                       
                        
                        schemaInfos = 
                            getSchemaInfos2 (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                            "/content/column[@new='true']"
                        
                        
                        newAlgNode =
                            EMPTY_TBL schemaInfos    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore)                             



             -- ********************************************************************************
             -- ********************************************************************************
                "ref_tbl"                ->
                
                    {-
                     <properties>
                        <keys>
                            (<key>
                                (<column name="COLNAME" position="[0..n]"/>)+
                            </key>)+
                        </keys>
                     </properties>
                    
                     <content>
                        <table name="TABLENAME">
                            (<column name="COLNAME" tname="TCOLNAME" type="DATATYPE"/>)+    
                        </table>
                     </content>
                    
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                       
                        
                        tableName =
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/table/@name" 
                        
                        --  (external attribute name, internal attribute name, attribute type)
                        -- [(AttrName, AttrName, AType)]
                        tblAttributeInfos = 
                            getTBLAttributeInfos (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                            "/content/table/column"
                        
                        keyInfos = 
                            getKeyInfos (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                            "/properties/keys/key"
                        
                        
                        newAlgNode =
                            TABLEREF (tableName, tblAttributeInfos, keyInfos)    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore)                             




             -- ********************************************************************************
             -- ********************************************************************************
                "attach"                ->
                
                    {-
                     <content>
                       <column name="COLNAME" new="true">
                         <value type="DATATYPE">VALUE</value>
                       </column>
                     </content>    
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                       
                        childNode = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        
                        resultAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='true']/@name" 
                        
                        aTypedValue = 
                            getATypedValue (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                            "/content/column/value"
                        
                        
                        newAlgNode =
                            ATTACH (resultAttrName, aTypedValue) childNode    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore) 



             -- ********************************************************************************
             -- ********************************************************************************
                "cast"                ->
                
                    {-
                     <content>
                       <column name="COLNAME" new="true"/>
                       <column name="COLNAME" new="false"/>
                       <type name="DATATYPE"/>
                    </content>
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                       
                        childNode = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        
                        resultAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='true']/@name" 
                        
                        attrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false']/@name" 
                        
                        
                        aType = 
                            getAType (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                            "/content/type/@name"
                        
                        
                        newAlgNode =
                            CAST (resultAttrName, attrName, aType) childNode    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore) 



             -- ********************************************************************************
             -- ********************************************************************************
                "eq"                ->
                
                    {-
                     <content>
                       <column name="COLNAME" new="true"/>
                       <column name="COLNAME" new="false" position="1"/>
                       <column name="COLNAME" new="false" position="2"/>
                     </content>
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                       
                        childNode = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        
                        resultAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='true']/@name" 
                        
                        leftAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false' and @position='1']/@name" 
                            
                        rightAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false' and @position='2']/@name" 
                        
                        newAlgNode =
                            FUN_NUM_EQ (resultAttrName, (leftAttrName, rightAttrName)) childNode    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore) 




             -- ********************************************************************************
             -- ********************************************************************************
                "gt"                ->
                
                    {-
                     <content>
                       <column name="COLNAME" new="true"/>
                       <column name="COLNAME" new="false" position="1"/>
                       <column name="COLNAME" new="false" position="2"/>
                     </content>
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                       
                        childNode = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        
                        resultAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='true']/@name" 
                        
                        leftAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false' and @position='1']/@name" 
                            
                        rightAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false' and @position='2']/@name" 
                        
                        newAlgNode =
                            FUN_NUM_GT (resultAttrName, (leftAttrName, rightAttrName)) childNode    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore) 



             -- ********************************************************************************
             -- ********************************************************************************
                "fun"                ->
                
                    {-
                     <content>
                       <kind name="FUNCTION"/>
                       <column name="COLNAME" new="true"/>
                      (<column name="COLNAME" new="false" position="[0..n]"/>)*
                     </content>
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                       
                        childNode = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        
                        functionType1TO1 =
                            getFunctionType1TO1 (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/kind/@name" 
                        
                        resultAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='true']/@name" 
                        
                        
                        operandAttrNames = 
                            getAttributeNames (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false']" 
                        
                        
                        newAlgNode =
                            FUN_1TO1 (functionType1TO1, resultAttrName, operandAttrNames) childNode    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore) 



             -- ********************************************************************************
             -- ********************************************************************************
                "and"                ->
                
                    {-
                     <content>
                       <column name="COLNAME" new="true"/>
                       <column name="COLNAME" new="false" position="1"/>
                       <column name="COLNAME" new="false" position="2"/>
                     </content>
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                       
                        childNode = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        
                        resultAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='true']/@name" 
                        
                        leftAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false' and @position='1']/@name" 
                            
                        rightAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false' and @position='2']/@name" 
                        
                        newAlgNode =
                            FUN_BOOL_AND (resultAttrName, (leftAttrName, rightAttrName)) childNode    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore) 


             -- ********************************************************************************
             -- ********************************************************************************
                "or"                ->
                
                    {-
                     <content>
                       <column name="COLNAME" new="true"/>
                       <column name="COLNAME" new="false" position="1"/>
                       <column name="COLNAME" new="false" position="2"/>
                     </content>
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                       
                        childNode = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        
                        resultAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='true']/@name" 
                        
                        leftAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false' and @position='1']/@name" 
                            
                        rightAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false' and @position='2']/@name" 
                        
                        newAlgNode =
                            FUN_BOOL_OR (resultAttrName, (leftAttrName, rightAttrName)) childNode    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore) 





             -- ********************************************************************************
             -- ********************************************************************************
                "not"                ->
                
                    {-
                     <content>
                       <column name="COLNAME" new="true"/>
                       <column name="COLNAME" new="false"/>
                     </content>
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                       
                        childNode = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        
                        resultAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='true']/@name" 
                        
                        operandAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false']/@name" 
                            
                        
                        
                        newAlgNode =
                            FUN_BOOL_NOT (resultAttrName, operandAttrName) childNode    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore) 



             -- ********************************************************************************
             -- ********************************************************************************
                "avg"                ->
                    createAndStoreAGGRFunctionAlgOpNode (xpathPrefixForTableInfosNode, (documentNode,tableInfosNode),  nodeStore) elementNode xpathPrefixForAlgebraNode  laOpKind

                "max"                ->
                    createAndStoreAGGRFunctionAlgOpNode (xpathPrefixForTableInfosNode, (documentNode,tableInfosNode),  nodeStore) elementNode xpathPrefixForAlgebraNode  laOpKind
         
                "min"                ->
                    createAndStoreAGGRFunctionAlgOpNode (xpathPrefixForTableInfosNode, (documentNode,tableInfosNode),  nodeStore) elementNode xpathPrefixForAlgebraNode  laOpKind
         
                "sum"                ->
                    createAndStoreAGGRFunctionAlgOpNode (xpathPrefixForTableInfosNode, (documentNode,tableInfosNode),  nodeStore) elementNode xpathPrefixForAlgebraNode  laOpKind
         
                     
            

             -- ********************************************************************************
             -- ********************************************************************************
                "count"                ->
                
                    {-
                     <content>
                       <column name="COLNAME" new="true"/>
                      (<column name="COLNAME" function="partition" new="false"/>)?
                    </content>
                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                       
                        childNode = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        
                        resultAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='true']/@name" 
                        
                        partitioningAttrName =
                            getOptionalAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false' and @function='partition']/@name" 
                    
        
                        
                        newAlgNode =
                            FUN_AGGR_COUNT (resultAttrName, partitioningAttrName) childNode    
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore) 








             -- ********************************************************************************
             -- ********************************************************************************
                "serialize relation"                ->
                
                    {-
                     <content>
                       <column name="COLNAME" new="false" function="iter"/>
                       <column name="COLNAME" new="false" function="pos"/>
                      (<column name="COLNAME" new="false" function="item" position="[0..n]"/>)+
                     </content>

                    -}
                    
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                       
                        childNode1 = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
                        
                        childNode2 = 
                            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 1
                        
                        iterAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false' and @function='iter']/@name" 
                        
                        posAttrName = 
                            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
                            "/content/column[@new='false' and @function='pos']/@name" 
                            
                        
                        itemAttrNames =
                            getAttributeNames (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                            "/content/column[@new='false' and @function='item']"
                            
                            
                        newAlgNode =
                            SERIALIZE_REL (iterAttrName, posAttrName, itemAttrNames) childNode1 childNode2
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore) 


             -- ********************************************************************************
             -- ********************************************************************************
                "nil"                ->
             
                    {-
                     <content>
                       <column name="COLNAME" new="false" function="iter"/>
                       <column name="COLNAME" new="false" function="pos"/>
                      (<column name="COLNAME" new="false" function="item" position="[0..n]"/>)+
                     </content>
             
                    -}
             
                    let
                        nodeID =
                            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                        
                        newAlgNode =
                            NIL
                    in
                        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore) 
                       




createAndStoreAGGRFunctionAlgOpNode (xpathPrefixForTableInfosNode, (documentNode,tableInfosNode),  nodeStore) elementNode xpathPrefixForAlgebraNode  laOpKind =
    {-
     <content>
        <column name="COLNAME" new="true"/>
        <column name="COLNAME" new="false" function="item"/>
        (<column name="COLNAME" function="partition" new="false"/>)?
     </content>
    -}

    let
        nodeID =
            getNodeID (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode
                       
        childNode = 
            getChildNode (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 0
        
        
        functionTypeAGGR =
            conv_2LA_functionTypeAGGR laOpKind 
        
        resultAttrName = 
            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
            "/content/column[@new='true']/@name" 
        
        operandAttrName = 
            getAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
            "/content/column[@new='false' and @function='item']/@name" 
        
        partitioningAttrName =
            getOptionalAttributeName (xpathPrefixForAlgebraNode,  (documentNode,tableInfosNode),  nodeStore) elementNode 
            "/content/column[@new='false' and @function='partition']/@name" 
    
        
        newAlgNode =
            FUN_AGGR (functionTypeAGGR, (resultAttrName, operandAttrName), partitioningAttrName) childNode    
    
    in
        (xpathPrefixForTableInfosNode,  (documentNode, tableInfosNode), (nodeID,newAlgNode):nodeStore) 

                    






-- =============================================================================
-- common auxiliary-functions
-- =============================================================================


----------------------------------------------------------------------
----------------------------------------------------------------------
getAType :: CTX -> ElementNode -> XPath -> AType
----------------------------------------------------------------------
----------------------------------------------------------------------
getAType (xpathPrefixForAlgebraNode,  (documentNode, tableInfosNode), _)  elementNode xpath = aType
    where
        typeString = getAttributeValueFromAttributeNode (getNthNode (evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForAlgebraNode ++ xpath)) 0)
        aType = (read typeString)::AType

----------------------------------------------------------------------
----------------------------------------------------------------------
                                             -- (AType, AValue)
getATypedValue :: CTX -> ElementNode -> XPath -> ATypedValue
----------------------------------------------------------------------
----------------------------------------------------------------------
getATypedValue (xpathPrefixForAlgebraNode,  (documentNode, tableInfosNode), _)  elementNode xpath = aTypedValue 
-- <value type="DATATYPE">VALUE</value>

    where
        
        aTypedValue = (aType, aValue)
        
        valueElement = 
            getNthNode (evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForAlgebraNode ++ xpath)) 0
        
        typeString = getAttributeValueFromElementNode valueElement "type"
        valueString =  getElementValue  valueElement
            
        aType = (read typeString)::AType
        
        
            
        aValue = case typeString of
            "int"   ->        AV_INT ((read valueString)::Integer)   
            "str"   ->        AV_STR valueString    
            "bool"  ->        AV_BLN (case valueString of {"true" -> True; "false" -> False; "True" -> True; "False" -> False})    
            "dbl"   ->        AV_DBL ((read valueString)::Double)    
            "dec"   ->        AV_DEC ((read valueString)::Float)    
            "nat"   ->        AV_NAT ((read valueString)::Integer)   
                                            

----------------------------------------------------------------------
----------------------------------------------------------------------
                                             -- (AType, AValue)
e2int :: CTX -> ElementNode -> XPath -> Int
----------------------------------------------------------------------
----------------------------------------------------------------------
e2int (xpathPrefixForAlgebraNode,  (documentNode, tableInfosNode), _)  elementNode xpath = intValue


    where
        
        valueElement = 
            getNthNode (evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForAlgebraNode ++ xpath)) 0
        
        --test1 = error(show valueElement)
        
        valueString =  getElementValue valueElement
            
        intValue = (read valueString)::Int
        
       
                                            


----------------------------------------------------------------------
----------------------------------------------------------------------
                                              -- [(AttrName, AType)]
getSchemaInfos :: CTX -> ElementNode -> XPath -> SchemaInfos
----------------------------------------------------------------------
----------------------------------------------------------------------
getSchemaInfos (xpathPrefixForAlgebraNode,  (documentNode, tableInfosNode), _)  elementNode xpath = schemaInfos
-- (<column name="COLNAME" new="true"/>
--     (<value type="DATATYPE">VALUE</value>)+
--  </column>)+

    where
        
        
        columnElements = 
            evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForAlgebraNode ++ xpath)
        
        schemaInfos = 
            map f columnElements
                where
                    --f columnElement = ("a", AT_NONE)
                    f columnElement = (attrName, attrType)
                        where
                            attrName =
                                (getAttributeValueFromElementNode columnElement "name")    
                            
                            
                            valueElementsXML = 
                                getElementChildrenFromElementNode columnElement "value"
                                
                            attrType = 
                                                let
                                                    typeString = getAttributeValueFromElementNode (head valueElementsXML) "type"
                                                in
                                                    read typeString 


----------------------------------------------------------------------
----------------------------------------------------------------------
                                              -- [(AttrName, AType)]
getSchemaInfos2 :: CTX -> ElementNode -> XPath -> SchemaInfos
----------------------------------------------------------------------
----------------------------------------------------------------------
getSchemaInfos2 (xpathPrefixForAlgebraNode,  (documentNode, tableInfosNode), _)  elementNode xpath = schemaInfos
-- (<column name="COLNAME" type="DATATYPE" new="true"/>)*    


    where
        
        
        columnElements = 
            evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForAlgebraNode ++ xpath)
        
        schemaInfos = 
            map f columnElements
                where
                    --f columnElement = ("a", AT_NONE)
                    f columnElement = (attrName, attrType)
                        where
                            attrName =
                                (getAttributeValueFromElementNode columnElement "name")    
                            
                            attrType = 
                                    let
                                        typeString = (getAttributeValueFromElementNode columnElement "type")
                                    in
                                        read typeString 
                         

----------------------------------------------------------------------
----------------------------------------------------------------------
                                                    --  (external attribute name, internal attribute name, attribute type)
                                                    -- [(AttrName, AttrName, AType)]
getTBLAttributeInfos :: CTX -> ElementNode -> XPath -> TBLAttributeInfos
----------------------------------------------------------------------
----------------------------------------------------------------------
getTBLAttributeInfos (xpathPrefixForAlgebraNode,  (documentNode, tableInfosNode), _)  elementNode xpath = tblAttributeInfos
 {-
  (<column name="COLNAME" tname="TCOLNAME" type="DATATYPE"/>)+    
-}
    where
        
        columnElements = 
            evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForAlgebraNode ++ xpath)
        
        tblAttributeInfos = 
            map f columnElements
                where
                    f columnElement = (attrNameExternal, attrNameInternal, attrType)
                        where
                            attrNameInternal =
                                (getAttributeValueFromElementNode columnElement "name") 
                             
                            attrNameExternal =
                                getAttributeValueFromElementNode columnElement "tname" 
                              
                            typeString = getAttributeValueFromElementNode columnElement "type"
                            attrType = case typeString of
                                "int"   ->         AT_INT 
                                "str"   ->         AT_STR 
                                "bool"  ->         AT_BOOL
                                "dbl"   ->         AT_DEC 
                                "dec"   ->         AT_DBL 
                                "nat"   ->         AT_NAT  
                                
 
----------------------------------------------------------------------
----------------------------------------------------------------------
                                           -- [[AttrName]]
getKeyInfos :: CTX -> ElementNode -> XPath -> KeyInfos
----------------------------------------------------------------------
----------------------------------------------------------------------
getKeyInfos (xpathPrefixForAlgebraNode,  (documentNode, tableInfosNode), _)  elementNode xpath = keyInfos
 {-
  (<key>
    (<column name="COLNAME" position="[0..n]"/>)+
  </key>)+    
-}
    where
        
        keyElements = 
            evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForAlgebraNode ++ xpath)
        
        columnElementsGroupedByKey =
            map f keyElements
                where
                    f :: XmlTree -> [XmlTree]
                    f keyElement =
                        getElementChildrenFromElementNode keyElement "column"
        
        
            
        keyInfos =
            map f1 columnElementsGroupedByKey
                where
                    f1 columnElements =
                        map f2 columnElements
                            where
                                f2 columnElement = 
                                    let
                                        attrNameString =
                                            getAttributeValueFromElementNode columnElement "name"
                                    in
                                        attrNameString   
                                
        
                                  

----------------------------------------------------------------------
----------------------------------------------------------------------
                                         -- [[AValue]]
getTuples :: CTX -> ElementNode -> XPath -> [Tuple]
----------------------------------------------------------------------
----------------------------------------------------------------------
getTuples (xpathPrefixForAlgebraNode,  (documentNode, tableInfosNode), _)  elementNode xpath = tuples
-- (<column name="COLNAME" new="true"/>
--     (<value type="DATATYPE">VALUE</value>)+
--  </column>)+
    
    where
        
        columnElements = 
            evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForAlgebraNode ++ xpath)
        
        valueElementsGroupedByColumn =
            map f columnElements
                where
                    f :: XmlTree -> [XmlTree]
                    f columnElement =
                        getElementChildrenFromElementNode columnElement "value"
        
        valueElementsGroupedByRow =
            transpose valueElementsGroupedByColumn
            
        tuples =
            map f1 valueElementsGroupedByRow
                where
                    f1 valueElements =
                        map f2 valueElements
                            where
                                f2 valueElement = 
                                    let
                                        typeString =
                                            getAttributeValueFromElementNode valueElement "type"
                                        valueString =
                                            getElementValue  valueElement
                                    in
                                        case typeString of
                                            "int"   ->        AV_INT ((read valueString)::Integer)   
                                            "str"   ->        AV_STR valueString
                                            "bool"  ->        AV_BLN (case valueString of {"true" -> True; "false" -> False; "True" -> True; "False" -> False})    
                                            "dbl"   ->        AV_DBL ((read valueString)::Double)    
                                            "dec"   ->        AV_DEC ((read valueString)::Float)    
                                            "nat"   ->        AV_NAT ((read valueString)::Integer)           


----------------------------------------------------------------------
----------------------------------------------------------------------
                                                 -- [(JoinComparisonKind, (LeftAttrName,RightAttrName))]
getJoinPredicates :: CTX -> ElementNode -> XPath -> JoinPredicates
----------------------------------------------------------------------
----------------------------------------------------------------------
getJoinPredicates (xpathPrefixForAlgebraNode,  (documentNode, tableInfosNode), _)  elementNode xpath = joinPredicates
--   (<comparison kind="KIND">
--      <column name="COLNAME" new="false" position="1"/>
--      <column name="COLNAME" new="false" position="2"/>
--    </comparison>)*

    where
        joinPredicates = map f comparisonElements
        
        comparisonElements =
            evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForAlgebraNode ++ xpath)
            
        f comparisonElement =
            (joinComparisonKind, (leftAttrName, rightAttrName))
                where
                    
                    joinComparisonKind = 
                        case (getAttributeValueFromElementNode comparisonElement "kind") of
                            "eq"   ->    TJ_EQ
                            "gt"   ->    TJ_GT
                            "ge"   ->    TJ_GE 
                            "lt"   ->    TJ_LT
                            "le"   ->    TJ_LE
                            "ne"   ->    TJ_NE
                    
                    leftAttrName = 
                        (getAttributeValueFromElementNode
                            (getNthNode (getElementChildrenFromElementNode comparisonElement "column") 0)
                            "name")
                            
                    rightAttrName = 
                        (getAttributeValueFromElementNode
                            (getNthNode (getElementChildrenFromElementNode comparisonElement "column") 1)
                            "name")  
            



----------------------------------------------------------------------
----------------------------------------------------------------------
                                           -- [(NewAttrName, OldAttrName)]
getProjInfos :: CTX -> ElementNode -> XPath -> ProjInfos
----------------------------------------------------------------------
----------------------------------------------------------------------
getProjInfos (xpathPrefixForAlgebraNode,  (documentNode, tableInfosNode), _)  elementNode xpath = projInfos
--   (<column name="COLNAME" old_name="COLNAME" new="true"/>
--  | <column name="COLNAME" new="false"/>)*

    where
        projInfos = map f projColumnElements
        
        projColumnElements =
            evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForAlgebraNode ++ xpath)
            
        f projColumnElement =
            (newAttrName, oldAttrName)
                where
                    newAttrName = 
                        (getAttributeValueFromElementNode projColumnElement "name")
                    oldAttrName = 
                        case (getAttributeValueFromElementNode projColumnElement "old_name") of
                            []   ->  newAttrName  
                            name ->  (name)  
            
                       
                
----------------------------------------------------------------------
----------------------------------------------------------------------
                                           -- [(SortAttrName, SortDirection)]
getSortInfos :: CTX -> ElementNode -> XPath -> SortInfos
----------------------------------------------------------------------
----------------------------------------------------------------------
getSortInfos (xpathPrefixForAlgebraNode,  (documentNode, tableInfosNode), _)  elementNode xpath = sortInfos
-- (<column name="COLNAME" function="sort" position="[0..n]" direction="DIRECTION" new="false"/>)+
    
    where
        sortInfos = map f sortColumnElements
        
        sortColumnElements =
            evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForAlgebraNode ++ xpath)
            
        f sortColumnElement =
            (sortAttrName, sortDirection)
                where
                    sortAttrName = 
                        (getAttributeValueFromElementNode sortColumnElement "name")
                    sortDirection = 
                        case (getAttributeValueFromElementNode sortColumnElement "direction") of
                            "ascending"  ->  Ascending  
                            "descending" ->  Descending  
            
                      


----------------------------------------------------------------------
----------------------------------------------------------------------
getAttributeNames :: CTX -> ElementNode -> XPath -> [String]
----------------------------------------------------------------------
----------------------------------------------------------------------
getAttributeNames  (xpathPrefixForAlgebraNode,  (documentNode, tableInfosNode), _)  elementNode xpath = attributeNames
-- (<column name="COLNAME" .../>)+
    
    where
        
        columnElements = 
            evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForAlgebraNode ++ xpath)
            
        attributeNames = 
            map f columnElements
                where
                    f columnElement =
                        (getAttributeValueFromElementNode columnElement "name")    

            
    

----------------------------------------------------------------------
----------------------------------------------------------------------
getAttributeName :: CTX -> ElementNode -> XPath -> String
----------------------------------------------------------------------
----------------------------------------------------------------------
getAttributeName  (xpathPrefixForAlgebraNode,  (documentNode, tableInfosNode), _)  elementNode xpath = 
    (getAttributeValueFromAttributeNode (getNthNode (evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForAlgebraNode ++ xpath)) 0))


----------------------------------------------------------------------
----------------------------------------------------------------------
getOptionalAttributeName :: CTX -> ElementNode -> XPath -> Maybe String
----------------------------------------------------------------------
----------------------------------------------------------------------
getOptionalAttributeName  (xpathPrefixForAlgebraNode,  (documentNode, tableInfosNode), _) elementNode xpath  = 
    let
        nodes = evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForAlgebraNode ++ xpath)
        size = getNodeCount nodes
    in
        case size of
            0 -> Nothing
            1 -> Just (getAttributeValueFromAttributeNode (getNthNode (nodes) 0))   
    


----------------------------------------------------------------------
----------------------------------------------------------------------
getFunctionType1TO1 :: CTX -> ElementNode -> XPath -> FunctionType1TO1
----------------------------------------------------------------------
----------------------------------------------------------------------
getFunctionType1TO1  (xpathPrefixForAlgebraNode,  (documentNode, tableInfosNode), _)  elementNode xpath = 
    conv_2LA_functionType1TO1 (getAttributeValueFromAttributeNode (getNthNode (evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForAlgebraNode ++ xpath)) 0))




----------------------------------------------------------------------
----------------------------------------------------------------------
getNodeID :: CTX -> ElementNode -> Int
----------------------------------------------------------------------
----------------------------------------------------------------------
getNodeID (xpathPrefixForAlgebraNode,(documentNode, tableInfosNode), _) elementNode =
    getIntValue(getAttributeValueFromAttributeNode(getNthNode(evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForAlgebraNode ++ "/@id")) 0))    


----------------------------------------------------------------------
----------------------------------------------------------------------
getPFLAOpKind :: CTX -> ElementNode -> String
----------------------------------------------------------------------
----------------------------------------------------------------------
getPFLAOpKind  (xpathPrefixForAlgebraNode,(documentNode, tableInfosNode), _) elementNode = 
    getAttributeValueFromAttributeNode (getNthNode (evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForAlgebraNode ++ "/@kind")) 0)


----------------------------------------------------------------------
----------------------------------------------------------------------
getChildNode :: CTX -> ElementNode -> Int ->  AlgbExp
----------------------------------------------------------------------
----------------------------------------------------------------------
getChildNode  (xpathPrefixForAlgebraNode,(documentNode,tableInfosNode),  nodeStore) elementNode pos = algbExp
    where
        algbExp = case lookup childNodeID nodeStore of
            Just a  -> a
            Nothing -> error "mist"
            
        childNodeID = getIntValue childNodeIDString
        childNodeIDString =
            getAttributeValueFromAttributeNode ( 
                (getNthNode (evalXPathFromDocumentNodeCtx documentNode (xpathPrefixForAlgebraNode ++ "//edge/@to")) pos)
                )
         

        
    
    
    
    
    
 
    