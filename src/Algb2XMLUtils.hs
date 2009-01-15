-----------------------------------------------------------------------------------------
{-| Module      : Algb2XMLLabels
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Algb2XMLUtils(

-- =============================================================================
-- exports
-- =============================================================================
    
    getNodeKindName,
    getEdgeElements,   
    
    getPropertiesElement,
    getSemanticContentElement

) where


-- =============================================================================
-- imports
-- =============================================================================
    
import AttributeNameTransformation (AttributeNameTransformer)



import LangAlgb(AlgbExp (..), FunctionTypeAGGR (..), FunctionType1TO1 (..), SortDirection (..), JoinComparisonKind (..), AValue (..), AType (..))


import Data.List (intersperse, transpose)


import Text.XML.HXT.Arrow
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.TypeDefs

-- =============================================================================
-- functions
-- =============================================================================
    
    

----------------------------------------------------------------------
----------------------------------------------------------------------
getNodeKindName :: AlgbExp -> String
----------------------------------------------------------------------
----------------------------------------------------------------------
getNodeKindName (ROWNUM         _semInfos _e)            = "rownum" 
getNodeKindName (ROWID         _semInfos _e)             = "rowid"
getNodeKindName (ROWRANK        _semInfos _e)            = "rowrank"
getNodeKindName (RANK           _semInfos _e)            = "rank"
 
getNodeKindName (PROJ           _semInfos _e)            = "project" 
getNodeKindName (SEL            _semInfos _e)            = "select"       
getNodeKindName (POS_SELECT     _semInfos _e)            = "pos_select"       

getNodeKindName (CROSS          _e1 _e2)                 = "cross" 
getNodeKindName (EQJOIN         _semInfos _e1 _e2)       = "eqjoin" 
getNodeKindName (SEMIJOIN         _semInfos _e1 _e2)     = "semijoin" 
getNodeKindName (THETAJOIN      _semInfos _e1 _e2)       = "thetajoin" 

getNodeKindName (DISJUNION      _e1 _e2)                 = "union" 
getNodeKindName (DIFFERENCE      _e1 _e2)                = "difference" 

getNodeKindName (DISTINCT       _e1)                     = "distinct" 

getNodeKindName (LIT_TBL        _semInfos _schemaInfos)  = "table" 
getNodeKindName (EMPTY_TBL      _schemaInfos)            = "empty_tbl" 
getNodeKindName (TABLEREF       _schemaInfos)            = "ref_tbl" 

getNodeKindName (ATTACH         _semInfos _e)            = "attach" 

getNodeKindName (CAST           _semInfos _e)            = "cast" 


getNodeKindName (FUN_NUM_EQ     _semInfos _e)            = "eq" 
getNodeKindName (FUN_NUM_GT     _semInfos _e)            = "gt"

getNodeKindName (FUN_1TO1       _semInfos _e)            = "fun"

getNodeKindName (FUN_BOOL_AND   _semInfos _e)            = "and" 
getNodeKindName (FUN_BOOL_OR    _semInfos _e)            = "or" 
getNodeKindName (FUN_BOOL_NOT   _semInfos _e)            = "not" 

getNodeKindName (FUN_AGGR        semInfos _e)            = functionTypeAGGRString
    where
        (functionTypeAGGR, (_resultAttrName, _attrName), _partitioningAttrName) = semInfos

        functionTypeAGGRString = case functionTypeAGGR of
                                      FTAGGR_AVG -> "avg"  
                                      FTAGGR_MAX -> "max"
                                      FTAGGR_MIN -> "min"
                                      FTAGGR_SUM -> "sum"  
                                          
getNodeKindName (FUN_AGGR_COUNT _semInfos _e)            = "count" 


getNodeKindName (SERIALIZE_REL  _semInfos _e1 _e2)       = "serialize relation" 


getNodeKindName (NIL)       = "nil" 


----------------------------------------------------------------------
----------------------------------------------------------------------
getEdgeElements :: ArrowXml a => AlgbExp -> [a XmlTree XmlTree] 
----------------------------------------------------------------------



getEdgeElements ( ROWNUM        _semInfos                   (ADAGNodeID cid) )                      = 
    [mkelem "edge" [sattr "to" (show cid)][]]
getEdgeElements ( ROWID        _semInfos                    (ADAGNodeID cid) )                      =
    [mkelem "edge" [sattr "to" (show cid)][]]
getEdgeElements ( ROWRANK          _semInfos                (ADAGNodeID cid) )                      =
    [mkelem "edge" [sattr "to" (show cid)][]]
getEdgeElements ( RANK          _semInfos                   (ADAGNodeID cid) )                      =
    [mkelem "edge" [sattr "to" (show cid)][]]

getEdgeElements ( PROJ          _semInfos                   (ADAGNodeID cid) )                      =
    [mkelem "edge" [sattr "to" (show cid)][]]
getEdgeElements ( SEL           _semInfos                   (ADAGNodeID cid) )                      =
    [mkelem "edge" [sattr "to" (show cid)][]]

getEdgeElements ( POS_SELECT    _semInfos                   (ADAGNodeID cid) )                      =
    [mkelem "edge" [sattr "to" (show cid)][]]


getEdgeElements ( CROSS                                     (ADAGNodeID cid1) (ADAGNodeID cid2) )   =
    [mkelem "edge" [sattr "to" (show cid1)][], mkelem "edge" [sattr "to" (show cid2)][]]
getEdgeElements ( EQJOIN        _semInofs                   (ADAGNodeID cid1) (ADAGNodeID cid2) )   =
    [mkelem "edge" [sattr "to" (show cid1)][], mkelem "edge" [sattr "to" (show cid2)][]]
getEdgeElements ( SEMIJOIN        _semInofs                   (ADAGNodeID cid1) (ADAGNodeID cid2) )   =
    [mkelem "edge" [sattr "to" (show cid1)][], mkelem "edge" [sattr "to" (show cid2)][]]

getEdgeElements ( THETAJOIN     _semInofs                   (ADAGNodeID cid1) (ADAGNodeID cid2) )   =
    [mkelem "edge" [sattr "to" (show cid1)][], mkelem "edge" [sattr "to" (show cid2)][]]

getEdgeElements ( DISJUNION                                 (ADAGNodeID cid1) (ADAGNodeID cid2) )   =
    [mkelem "edge" [sattr "to" (show cid1)][], mkelem "edge" [sattr "to" (show cid2)][]]

getEdgeElements ( DIFFERENCE                                 (ADAGNodeID cid1) (ADAGNodeID cid2) )   =
    [mkelem "edge" [sattr "to" (show cid1)][], mkelem "edge" [sattr "to" (show cid2)][]]



getEdgeElements ( DISTINCT                                 (ADAGNodeID cid1)  )   =
    [mkelem "edge" [sattr "to" (show cid1)][]]


getEdgeElements ( LIT_TBL       _semInfos   _schemaInfos )                                          = []
getEdgeElements ( EMPTY_TBL                 _schemaInfos )                                          = []
getEdgeElements ( TABLEREF                  _schemaInfos )                                          = []

getEdgeElements ( ATTACH        _semInfos                   (ADAGNodeID cid) )                      =
    [mkelem "edge" [sattr "to" (show cid)][]]

getEdgeElements ( CAST        _semInfos                   (ADAGNodeID cid) )                      =
    [mkelem "edge" [sattr "to" (show cid)][]]


getEdgeElements ( FUN_NUM_EQ    _semInfos                   (ADAGNodeID cid) )                      =
    [mkelem "edge" [sattr "to" (show cid)][]]
getEdgeElements ( FUN_NUM_GT    _semInfos                   (ADAGNodeID cid) )                      =
    [mkelem "edge" [sattr "to" (show cid)][]]

getEdgeElements ( FUN_1TO1    _semInfos                     (ADAGNodeID cid) )                      =
    [mkelem "edge" [sattr "to" (show cid)][]]

getEdgeElements ( FUN_BOOL_AND  _semInfos                   (ADAGNodeID cid) )                      =
    [mkelem "edge" [sattr "to" (show cid)][]]
getEdgeElements ( FUN_BOOL_OR  _semInfos                   (ADAGNodeID cid) )                      =
    [mkelem "edge" [sattr "to" (show cid)][]]
getEdgeElements ( FUN_BOOL_NOT  _semInfos                   (ADAGNodeID cid) )                      =
    [mkelem "edge" [sattr "to" (show cid)][]]


getEdgeElements ( FUN_AGGR  _semInfos                   (ADAGNodeID cid) )                      =
    [mkelem "edge" [sattr "to" (show cid)][]]
getEdgeElements ( FUN_AGGR_COUNT  _semInfos                   (ADAGNodeID cid) )                      =
    [mkelem "edge" [sattr "to" (show cid)][]]


getEdgeElements ( SERIALIZE_REL  _semInfos                      (ADAGNodeID cid1) (ADAGNodeID cid2) )   =
    [mkelem "edge" [sattr "to" (show cid1)][], mkelem "edge" [sattr "to" (show cid2)][]]
   

getEdgeElements ( NIL )                                          = []


----------------------------------------------------------------------
----------------------------------------------------------------------
getPropertiesElement :: ArrowXml a => AlgbExp -> AttributeNameTransformer -> Maybe (a XmlTree XmlTree) 
----------------------------------------------------------------------
----------------------------------------------------------------------
getPropertiesElement (TABLEREF       semInfos) ant = 

    {-
     <properties>
        <keys>
            (<key>
                (<column name="COLNAME" position="[0..n]"/>)+
            </key>)+
        </keys>
     </properties>
     -}
     
     Just (    
        mkelem "properties" []              
        [    
            mkelem "keys" []              
                keyElements
             
        ]
     )
     
     where
                                      -- [[AttrName]]
        (_tableName, _tblAttributeInfos, keyInfos) = semInfos 
        
        keyElements = map f1 keyInfos
            where
                f1 attrNames =
                    mkelem "key" []              
                        columnElements
                        
                            where
                                columnElements = map f2 (zip [0..] attrNames)
                                    where
                                        f2 (pos, attrName) =
                                            mkelem "column" [sattr "name" (ant attrName), sattr "position" (show pos)]
                                            []        
            



getPropertiesElement (_) _ant = Nothing 




----------------------------------------------------------------------
----------------------------------------------------------------------
getSemanticContentElement :: ArrowXml a => AlgbExp -> AttributeNameTransformer -> Maybe (a XmlTree XmlTree) 
----------------------------------------------------------------------
----------------------------------------------------------------------

getSemanticContentElement (ROWNUM        semInfos _e)    ant        =  
   
   {-
    <content>
       <column name="COLNAME" new="true"/>
      (<column name="COLNAME" function="sort" position="[0..n]" direction="DIRECTION" new="false"/>)+
      (<column name="COLNAME" function="partition" new="false"/>)?
    </content>
    -}
          
     Just ( 
        mkelem "content" []              
        (
        [
            mkelem "column" [sattr "name" (ant resultAttrName), sattr "new" "true"][]
        ]   ++ orderingColumns
            ++ partitioningColumn
        )
     )           
        where

                              -- [(SortAttrName, SortDirection)]  
            (resultAttrName,  sortInfos,  partitioningAttrName) = semInfos

           
            orderingColumns = map f1 (zip [0..] sortInfos)
                where           
                    f1 (position,(sortAttrName, sortDirection)) = 
                        mkelem "column" [
                                            sattr "name" (ant sortAttrName),
                                            sattr "function" "sort",
                                            sattr "position" (show position),
                                            sattr "direction" directionString, 
                                            sattr "new" "false"
                                         ][]           
                        where
                            directionString = case sortDirection of
                                                 Ascending  -> "ascending" 
                                                 Descending -> "descending"
            
            partitioningColumn = case partitioningAttrName of
                                                Just name   -> [mkelem "column"  [
                                                                                    sattr "name" (ant name),
                                                                                    sattr "function" "partition",
                                                                                    sattr "new" "false"    
                                                                                ][]
                                                               ]
                                                Nothing     -> []

    

getSemanticContentElement (ROWID        semInfos _e)    ant        = 

    {-
     <content>
       <column name="COLNAME" new="true"/>
     </content>
    -}
     Just (   
        mkelem "content" []              
        [
            mkelem "column" [sattr "name" (ant resultAttrName), sattr "new" "true"][]
        ]   
      )      
            
        where
            
            resultAttrName = semInfos
 


getSemanticContentElement (ROWRANK        semInfos _e)    ant        =  
   
   {-
     <content>
       <column name="COLNAME" new="true"/>
      (<column name="COLNAME" function="sort" position="[0..n]" direction="DIRECTION" new="false"/>)+
     </content>
    -}
          
     Just ( 
        mkelem "content" []              
        (
        [
            mkelem "column" [sattr "name" (ant resultAttrName), sattr "new" "true"][]
        ]   ++ orderingColumns
        )
     )           
        where

                              -- [(SortAttrName, SortDirection)]  
            (resultAttrName,  sortInfos) = semInfos

           
            orderingColumns = map f1 (zip [0..] sortInfos)
                where           
                    f1 (position,(sortAttrName, sortDirection)) = 
                        mkelem "column" [
                                            sattr "name" (ant sortAttrName),
                                            sattr "function" "sort",
                                            sattr "position" (show position),
                                            sattr "direction" directionString, 
                                            sattr "new" "false"
                                         ][]           
                        where
                            directionString = case sortDirection of
                                                 Ascending  -> "ascending" 
                                                 Descending -> "descending"
      


getSemanticContentElement (RANK        semInfos _e)    ant        =  
   
   {-
     <content>
       <column name="COLNAME" new="true"/>
      (<column name="COLNAME" function="sort" position="[0..n]" direction="DIRECTION" new="false"/>)+
     </content>
    -}
          
     Just ( 
        mkelem "content" []              
        (
        [
            mkelem "column" [sattr "name" (ant resultAttrName), sattr "new" "true"][]
        ]   ++ orderingColumns
        )
     )           
        where

                              -- [(SortAttrName, SortDirection)]  
            (resultAttrName,  sortInfos) = semInfos

           
            orderingColumns = map f1 (zip [0..] sortInfos)
                where           
                    f1 (position,(sortAttrName, sortDirection)) = 
                        mkelem "column" [
                                            sattr "name" (ant sortAttrName),
                                            sattr "function" "sort",
                                            sattr "position" (show position),
                                            sattr "direction" directionString, 
                                            sattr "new" "false"
                                         ][]           
                        where
                            directionString = case sortDirection of
                                                 Ascending  -> "ascending" 
                                                 Descending -> "descending"
            
            



getSemanticContentElement (PROJ          semInfos _e)   ant         =  

    {-
    <content>
     (<column name="COLNAME" old_name="COLNAME" new="true"/>
    | <column name="COLNAME" new="false"/>)*
    </content>
    -}
    Just (
        mkelem "content" []              
            projectionColumns
    )    

       
        where
        
            -- [(NewAttrName, OldAttrName)]
            projInfos = semInfos
            
            projectionColumns = map f projInfos
                where
            
                    f (newAttrName, oldAttrName) = 
                        if (newAttrName == oldAttrName) 
                            then 
                                mkelem "column" [
                                                    sattr "name" (ant newAttrName), 
                                                    sattr "new" "false"
                                                 ][] 
                            else 
                                mkelem "column" [
                                                    sattr "name" (ant newAttrName), 
                                                    sattr "old_name" (ant oldAttrName), 
                                                    sattr "new" "true"
                                                 ][] 



getSemanticContentElement (SEL           semInfos _e)    ant        =     

    {-
    <content>
       <column name="COLNAME" new="false"/>
     </content>
    -}

    Just (    
        mkelem "content" []              
        [
            mkelem "column" [sattr "name" (ant selectionAttrName), sattr "new" "false"][]
        ]   
     )   
            
    where
        
            selectionAttrName = semInfos




getSemanticContentElement (POS_SELECT        semInfos _e)    ant        =  
   
   {-
    <content>
       <position>POSITION</position>
      (<column name="COLNAME" function="sort" position="[0..n]" direction="DIRECTION" new="false"/>)+
      (<column name="COLNAME" function="partition" new="false"/>)?
    </content>
    -}
          
     Just ( 
        mkelem "content" []              
        (
        [
            mkelem "position" [][txt (show position)]
        ]   ++ orderingColumns
            ++ partitioningColumn
        )
     )           
        where

                              -- [(SortAttrName, SortDirection)]  
            (position,  sortInfos,  partitioningAttrName) = semInfos

           
            orderingColumns = map f1 (zip [0..] sortInfos)
                where           
                    f1 (position,(sortAttrName, sortDirection)) = 
                        mkelem "column" [
                                            sattr "name" (ant sortAttrName),
                                            sattr "function" "sort",
                                            sattr "position" (show position),
                                            sattr "direction" directionString, 
                                            sattr "new" "false"
                                         ][]           
                        where
                            directionString = case sortDirection of
                                                 Ascending  -> "ascending" 
                                                 Descending -> "descending"
            
            partitioningColumn = case partitioningAttrName of
                                                Just name   -> [mkelem "column"  [
                                                                                    sattr "name" (ant name),
                                                                                    sattr "function" "partition",
                                                                                    sattr "new" "false"    
                                                                                ][]
                                                               ]
                                                Nothing     -> []







getSemanticContentElement (CROSS         _e1 _e2)       ant          = 

    Nothing 



getSemanticContentElement (EQJOIN        semInfos _e1 _e2)    ant   = 

    {-
     <content>
       <column name="COLNAME" new="false" position="1"/>
       <column name="COLNAME" new="false" position="2"/>
     </content>            
    -}
    Just (    
        mkelem "content" []              
        [
            mkelem "column" [sattr "name" (ant leftAttrName), sattr "new" "false", sattr "position" "1"][],
            mkelem "column" [sattr "name" (ant rightAttrName), sattr "new" "false", sattr "position" "2"][]
        ]   
        
    )     
   
        where
            
            (leftAttrName,rightAttrName) = semInfos


getSemanticContentElement (SEMIJOIN        semInfos _e1 _e2)    ant   = 

    {-
     <content>
       <column name="COLNAME" new="false" position="1"/>
       <column name="COLNAME" new="false" position="2"/>
     </content>            
    -}
    Just (    
        mkelem "content" []              
        [
            mkelem "column" [sattr "name" (ant leftAttrName), sattr "new" "false", sattr "position" "1"][],
            mkelem "column" [sattr "name" (ant rightAttrName), sattr "new" "false", sattr "position" "2"][]
        ]   
        
    )     
   
        where
            
            (leftAttrName,rightAttrName) = semInfos



getSemanticContentElement (THETAJOIN     semInfos _e1 _e2)   ant    = 
    
     {-
      <content>
       (<comparison kind="KIND">
          <column name="COLNAME" new="false" position="1"/>
          <column name="COLNAME" new="false" position="2"/>
        </comparison>)*
     </content>             
    -}      
    
    Just (    
        mkelem "content" []              
            comparisonElements
        
    )     
    
    
    where
    
               -- [(JoinComparisonKind, (LeftAttrName,RightAttrName))]
        list = semInfos
        
        comparisonElements = map f list
            where
        
                f (thetaJoinComparisonKind, (leftAttrName,rightAttrName)) = 
                    
                    mkelem "comparison" [sattr "kind" kindString]
                    [
                        mkelem "column" [sattr "name" (ant leftAttrName), sattr "new" "false", sattr "position" "1"][],
                        mkelem "column" [sattr "name" (ant rightAttrName), sattr "new" "false", sattr "position" "2"][]
                    ]
                    
                    where
                
                        kindString = case thetaJoinComparisonKind of
                                        TJ_EQ -> "eq"
                                        TJ_GT -> "gt"
                                        TJ_GE -> "ge" 
                                        TJ_LT -> "lt"
                                        TJ_LE -> "le"
                                        TJ_NE -> "ne"


getSemanticContentElement (DISJUNION     _e1 _e2)          ant       = 
    
    Nothing 


getSemanticContentElement (DIFFERENCE     _e1 _e2)          ant       = 
    
    Nothing 


getSemanticContentElement (DISTINCT     _e1)          ant       = 
    
    Nothing 

getSemanticContentElement (LIT_TBL       semInfos schemaInfos) ant = 

    {-
     <content>
       (<column name="COLNAME" new="true"/>
         (<value type="DATATYPE">VALUE</value>)+
        </column>)+
     </content>
     -}
     
     Just (    
        mkelem "content" []              
            columnElements
        
     )   

        where
            
            -- [(AttrName, AType)]
            columnNames = schemaInfos 
            
            -- [[AValue]]    
            -- [[1, "hallo1"], [2, "hallo2"]]
            tuples = semInfos         
            -- [[1, 2], ["hallo1", "hallo2"]]
            tuplesGroupedByColumn = transpose tuples
            
            -- [("iter", [1, 2]), ("item", ["hallo1", "hallo2"])]
            columnNamesANDtheirTuples = zip columnNames tuplesGroupedByColumn
            
            
            columnElements = map f1 columnNamesANDtheirTuples
                where
                    f1 ((attrName, aType), columnValues) = 
                        mkelem "column" [sattr "name" (ant attrName), sattr "new" "true"]
                            valueElements
                        where
                            valueElements = map f2 columnValues
                                where
                                    f2 (aValue) = 
                                        mkelem "value" [sattr "type" typeString]
                                        [
                                            txt valueString
                                        ]
                                        where
                                            typeString = show aType
                                            
                                            valueString = case aValue of
                                                             
                                                 AV_STR x     -> x
                                                 _            -> show aValue
                                              
                                                


getSemanticContentElement (EMPTY_TBL     schemaInfos)       ant     =

    
    {-
    <content>
      (<column name="COLNAME" type="DATATYPE" new="true"/>)*
    </content>            
    -}
    
    Just (    
        mkelem "content" []              
            columnElements
        
     )   

        where
            
            -- [(AttrName, AType)]
            columnNames = schemaInfos
            
            columnElements = map f1 columnNames
                where
                    f1 (attrName, aType) = 
                        mkelem "column" [sattr "name" (ant attrName), sattr "type" (show aType), sattr "new" "true"][]
                            
              



getSemanticContentElement (TABLEREF       semInfos) ant = 

    {-
     <content>
        <table name="TABLENAME">
            (<column name="COLNAME" tname="TCOLNAME" type="DATATYPE"/>)+    
        </table>
     </content>
     -}
     
     Just (    
        mkelem "content" []              
        [    
            mkelem "table" [sattr "name" tableName] 
                columnElements 
        ]
     )   

        where
                     -- (external attribute name, internal attribute name, attribute type)
                     -- [(AttrName, AttrName, AType)]
            (tableName, tblAttributeInfos, _keyInfos) = semInfos 
            
            
            
            columnElements = map f1 tblAttributeInfos
                where
                    f1 (attrNameExternal, attrNameInternal, attrType) = 
                        mkelem "column" [sattr "name" (ant attrNameInternal), 
                                         sattr "tname" attrNameExternal, 
                                         sattr "type" typeString][]
                           
                            where
                                
                                typeString = show attrType

                           
                            
                                                
                                                   
    






getSemanticContentElement (ATTACH        semInfos _e)      ant      =


    {-
    <content>
       <column name="COLNAME" new="true">
         <value type="DATATYPE">VALUE</value>
       </column>
     </content>            
    -}
    
    Just (    
        mkelem "content" []
        [              
            mkelem "column" [sattr "name" (ant attrName), sattr "new" "true"]
            [
                mkelem "value" [sattr "type" typeString]
                [
                    txt valueString
                ]
            ]
        ]
     )   

    
        where 
                             
            (attrName, aTypedValue) = semInfos
            (aType, aValue) = aTypedValue
            
            typeString = show aType
                
            valueString = case aValue of
                                         
                             AV_STR x     -> x
                             _            -> show aValue           
    
                

getSemanticContentElement (CAST        semInfos _e)      ant      =


    {-
    <content>
       <column name="COLNAME" new="true"/>
       <column name="COLNAME" new="false"/>
       <type name="DATATYPE"/>
    </content>
          
    -}
    
    Just (    
        mkelem "content" []
        [              
            mkelem "column" [sattr "name" (ant resultAttrName), sattr "new" "true"][],
            mkelem "column" [sattr "name" (ant attrName), sattr "new" "false"][],
            mkelem "type" [sattr "name" (show aType)][]
        ]
     )   

    
        where 
                             
            (resultAttrName, attrName, aType) = semInfos
           
           
           

getSemanticContentElement (FUN_NUM_EQ    semInfos _e)       ant     = 

   {- 
   <content>
      <column name="COLNAME" new="true"/>
      <column name="COLNAME" new="false" position="1"/>
      <column name="COLNAME" new="false" position="2"/>
   </content>
   -}

   Just (    
        mkelem "content" []
        [              
            mkelem "column" [sattr "name" (ant resultAttrName), sattr "new" "true"][],
            mkelem "column" [sattr "name" (ant leftOperandAttrName), sattr "new" "false", sattr "position" "1"][],
            mkelem "column" [sattr "name" (ant rightOperandAttrName), sattr "new" "false", sattr "position" "2"][]
            
        ]
   )   
    
        where
                   
            (resultAttrName, (leftOperandAttrName,rightOperandAttrName)) = semInfos
            
            

getSemanticContentElement (FUN_NUM_GT    semInfos _e)      ant      = 

   {- 
   <content>
      <column name="COLNAME" new="true"/>
      <column name="COLNAME" new="false" position="1"/>
      <column name="COLNAME" new="false" position="2"/>
   </content>
   -}

   Just (    
        mkelem "content" []
        [              
            mkelem "column" [sattr "name" (ant resultAttrName), sattr "new" "true"][],
            mkelem "column" [sattr "name" (ant leftOperandAttrName), sattr "new" "false", sattr "position" "1"][],
            mkelem "column" [sattr "name" (ant rightOperandAttrName), sattr "new" "false", sattr "position" "2"][]
            
        ]
   )   
    
        where
                   
            (resultAttrName, (leftOperandAttrName,rightOperandAttrName)) = semInfos




getSemanticContentElement (FUN_1TO1    semInfos _e)       ant     = 

   {- 
    <content>
       <kind name="FUNCTION"/>
       <column name="COLNAME" new="true"/>
      (<column name="COLNAME" new="false" position="[0..n]"/>)*
    </content>
   -}

   Just (    
        mkelem "content" []
        (
        [              
            mkelem "kind"   [sattr "name" (ant functionType1To1Name)][],
            mkelem "column" [sattr "name" (ant resultAttrName), sattr "new" "true"][]
        ]++ operandElements
        )
   )   
    
        where
            (functionType1TO1, resultAttrName, operandAttrNames) = semInfos
            functionType1To1Name = case functionType1TO1 of
                                                FT1TO1_ADD        -> "add"
                                                FT1TO1_SUBTRACT   -> "subtract"
                                                FT1TO1_MULTIPLY   -> "multiply"
                                                FT1TO1_DIVIDE     -> "divide"
                                                FT1TO1_MODULO     -> "modulo"

            operandElements = map f (zip [0..] operandAttrNames)
                where
                    f (pos, attrName) =
                        mkelem "column" [sattr "name" (ant attrName), sattr "new" "false", sattr "position" (show pos)][]
           



getSemanticContentElement (FUN_BOOL_AND    semInfos _e)      ant      = 

   {- 
   <content>
      <column name="COLNAME" new="true"/>
      <column name="COLNAME" new="false" position="1"/>
      <column name="COLNAME" new="false" position="2"/>
   </content>
   -}

   Just (    
        mkelem "content" []
        [              
            mkelem "column" [sattr "name" (ant resultAttrName), sattr "new" "true"][],
            mkelem "column" [sattr "name" (ant leftOperandAttrName), sattr "new" "false", sattr "position" "1"][],
            mkelem "column" [sattr "name" (ant rightOperandAttrName), sattr "new" "false", sattr "position" "2"][]
            
        ]
   )   
    
        where
                   
            (resultAttrName, (leftOperandAttrName,rightOperandAttrName)) = semInfos


getSemanticContentElement (FUN_BOOL_OR    semInfos _e)      ant      = 

   {- 
   <content>
      <column name="COLNAME" new="true"/>
      <column name="COLNAME" new="false" position="1"/>
      <column name="COLNAME" new="false" position="2"/>
   </content>
   -}

   Just (    
        mkelem "content" []
        [              
            mkelem "column" [sattr "name" (ant resultAttrName), sattr "new" "true"][],
            mkelem "column" [sattr "name" (ant leftOperandAttrName), sattr "new" "false", sattr "position" "1"][],
            mkelem "column" [sattr "name" (ant rightOperandAttrName), sattr "new" "false", sattr "position" "2"][]
            
        ]
   )   
    
        where
                   
            (resultAttrName, (leftOperandAttrName,rightOperandAttrName)) = semInfos


getSemanticContentElement (FUN_BOOL_NOT  semInfos _e)     ant       =


    {- 
    <content>
      <column name="COLNAME" new="true"/>
      <column name="COLNAME" new="false"/>
    </content>
    -}

   Just (    
        mkelem "content" []
        [              
            mkelem "column" [sattr "name" (ant resultAttrName), sattr "new" "true"][],
            mkelem "column" [sattr "name" (ant operandAttrName), sattr "new" "false"][]
        ]
   )   
    
        where
                   
             (resultAttrName, operandAttrName) = semInfos





getSemanticContentElement (FUN_AGGR  semInfos _e)     ant       =


    {- 
    <content>
      <column name="COLNAME" new="true"/>
      <column name="COLNAME" new="false" function="item"/>
     (<column name="COLNAME" function="partition" new="false"/>)?
    </content>
    -}

   Just (    
        mkelem "content" []
        (
        [              
            mkelem "column" [sattr "name" (ant resultAttrName), sattr "new" "true"][],
            mkelem "column" [sattr "name" (ant operandAttrName), sattr "new" "false", sattr "function" "item"][]
        ]++ partitioningColumn
        )
   )   
    
        where
            (functionTypeAGGR, (resultAttrName, operandAttrName), partitioningAttrName) = semInfos       
          
            partitioningColumn = case partitioningAttrName of
                                                Just name   -> [mkelem "column"  [
                                                                                    sattr "name" (ant name),
                                                                                    sattr "function" "partition",
                                                                                    sattr "new" "false"    
                                                                                ][]
                                                               ]
                                                Nothing     -> []




getSemanticContentElement (FUN_AGGR_COUNT  semInfos _e)     ant       =


    {- 
    <content>
      <column name="COLNAME" new="true"/>
      <column name="COLNAME" function="partition" new="false"/>)?
    </content>
    -}

   Just (    
        mkelem "content" []
        (
        [              
            mkelem "column" [sattr "name" (ant resultAttrName), sattr "new" "true"][]
        ]++ partitioningColumn
        )
   )   
    
        where
            (resultAttrName, partitioningAttrName) = semInfos       
          
            partitioningColumn = case partitioningAttrName of
                                                Just name   -> [mkelem "column"  [
                                                                                    sattr "name" (ant name),
                                                                                    sattr "function" "partition",
                                                                                    sattr "new" "false"    
                                                                                ][]
                                                               ]
                                                Nothing     -> []



getSemanticContentElement (SERIALIZE_REL  semInfos _e1 _e2)     ant       =
    
    
    {-
     <content>
       <column name="COLNAME" new="false" function="iter"/>
       <column name="COLNAME" new="false" function="pos"/>
      (<column name="COLNAME" new="false" function="item"  position="[0..n]"/>)+
     </content>
    -}

    
    Just (    
        mkelem "content" []
        (
        [              
            mkelem "column" [sattr "name" (ant attrNameIter),  sattr "new" "false", sattr "function" "iter"][],
            mkelem "column" [sattr "name" (ant attrNamePos),  sattr "new" "false", sattr "function" "pos"][]
        ] ++ itemColumns
        )
    )   
    
        where
                   
             (attrNameIter, attrNamePos, items) = semInfos
             
             itemColumns = map f (zip [0..] items)
                where
                    
                    f (pos, attrName) =
                        mkelem "column" [sattr "name" (ant attrName),  
                                         sattr "new" "false", 
                                         sattr "function" "item", 
                                         sattr "position" (show pos)][]    






getSemanticContentElement (NIL)          ant       = 

    Nothing


