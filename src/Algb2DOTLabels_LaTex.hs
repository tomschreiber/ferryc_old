-----------------------------------------------------------------------------------------
{-| Module      : Algb2DOTLabels
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Algb2DOTLabels_LaTex (
-- =============================================================================
-- exports
-- =============================================================================
    getLabel


) where


-- =============================================================================
-- imports
-- =============================================================================
    


import LangAlgb(AlgbExp (..), FunctionTypeAGGR (..), FunctionType1TO1 (..))


import Data.List (intersperse)

-- =============================================================================
-- functions
-- =============================================================================
    
    
    
    
----------------------------------------------------------------------
----------------------------------------------------------------------
getLabel :: AlgbExp -> String
----------------------------------------------------------------------
----------------------------------------------------------------------
  
getLabel ae = nodeName ++ " " ++ semanticalInfoString 
    where
    
        nodeName                = getNodeName ae
        semanticalInfoString    = getSemanticalInfoString ae
    
    
    
----------------------------------------------------------------------
----------------------------------------------------------------------
getNodeName :: AlgbExp -> String
----------------------------------------------------------------------
----------------------------------------------------------------------
getNodeName (ROWNUM        _semInfos _e)            = "ROWNUM" 
getNodeName (ROWID         _semInfos _e)            = "ROWID" 
getNodeName (ROWRANK       _semInfos _e)            = "ROWRANK" 
getNodeName (RANK          _semInfos _e)            = "RANK" 

getNodeName (PROJ          _semInfos _e)            = "$\\pi$" 
getNodeName (SEL           _semInfos _e)            = "$\\sigma$"       

getNodeName (CROSS         _e1 _e2)                 = "$\times$" 
getNodeName (EQJOIN        _semInfos _e1 _e2)       = "$\\Join$" 
getNodeName (THETAJOIN     _semInfos _e1 _e2)       = "$\\Join$" 

getNodeName (DISJUNION     _e1 _e2)                 = "$\\mathbin{\\overset{.}{\\cup}}$" 
getNodeName (DIFFERENCE     _e1 _e2)                = "$\\difference$" 
getNodeName (DISTINCT      _e1)                     = "$\\delta$" 

getNodeName (LIT_TBL       _semInfos _schemaInfos)  = "LIT\\_TBL" 
getNodeName (EMPTY_TBL     _schemaInfos)            = "EMPTY\\_TBL" 

getNodeName (TABLEREF     _schemaInfos)            =  "$\\tableref$" 

getNodeName (ATTACH        _semInfos _e)            = "$\\texttt{@}$" 


getNodeName (FUN_NUM_EQ    _semInfos _e)            = "fun num" 
getNodeName (FUN_NUM_GT    _semInfos _e)            = "fun num"

getNodeName (FUN_1TO1    _semInfos _e)              = "fun 1:1"

getNodeName (FUN_BOOL_AND  _semInfos _e)            = "fun bool" 
getNodeName (FUN_BOOL_OR   _semInfos _e)            = "fun bool" 
getNodeName (FUN_BOOL_NOT  _semInfos _e)            = "fun bool" 

getNodeName (FUN_AGGR   _semInfos _e)               = "fun aggr"
getNodeName (FUN_AGGR_COUNT  _semInfos _e)          = "fun aggr" 


getNodeName (SERIALIZE_REL _semInfos _e1 _e2)       = "$\\serialize$" 

getNodeName (NIL)       = "$\\nil$" 






----------------------------------------------------------------------
----------------------------------------------------------------------
getSemanticalInfoString :: AlgbExp -> String
----------------------------------------------------------------------
----------------------------------------------------------------------


getSemanticalInfoString e = ""

{-
getSemanticalInfoString (ROWNUM        semInfos _e)             =  
    
    -- (pos1:<sort, pos>/outer)
    "(" ++ resultAttributeString ++ ":" ++ sortAttributesString ++ partitioningAttributeString ++ ")" 

        
        where
                              -- [(SortAttrName, SortDirection)]  
            (resultAttrName,  sortInfos,  partitioningAttrName) = semInfos
        
        
            resultAttributeString = resultAttrName
            
            sortAttributesString = "<" ++ concat (intersperse ", " (map fst sortInfos)) ++ ">"
            
            partitioningAttributeString = case partitioningAttrName of
                                                Just name   -> "/" ++ name
                                                Nothing     -> ""

    

getSemanticalInfoString (ROWID        semInfos _e)            = 

    -- (inner)
    "(" ++ resultAttributeString ++ ")"
    
        where
            
            resultAttrName = semInfos
            
            
            resultAttributeString = resultAttrName
            


getSemanticalInfoString (ROWRANK        semInfos _e)             =  
    
    -- (pos1:<sort, pos>/outer)
    "(" ++ resultAttributeString ++ ":" ++ sortAttributesString ++ ")" 

        
        where
                              -- [(SortAttrName, SortDirection)]  
            (resultAttrName,  sortInfos) = semInfos
        
        
            resultAttributeString = resultAttrName
            
            sortAttributesString = "<" ++ concat (intersperse ", " (map fst sortInfos)) ++ ">"
            


getSemanticalInfoString (RANK        semInfos _e)             =  
    
    -- (pos1:<sort, pos>/outer)
    "(" ++ resultAttributeString ++ ":" ++ sortAttributesString ++ ")" 

        
        where
                              -- [(SortAttrName, SortDirection)]  
            (resultAttrName,  sortInfos) = semInfos
        
        
            resultAttributeString = resultAttrName
            
            sortAttributesString = "<" ++ concat (intersperse ", " (map fst sortInfos)) ++ ">"
            
            



getSemanticalInfoString (PROJ          semInfos _e)            =  

    -- (iter:inner, pos, item)
    "(" ++ projectionString ++ ")"
    
        where
        
            -- [(NewAttrName, OldAttrName)]
            projInfos = semInfos
            
            projectionString = concat (intersperse ", " (map f projInfos))
                where
            
                    f (newAttrName, oldAttrName) = 
                        if (newAttrName == oldAttrName) then newAttrName else newAttrName ++ ":" ++ oldAttrName





getSemanticalInfoString (SEL           semInfos _e)            =     

    "(" ++ selectionAttrName ++ ")"
    
        where
        
            selectionAttrName = semInfos



getSemanticalInfoString (CROSS         _e1 _e2)                 = 

    "" 



getSemanticalInfoString (EQJOIN        semInfos _e1 _e2)       = 

    --(iter = inner)
    "(" ++ joinPredicateString ++ ")"
        where
            
            (leftAttrName,rightAttrName) = semInfos
            
            joinPredicateString = leftAttrName ++ " = " ++ rightAttrName



getSemanticalInfoString (THETAJOIN     semInfos _e1 _e2)       = 
    
    --(iter = inner, a < b, e >= f)
    "(" ++ joinPredicatesString ++ ")"
        where
        
                   -- [(JoinComparisonKind, (LeftAttrName,RightAttrName))]
            list = semInfos
            
            joinPredicatesString = concat (intersperse ", " (map f list))
                where
            
                    f (thetaJoinComparisonKind, (leftAttrName,rightAttrName)) = 
                        leftAttrName ++ " " ++ show thetaJoinComparisonKind ++ " " ++ rightAttrName
            
            



getSemanticalInfoString (DISJUNION     _e1 _e2)                 = 
    
    "" 


getSemanticalInfoString (DIFFERENCE     _e1 _e2)                 = 
    
    "" 


getSemanticalInfoString (DISTINCT     _e1)                 = 
    
    "" 

getSemanticalInfoString (LIT_TBL       semInfos schemaInfos)  = 

    -- (a, b)
    -- [1, "hallo1"]
    -- [2, "hallo2"]
    columnNamesString ++ "\\n" ++ tuplesString
        where
        
            -- [(AttrName, AType)]
            list = schemaInfos 
            columnNamesString = "(" ++ (concat (intersperse ", " (map fst list))) ++ ")"
        
        
        
            -- [[AValue]]    
            -- [[1, "hallo1"], [2, "hallo2"]]
            tuples = semInfos            
            tuplesString =  concat (intersperse "\\n" (map f tuples))
                where
                    
                    f colums = escquot (show colums)


getSemanticalInfoString (EMPTY_TBL     schemaInfos)            =

    -- (a, b)
    columnNamesString
        where
        
            -- [(AttrName, AType)]
            list = schemaInfos 
            columnNamesString = "(" ++ (concat (intersperse ", " (map fst list))) ++ ")"




-- getSemanticalInfoString (TABLEREF        semInfos)            = ""

getSemanticalInfoString (TABLEREF        semInfos)            =

    semanticalInfoString
        where 
                    
                   -- (external,    , internal, type)       
                   -- (AttrName, AttrName, AType)       
            (tBLName, tblAttributeInfoList, keyInfoList) = semInfos
            
            
            tblAttributeInfoStringsList = "[" ++ (concat (intersperse ",\\n" (map f tblAttributeInfoList))) ++ "]"
                where
                    f (attrNameOriginal, attrNameInternal, attrType) =
                        "(" ++ attrNameOriginal ++ "," ++ attrNameInternal ++ "," ++ (show attrType) ++ ")"
              
            keyInfoStringsList =  "[" ++ (concat (intersperse ",\\n" (map f1 keyInfoList))) ++ "]"
                where
                    f1 keyList =  "(" ++ (concat (intersperse "," (map f2 keyList))) ++ ")"
                        where
                            f2 keyAttributeName = keyAttributeName  
                      
            semanticalInfoString = 
                --"(" ++ tBLName ++ ":" ++ "<" ++ (concat (intersperse "," tblAttributeInfoStringsList)) ++ ">" ++ ")"
                "(" ++ tBLName ++ ")" ++ "\\n" ++ tblAttributeInfoStringsList ++ "\\n" ++ keyInfoStringsList
               
             



getSemanticalInfoString (ATTACH        semInfos _e)            =

    -- (ord), val: 4
    attributeNameString ++ ", " ++ attributeValueString
        where 
                             
            (resultAttrName, aTypedValue) = semInfos
            (aType, aValue) = aTypedValue
            
            attributeNameString = "(" ++ resultAttrName ++ ")"
            attributeValueString = "val: " ++ escquot (show aValue) 
            


getSemanticalInfoString (FUN_NUM_EQ    semInfos _e)            = 

    -- (res:<a,b>) 
    "[=]" ++ " (" ++ resultAttributeNameString ++ ":" ++ operandAtrributeNamesString ++ ")"
        where
                   
            (resultAttrName, (leftAttrName,rightAttrName)) = semInfos
            
            resultAttributeNameString = resultAttrName
            
            operandAtrributeNamesString = "<" ++ leftAttrName ++ ", " ++ rightAttrName ++ ">"
            

getSemanticalInfoString (FUN_NUM_GT    semInfos _e)            = 

-- (res:<a,b>) 
    "[>]" ++ " (" ++ resultAttributeNameString ++ ":" ++ operandAtrributeNamesString ++ ")"
        where
                   
            (resultAttrName, (leftAttrName,rightAttrName)) = semInfos
            
            resultAttributeNameString = resultAttrName
            
            operandAtrributeNamesString = "<" ++ leftAttrName ++ ", " ++ rightAttrName ++ ">"



getSemanticalInfoString (FUN_1TO1    semInfos _e)            = 

-- [+] (res:<o_1,...,o_n>) 
    "[" ++ functionType1TO1String ++ "]" ++ " (" ++ resultAttrName ++ ":" ++ operandAtrributeNamesString ++ ")"
        where
             
            (functionType1TO1, resultAttrName, operandAttrNames) = semInfos 
            
            functionType1TO1String = case functionType1TO1 of
                                                FT1TO1_ADD        -> "+"
                                                FT1TO1_SUBTRACT   -> "-"
                                                FT1TO1_MULTIPLY   -> "*"
                                                FT1TO1_DIVIDE     -> "/"
                                                
                                                        
            operandAtrributeNamesString = "<" ++ operandAtrributeNamesString2 ++ ">"
            
            operandAtrributeNamesString2  =  concat (intersperse "," operandAttrNames)
                                     





getSemanticalInfoString (FUN_BOOL_AND    semInfos _e)            = 

-- (res:<a,b>) 
    "[AND]" ++ " (" ++ resultAttributeNameString ++ ":" ++ operandAtrributeNamesString ++ ")"
        where
                   
            (resultAttrName, (leftAttrName,rightAttrName)) = semInfos
            
            resultAttributeNameString = resultAttrName
            
            operandAtrributeNamesString = "<" ++ leftAttrName ++ ", " ++ rightAttrName ++ ">"



getSemanticalInfoString (FUN_BOOL_OR    semInfos _e)            = 

-- (res:<a,b>) 
    "[OR]" ++ " (" ++ resultAttributeNameString ++ ":" ++ operandAtrributeNamesString ++ ")"
        where
                   
            (resultAttrName, (leftAttrName,rightAttrName)) = semInfos
            
            resultAttributeNameString = resultAttrName
            
            operandAtrributeNamesString = "<" ++ leftAttrName ++ ", " ++ rightAttrName ++ ">"




getSemanticalInfoString (FUN_BOOL_NOT  semInfos _e)            =

-- res:<a> 
    "[NOT]" ++ " (" ++ resultAttrName ++ ":" ++ operandAtrributeNamesString ++ ")"
        where
                   
            (resultAttrName, attrName) = semInfos
            
            
            operandAtrributeNamesString = "<" ++ attrName ++ ">" 





getSemanticalInfoString (FUN_AGGR    semInfos _e)            = 

-- [+] (res:<a,b>) 
    "[" ++ functionTypeAGGRString ++ "]" ++ " (" ++ resultAttrName ++ ":" ++ operandAtrributeNamesString ++ partitioningAttributeString ++ ")"
        where
             
            (functionTypeAGGR, (resultAttrName, attrName), partitioningAttrName) = semInfos
            
            functionTypeAGGRString = case functionTypeAGGR of
                                          FTAGGR_AVG -> "AVG"  
                                          FTAGGR_MAX -> "MAX"
                                          FTAGGR_MIN -> "MIN"
                                          FTAGGR_SUM -> "SUM" 

                                                        
            operandAtrributeNamesString = "<" ++ attrName ++ ">"

             
            partitioningAttributeString = case partitioningAttrName of
                                                Just name   -> "/" ++ name
                                                Nothing     -> ""           
                       
                       
                       
getSemanticalInfoString (FUN_AGGR_COUNT    semInfos _e)            = 

-- [+] (res:<a,b>) 
    "[" ++ "COUNT" ++ "]" ++ " (" ++ resultAttrName ++ partitioningAttributeString ++ ")"
        where
             
            (resultAttrName, partitioningAttrName) = semInfos
            
             
            partitioningAttributeString = case partitioningAttrName of
                                                Just name   -> "/" ++ name
                                                Nothing     -> ""                              
                       
                                    
getSemanticalInfoString (SERIALIZE_REL  semInfos _e)            =
    "(" ++ iterAttrNameString ++ ", " ++ posAttrNameString ++ ", " ++ "[" ++ itemAttrNamesString ++ "]" ++ ")"

    where
        (iterAttrName, posAttrName, itemAttrNames) = semInfos
        
        iterAttrNameString =
            case iterAttrName of
                "iter" -> iterAttrName
                n      ->  "iter:" ++ n
                
        posAttrNameString =
            case posAttrName of
                "pos" -> posAttrName
                n      ->  "pos:" ++ n
        
        
        itemAttrNamesString = concat (intersperse ", " (map f (zip [1..] itemAttrNames)))
            where
                f (pos, itemAttrName) = itemAttrNameString
                
                    where
                        itemAttrNameString = 
                            if (take 4 itemAttrName == "item")
                            then
                                let 
                                    tail = drop 4 itemAttrName
                                in
                                    if ((length tail > 0) && (((read tail)::Integer) == pos)) 
                                    then
                                        itemAttrName    
                                    else 
                                        "item" ++ (show pos) ++ ":" ++ itemAttrName     
                            else   
                                "item" ++ (show pos) ++ ":" ++ itemAttrName 
 
 
-}                                
----------------------------------------------------------------------
----------------------------------------------------------------------
escquot :: String -> String
----------------------------------------------------------------------


escquot "" = ""

escquot ('"':cs) = "\\\"" ++ escquot cs
escquot (c:cs)   = c:escquot cs


