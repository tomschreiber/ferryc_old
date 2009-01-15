-----------------------------------------------------------------------------------------
{-| Module      : XML2AlgUtils
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module XML2AlgUtils (

-- =============================================================================
-- exports
-- =============================================================================

    conv_2LA_functionType1TO1,
    conv_2LA_functionTypeAGGR    

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
-- functions
-- =============================================================================


----------------------------------------------------------------------
----------------------------------------------------------------------
conv_2LA_functionType1TO1 :: String -> FunctionType1TO1
----------------------------------------------------------------------
----------------------------------------------------------------------
conv_2LA_functionType1TO1 "add"           =       FT1TO1_ADD
conv_2LA_functionType1TO1 "subtract"      =       FT1TO1_SUBTRACT 
conv_2LA_functionType1TO1 "multiply"      =       FT1TO1_MULTIPLY
conv_2LA_functionType1TO1 "divide"        =       FT1TO1_DIVIDE 
conv_2LA_functionType1TO1 "modulo"        =       FT1TO1_MODULO 



----------------------------------------------------------------------
----------------------------------------------------------------------
conv_2LA_functionTypeAGGR :: String -> FunctionTypeAGGR
----------------------------------------------------------------------
----------------------------------------------------------------------
conv_2LA_functionTypeAGGR "avg"           =       FTAGGR_AVG
conv_2LA_functionTypeAGGR "max"           =       FTAGGR_MAX 
conv_2LA_functionTypeAGGR "min"           =       FTAGGR_MIN
conv_2LA_functionTypeAGGR "sum"           =       FTAGGR_SUM 