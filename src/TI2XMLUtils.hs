-----------------------------------------------------------------------------------------
{-| Module      : TI2XMLUtils
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module TI2XMLUtils (

-- =============================================================================
-- exports
-- =============================================================================
    
    getColsElement,
    getSubsElement

) where


-- =============================================================================
-- imports
-- =============================================================================
    



import Core2Algb(TableInfos (..)) 


import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.TypeDefs

-- =============================================================================
-- functions
-- =============================================================================
    



----------------------------------------------------------------------
----------------------------------------------------------------------
getColsElement :: ArrowXml a => TableInfos -> a XmlTree XmlTree 
----------------------------------------------------------------------

getColsElement (TI (_algbExp, cols, _subs)) =

    
    mkelem "cols"[]
        columnElements
        
    
    
        where
            columnElements = map f (zip [1..] cols)
                where
                    f (pos, colName) =
                        mkelem "column"[sattr "name" colName, sattr "position" (show pos)][]
                        



----------------------------------------------------------------------
----------------------------------------------------------------------
getSubsElement :: ArrowXml a => TableInfos -> a XmlTree XmlTree 
----------------------------------------------------------------------

getSubsElement (TI (_algbExp, _cols, subs)) =

    
    mkelem "subs"[]
        subtableRefElements
        
    
    
        where
            subtableRefElements = map f subs
                where
                    f (attrName, TIDAGNodeID id) =
                        mkelem "subtable_reference"[sattr "column_name" attrName, sattr "ref" (show id)][]
                        


