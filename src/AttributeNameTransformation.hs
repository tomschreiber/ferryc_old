-----------------------------------------------------------------------------------------
{-| Module      : AttributeNameTransformation
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module AttributeNameTransformation (

-- =============================================================================
-- exports
-- =============================================================================
    
    AttributeNameTransformer,
    
    getPFUniqueName,
    
    

) where


-- =============================================================================
-- imports
-- =============================================================================
    
import Utils.Int(stringToInt) 



-- =============================================================================
-- types
-- =============================================================================


----------------------------------------------------------------------
----------------------------------------------------------------------
type AttributeNameTransformer = String -> String 
----------------------------------------------------------------------
----------------------------------------------------------------------


type PaperString = String
type PFString = String


-- =============================================================================
-- functions
-- =============================================================================








----------------------------------------------------------------------
----------------------------------------------------------------------
getPFUniqueName :: PaperString -> PFString 
----------------------------------------------------------------------
----------------------------------------------------------------------

getPFUniqueName "iter"       =       "iter"
getPFUniqueName "iter'"      =       "iter100"

getPFUniqueName "ord"        =       "iter200"
getPFUniqueName "ord'"       =       "iter201"


getPFUniqueName "item'"      =       "iter300"
getPFUniqueName "item''"     =       "iter301"

getPFUniqueName "res"        =       "iter400"
getPFUniqueName "res'"       =       "iter401"

getPFUniqueName "inner"      =       "iter500"
getPFUniqueName "outer"      =       "iter501"



getPFUniqueName "pos"       =       "pos"

getPFUniqueName "pos'"      =       "pos100"
getPFUniqueName "pos''"      =      "pos101"

getPFUniqueName "ord"       =       "pos200"
getPFUniqueName "ord'"      =       "pos201"

getPFUniqueName "one"      =        "pos300"
getPFUniqueName "two"      =        "pos301"

getPFUniqueName "sort"      =       "pos401"

getPFUniqueName "grpKey"      =     "pos501"




getPFUniqueName n            =       
    if isItemAttributeNameWithApostrophe n
        then
            "item" ++ (show ((stringToInt (getIDFromItemAttributeNameWithApostrophe n)) + 1000))    
        else
            if isSortAttribute n
                then
                    "iter" ++ (show ((stringToInt (getIDFromSortAttribute n)) + 1000))
                else
                    n         





isItemAttributeNameWithApostrophe n =
    take 4 n == "item" && ((head (reverse n)) == '\'')
    
    
getIDFromItemAttributeNameWithApostrophe n = 
    reverse (drop 1 (reverse (drop 4 n)))    





isSortAttribute n =
     take 4 n == "sort" && idMatches
        where
            nID = drop 4 n
            idMatches = length nID > 0 && ((read nID)::Integer) >= 1
 
 
getIDFromSortAttribute n = 
    drop 4 n  
            

