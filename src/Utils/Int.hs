-----------------------------------------------------------------------------------------
{-| Module      : Utils.Int
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Utils.Int(

-- =============================================================================
-- exports
-- =============================================================================

    stringToInt

) where

-- =============================================================================
-- imports
-- =============================================================================

import Data.Char(digitToInt)



-- =============================================================================
-- functions
-- =============================================================================

----------------------------------------------------------------------
----------------------------------------------------------------------
stringToInt :: String -> Int
----------------------------------------------------------------------
----------------------------------------------------------------------

stringToInt (h:t)   =  foldl (\a b ->( (a * 10) + digitToInt b  )) (digitToInt h) t  

stringToInt []      = error("param must not be empty")