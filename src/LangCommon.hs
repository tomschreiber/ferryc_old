-----------------------------------------------------------------------------------------
{-| Module      : LangCommon
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module LangCommon where


type VarID = String
type FunID = String

type TblID = String
type ATRName = String





data SortDirection 
    =   Ascending
    |   Descending
    
    deriving (Show, Eq)
    
    
data ATRType = ATRT_INT                         -- int
        | ATRT_STR                              -- str
        | ATRT_BOOL                             -- bool
        | ATRT_DEC                              -- dec
        | ATRT_DBL                              -- dbl
        | ATRT_NAT                              -- nat
        | ATRT_NONE
         
        deriving (Show, Eq)    
