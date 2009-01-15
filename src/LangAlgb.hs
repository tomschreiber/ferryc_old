-----------------------------------------------------------------------------------------
{-| Module      : LangAlgb
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module LangAlgb(

-- =============================================================================
-- exports
-- =============================================================================

    AttrName, 
    FunctionType1TO1 (..),
    FunctionTypeAGGR (..),
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
    KeyInfos


) where

-- =============================================================================
-- imports
-- =============================================================================


import List (intersperse)
import Numeric (showFFloat)


-- =============================================================================
-- types and data
-- =============================================================================


data AType = AT_INT                              -- int
        | AT_STR                              -- str
        | AT_BOOL                             -- bool
        | AT_DEC                              -- dec
        | AT_DBL                              -- dbl
        | AT_NAT                              -- nat
        
         
        deriving (Eq)

 
instance Show AType where
    show AT_INT     = "int"
    show AT_STR     = "str"
    show AT_BOOL    = "bool"
    show AT_DEC     = "dec"
    show AT_DBL     = "dbl"
    show AT_NAT     = "nat"
   


instance Read AType where
    readsPrec i = \ s -> case s of 
                        "int" -> [(AT_INT, "")] 
                        "str" -> [(AT_STR, "")] 
                        "bool" -> [(AT_BOOL, "")] 
                        "dec" -> [(AT_DEC, "")] 
                        "dbl" -> [(AT_DBL, "")] 
                        "nat" -> [(AT_NAT, "")] 
                        


data AValue =       AV_INT    Integer               --   int
                  | AV_STR    String                --   str
                  | AV_BLN    Bool                  --   bool
                  | AV_DBL    Double                --   dbl
                  | AV_DEC    Float 
                  | AV_NAT    Integer                -- nat
                  
                  deriving (Eq, Ord)
                  
instance Show AValue where
    show (AV_INT x)     = show x
    show (AV_STR x)     = show x
    show (AV_BLN True)  = "true"
    show (AV_BLN False) = "false"
    show (AV_DBL x)     =  show x
    show (AV_DEC x)     = showFFloat (Just 2) x ""
    show (AV_NAT x)     = show x
   


 
type ATypedValue = (AType, AValue)



data SortDirection =    Ascending
                    |   Descending
                    
                    deriving (Eq)
                    
instance Show SortDirection where
    show Ascending     = "asc"
    show Descending     = "desc"
    


data JoinComparisonKind =       TJ_EQ 
                              | TJ_GT 
                              | TJ_GE 
                              | TJ_LT 
                              | TJ_LE 
                              | TJ_NE
                              
                              deriving (Eq)

instance Show JoinComparisonKind where
    show TJ_EQ     = "="
    show TJ_GT     = ">"
    show TJ_GE     = ">="
    show TJ_LT     = "<"
    show TJ_LE     = "<="
    show TJ_NE     = "!="



data FunctionType1TO1 =   FT1TO1_ADD
                        | FT1TO1_SUBTRACT
                        | FT1TO1_MULTIPLY
                        | FT1TO1_DIVIDE
                        | FT1TO1_MODULO
                        
                        deriving (Eq,  Show)
                        
                        
data FunctionTypeAGGR =   FTAGGR_AVG
                        | FTAGGR_MAX
                        | FTAGGR_MIN
                        | FTAGGR_SUM
                        
                        deriving (Eq, Show)



type TBLName                =   String              
type AttrName               =   String              
type ResultAttrName         =   AttrName
type PartitioningAttrName   =   AttrName
type SelectionAttrName      =   AttrName
type SortAttrName           =   AttrName
type NewAttrName            =   AttrName
type OldAttrName            =   AttrName
type LeftAttrName           =   AttrName
type RightAttrName          =   AttrName



type SortInfos              =   [(SortAttrName, SortDirection)]
type ProjInfos              =   [(NewAttrName, OldAttrName)]       
type JoinPredicates         =   [(JoinComparisonKind, (LeftAttrName,RightAttrName))]


type Tuple = [AValue]              
                                  -- (external attribute name, internal attribute name, attribute type)
type TBLAttributeInfos           =  [(AttrName, AttrName, AType)]
type KeyInfos                    =  [[AttrName]]

type SemanticalInfosROWNUM      =   (ResultAttrName,  SortInfos,  Maybe PartitioningAttrName) 
type SemanticalInfosROWID       =   ResultAttrName
type SemanticalInfosRANK        =   (ResultAttrName,  SortInfos)
type SemanticalInfosPROJ        =   ProjInfos
type SemanticalInfosSEL         =   SelectionAttrName
type SemanticalInfosPOS_SELECT =    (Int,  SortInfos,  Maybe PartitioningAttrName) 
type SemanticalInfosEQJOIN      =   (LeftAttrName,RightAttrName)
type SemanticalInfosTHETAJOIN   =   JoinPredicates 
type SemanticalInfosLIT_TBL     =   [Tuple]
type SemanticalInfosATTACH      =   (ResultAttrName, ATypedValue)
type SemanticalInfosCAST      =     (ResultAttrName, AttrName, AType)


type SemanticalInfosUNOP         =   (ResultAttrName, AttrName)   
type SemanticalInfosBINOP        =  (ResultAttrName, (LeftAttrName,RightAttrName))   
type SemanticalInfosFUN_1TO1     =  (FunctionType1TO1, ResultAttrName, [AttrName])   
type SemanticalInfosFUN_AGGR    =   (FunctionTypeAGGR, SemanticalInfosUNOP, Maybe PartitioningAttrName)
type SemanticalInfosFUN_AGGR_COUNT    =   (ResultAttrName, Maybe PartitioningAttrName)

 

type SemanticalInfosSERIALIZE_REL  
                                =   (AttrName, AttrName, [AttrName])   

type SemanticalInfosTABLEREF    =   (TBLName, TBLAttributeInfos, KeyInfos)

      
type SchemaInfos = [(AttrName, AType)]      





data AlgbExp =      ROWNUM          SemanticalInfosROWNUM                       AlgbExp
                |   ROWID           SemanticalInfosROWID                        AlgbExp
                |   ROWRANK         SemanticalInfosRANK                         AlgbExp
                |   RANK            SemanticalInfosRANK                         AlgbExp
                
                
                |   PROJ            SemanticalInfosPROJ                         AlgbExp
                |   SEL             SemanticalInfosSEL                          AlgbExp
                |   POS_SELECT      SemanticalInfosPOS_SELECT                   AlgbExp 
                
                |   CROSS                                                       AlgbExp  AlgbExp
                |   EQJOIN          SemanticalInfosEQJOIN                       AlgbExp  AlgbExp
                |   SEMIJOIN        SemanticalInfosEQJOIN                       AlgbExp  AlgbExp
                |   THETAJOIN       SemanticalInfosTHETAJOIN                    AlgbExp  AlgbExp
                
                |   DISJUNION                                                   AlgbExp  AlgbExp  
                |   DIFFERENCE                                                  AlgbExp  AlgbExp  
                
                |   DISTINCT                                                    AlgbExp
                
                |   LIT_TBL         SemanticalInfosLIT_TBL      SchemaInfos         
                |   EMPTY_TBL                                   SchemaInfos           
                
                |   TABLEREF        SemanticalInfosTABLEREF           
                
                |   ATTACH          SemanticalInfosATTACH                       AlgbExp           
                
                |   CAST            SemanticalInfosCAST                         AlgbExp           
                
                
                |   FUN_NUM_EQ      SemanticalInfosBINOP                          AlgbExp  
                |   FUN_NUM_GT      SemanticalInfosBINOP                          AlgbExp  
                
                |   FUN_1TO1        SemanticalInfosFUN_1TO1                          AlgbExp  

                |   FUN_BOOL_AND    SemanticalInfosBINOP                          AlgbExp 
                |   FUN_BOOL_OR     SemanticalInfosBINOP                          AlgbExp 
                |   FUN_BOOL_NOT    SemanticalInfosUNOP                          AlgbExp 
                
                |   FUN_AGGR        SemanticalInfosFUN_AGGR                      AlgbExp 
                |   FUN_AGGR_COUNT  SemanticalInfosFUN_AGGR_COUNT                AlgbExp 
                
                
                |   SERIALIZE_REL   SemanticalInfosSERIALIZE_REL                AlgbExp AlgbExp
                
                |   NIL
                
                |   ADAGNodeID Int
                
                
                deriving (Eq, Show)



  