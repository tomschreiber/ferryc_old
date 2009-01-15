-----------------------------------------------------------------------------------------
{-| Module      : LangCore
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module LangCore where


import LangCommon


type ForOrderByClause 
    = [(Expr, SortDirection)]
      





data LiteralExpr =  IntLiteral Integer
                  | StringLiteral String
                  | BoolLiteral Bool
                deriving (Show, Eq) 

data OpAppExpr =  UnOp String Expr 
                | BinOp String Expr Expr 
                 deriving (Show, Eq)
    

  
data FunArgs =    Exprs [Expr]
                | InlineLambda ((VarID, Expr), Expr)
                
                 deriving (Show, Eq)
                 
                 
data Expr
  = 
    Literal LiteralExpr
  
  | OpApp OpAppExpr 
  
  | Tuple [Expr]
  
  | PosAcc Expr Int
  
  
  | List [Expr]
  
  | Let VarID Expr Expr
  
  | Var VarID
  
  | If Expr Expr (Maybe Expr)
 
  
  | For VarID Expr Expr (Maybe ForOrderByClause)  
  

                 
  | Table TblID [(ATRName, ATRType)] [[ATRName]] [(ATRName, SortDirection)]     
  
  
  
  | FunApp FunID FunArgs

  | Parenthesed Expr  
  
  deriving (Show, Eq)
  
  

