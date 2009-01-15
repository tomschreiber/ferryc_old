module Lang where


import LangCommon


data ForClause 
    =   Binding [(VarID, Expr)]
    |   Where Expr
    |   GroupBy [Expr]
    |   OrderBy [(Expr, Maybe SortDirection)]
    |   Return Expr     
      

       deriving (Show, Eq)





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
  
  | Let [(VarID, Expr)] Expr
  
  | Var VarID
  
  | If Expr Expr (Maybe Expr)
  
  | For [ForClause]
  

     
  | Table TblID [(ATRName, ATRType)] [[ATRName]] [(ATRName, Maybe SortDirection)]    

  | NomAcc Expr String  
  
  
  
  | FunApp FunID FunArgs

  | Parenthesed Expr  
  
  deriving (Show, Eq)
  
  


