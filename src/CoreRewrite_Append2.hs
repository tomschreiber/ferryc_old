-----------------------------------------------------------------------------------------
{-| Module      : Annotation
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module CoreRewrite_Append2 (

-- =============================================================================
-- exports
-- =============================================================================

rewrite

) where


-- =============================================================================
-- imports
-- =============================================================================

import LangCore





-- =============================================================================
-- functions
-- =============================================================================


----------------------------------------------------------------------
----------------------------------------------------------------------
rewrite :: Expr -> Expr
----------------------------------------------------------------------
----------------------------------------------------------------------

rewrite e = case e of
                                  
     
    Literal literalExpr -> Literal literalExpr
      
    OpApp (UnOp opID e) -> OpApp (UnOp opID (rewrite e))   
    OpApp (BinOp opID e1 e2) -> OpApp (BinOp opID (rewrite e1) (rewrite e2))   
    
    Tuple exprs        -> Tuple (map rewrite exprs)
    
    PosAcc e i         ->  PosAcc (rewrite e) i
    
    List []            ->  List []
    List [e]           ->  List [rewrite e]
    List elems         ->  error("oops")
   
                
    Let v e1 e2         ->  Let v (rewrite e1) (rewrite e2) 

    Var v               -> Var v

    If e1 e2 maybeE3         ->  If (rewrite e1) (rewrite e2) maybeE3'
        where
            maybeE3' = case maybeE3 of
                        Nothing -> Nothing
                        Just e3 -> Just (rewrite e3)
    
    For v e1 e2 maybeForOrderByClause -> For v (rewrite e1) (rewrite e2) maybeForOrderByClause'
        where
            maybeForOrderByClause' = case maybeForOrderByClause of
                        Nothing -> Nothing
                        Just clauses -> Just ( zip (map rewrite (map fst clauses)) (map snd clauses) )
                           
            
    Table tblID tableAttSpecs tableKeySpecs tableOrderSpecs ->  Table tblID tableAttSpecs tableKeySpecs tableOrderSpecs         
    
    
    FunApp "append" (Exprs [FunApp "append" (Exprs [e1, e2]) , e3]) -> 
            rewrite (FunApp "append" (Exprs [e1, FunApp "append" (Exprs [e2, e3])]))
    
    FunApp "append" (Exprs [e1, e2]) -> 
            FunApp "append" (Exprs [rewrite e1, rewrite e2])
    
    FunApp funID (Exprs exprs) -> FunApp funID (Exprs (map rewrite exprs))
    
    FunApp funID (InlineLambda ((varID, e1), e2)) -> FunApp funID (InlineLambda ((varID, rewrite e1), rewrite e2))

    Parenthesed e   -> Parenthesed (rewrite e) 
    
    