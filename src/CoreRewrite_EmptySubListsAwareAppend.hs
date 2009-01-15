-----------------------------------------------------------------------------------------
{-| Module      : Annotation
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module CoreRewrite_EmptySubListsAwareAppend (

-- =============================================================================
-- exports
-- =============================================================================

rewrite

) where


-- =============================================================================
-- imports
-- =============================================================================

import LangCore

import CoreInference_EmptySubLists(infere, VarEnv)




-- =============================================================================
-- functions
-- =============================================================================


----------------------------------------------------------------------
----------------------------------------------------------------------
rewrite :: Expr -> Expr
----------------------------------------------------------------------
----------------------------------------------------------------------
rewrite e = _rewrite [] e



_rewrite varEnv e = case e of
                                  
     
    Literal literalExpr -> Literal literalExpr
      
    OpApp (UnOp opID e) -> OpApp (UnOp opID (_rewrite varEnv e))   
    OpApp (BinOp opID e1 e2) -> OpApp (BinOp opID (_rewrite varEnv e1) (_rewrite varEnv e2))   
    
    
    Tuple exprs        -> Tuple (map (_rewrite varEnv) exprs)
    
    PosAcc e i         ->  PosAcc (_rewrite varEnv e) i
    
    List []             ->   List [] 
    List [e]             ->  List [_rewrite varEnv e]
    List (e:tl)          ->  error("oops")
                
    Let v e1 e2         ->  let
                                e1HasEmptySubLists =  infere varEnv e1
                                e1' = _rewrite  varEnv e1
                                e2' = _rewrite  ([(v, e1HasEmptySubLists)] ++ varEnv) e2
                            in
                                Let v e1' e2' 
                                 

    Var v               -> Var v

    If e1 e2 maybeE3         ->  If (_rewrite varEnv e1) (_rewrite varEnv e2) maybeE3'
        where
            maybeE3' = case maybeE3 of
                        Nothing -> Nothing
                        Just e3 -> Just (_rewrite varEnv e3)
    
    
    For v e1 e2 maybeForOrderByClause ->    let
                                                e1HasEmptySubLists =  infere varEnv e1
                                                e1' = _rewrite  varEnv e1
                                                varEnv' = ([(v, e1HasEmptySubLists)] ++ varEnv)
                                                e2' = _rewrite  varEnv' e2
                                                maybeForOrderByClause' = case maybeForOrderByClause of
                                                                    Nothing -> Nothing
                                                                    Just clauses -> Just ( zip (map (_rewrite varEnv') (map fst clauses)) (map snd clauses) ) 
                                            in
                                                For v e1' e2' maybeForOrderByClause'
                                                    
                                                        
                                                        
            
    Table tblID tableAttSpecs tableKeySpecs tableOrderSpecs ->  Table tblID tableAttSpecs tableKeySpecs tableOrderSpecs         
    
    
    FunApp "appendSF" (Exprs [_noEmptySubLists, e1, e2])    ->  
                                    let 
                                        e1HasEmptySubLists =  infere varEnv e1
                                        e2HasEmptySubLists =  infere varEnv e2
                                        e1_or_e2HasEmptySubLists = or [e1HasEmptySubLists, e2HasEmptySubLists]
                                        
                                        e1' = _rewrite varEnv e1
                                        e2' = _rewrite varEnv e2
                                    in
                                        FunApp "appendSF" (Exprs [Literal (BoolLiteral (e1_or_e2HasEmptySubLists)), e1', e2'])                                     
                       
    
    
    FunApp funID (Exprs exprs) -> FunApp funID (Exprs (map (_rewrite varEnv) exprs))
    
    
    
    FunApp funID (InlineLambda ((varID, e1), e2)) -> 
                                                
        let
            e2HasEmptySubLists =  infere varEnv e2
            e2' = _rewrite varEnv e2
            e1' = _rewrite  ([(varID, e2HasEmptySubLists)] ++ varEnv) e1
        in
            FunApp funID (InlineLambda ((varID, e1'), e2'))
    
    
    Parenthesed e   -> Parenthesed (_rewrite varEnv e) 
    
    