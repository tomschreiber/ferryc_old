-----------------------------------------------------------------------------------------
{-| Module      : Annotation
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module CoreInference_EmptySubLists (

-- =============================================================================
-- exports
-- =============================================================================

infere, VarEnv

) where


-- =============================================================================
-- imports
-- =============================================================================

import LangCore
import LangCommon




-- =============================================================================
-- functions
-- =============================================================================

type VarEnv = [(VarID, Bool)]


----------------------------------------------------------------------
----------------------------------------------------------------------
infere :: VarEnv -> Expr -> Bool
----------------------------------------------------------------------
----------------------------------------------------------------------


infere varEnv e = case e of
                                  
     
    Literal literalExpr -> False
      
    OpApp opAppExpr -> False   
    
    Tuple exprs        -> or (map (infere varEnv) exprs)
                          
    
    
    PosAcc e i         ->  infere varEnv e
    
    List []             ->   True 
    List exprs          -> or (map (infere varEnv) exprs)
    
                
    Let v e1 e2         ->    
                            let
                                e1HasEmptySubLists =  infere varEnv e1
                            in
                                infere ([(v, e1HasEmptySubLists)] ++ varEnv) e2
 

    Var v               ->  case lookup v varEnv of 
                                            Just bln    -> bln
                                            Nothing     -> error ("var " ++ v ++ " unknown")


                                  
    If e1 e2 maybeE3         ->  or [infere varEnv e2, maybeE3']
                                    where
                                            maybeE3' = case maybeE3 of 
                                                            Nothing -> False
                                                            Just e3 -> infere varEnv e3
                                         
                                

    
    For v e1 e2 maybeForOrderByClause -> let
                                            e1HasEmptySubLists =  infere varEnv e1
                                         in
                                            infere ([(v, e1HasEmptySubLists)] ++ varEnv) e2
    
            
    Table tblID tableAttSpecs tableKeySpecs tableOrderSpecs ->  False         
    
    
    FunApp "append"   (Exprs [e1, e2])       ->  or [infere varEnv e1, infere varEnv e2] 
    FunApp "appendSF" (Exprs [_,e1, e2])     ->   or [infere varEnv e1, infere varEnv e2] 
    FunApp "appendS"  (Exprs [e1, e2])       ->  or [infere varEnv e1, infere varEnv e2] 
    FunApp "appendST" (Exprs [e])            ->  infere varEnv e
    
    FunApp "box" (Exprs [e])                 ->  infere varEnv e
    FunApp "unbox" (Exprs [e])                 ->  infere varEnv e
    
    
    FunApp funID funArgs                     ->  True
    
    
    Parenthesed e   -> infere varEnv e 
    
    