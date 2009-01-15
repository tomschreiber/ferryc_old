-----------------------------------------------------------------------------------------
{-| Module      : Annotation
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Annotater where


-- =============================================================================
-- imports
-- =============================================================================

import LangCore
import TypeChecker_RT_GT




-- =============================================================================
-- functions
-- =============================================================================


----------------------------------------------------------------------
----------------------------------------------------------------------
annotate :: Expr -> Expr
----------------------------------------------------------------------
----------------------------------------------------------------------

annotate e = _annotate [] defaultFunSigTypeRTSDEnv e
    
    where
    
    ----------------------------------------------------------------------
    ----------------------------------------------------------------------
    _annotate :: VarEnv -> FunSigTypeRTSDEnv -> Expr -> Expr
    ----------------------------------------------------------------------
    ----------------------------------------------------------------------
    _annotate varEnv fsEnv e = case e of
             
             
                Literal literalExpr ->  Literal literalExpr
                
                  
                OpApp (UnOp name e) -> OpApp (UnOp name (_annotate varEnv fsEnv e))                   
                OpApp (BinOp name e1 e2) -> OpApp (BinOp name (_annotate varEnv fsEnv e1) (_annotate varEnv fsEnv e2))                  
                
                
                
                Tuple exprs       ->
                                      let
                                        types = map (_itypeSD varEnv fsEnv) exprs
                                        exprs' = map (_annotate varEnv fsEnv) exprs
                                        exprs'' = map (\(fun, e) -> fun e) (zip (map (ensureTypeConformance RT_Tuple) types) exprs')
                                      in
                                        Tuple exprs''  

                
                
                PosAcc e i  -> PosAcc (_annotate varEnv fsEnv e)  i
                
                
                
                List exprs       ->
                                      let
                                        types = map (_itypeSD varEnv fsEnv) exprs
                                        exprs' = map (_annotate varEnv fsEnv) exprs
                                        exprs'' = map (\(fun, e) -> fun e) (zip (map (ensureTypeConformance RT_Tuple) types) exprs')
                                      in
                                        List exprs''  
                
                
                Let v e1 e2 ->      let 
                                        t1 = _itypeSD varEnv fsEnv e1
                                        e1' = _annotate varEnv fsEnv e1  
                                        e2' = _annotate ([(v, t1)] ++ varEnv) fsEnv e2
                                    in  
                                        Let v e1' e2'    
                
                
                Var varID   ->  Var varID 
                
                If e1 e2 maybeE3 ->     let e1' = _annotate varEnv fsEnv e1  
                                            e2' = _annotate varEnv fsEnv e2
                                            maybeE3' = case maybeE3 of  
                                                        Just e3 -> Just (_annotate varEnv fsEnv e3)
                                                        _       -> Nothing 
                                        in  If e1' e2' maybeE3'       
                
               
                
                           
               
                
                For v e1 e2 Nothing ->  
                                        let t1 = _itypeSD varEnv fsEnv e1
                                            varEnv' = ([(v, RT_Tuple)] ++ varEnv)
                                            t2 = _itypeSD varEnv' fsEnv e2
                                            e1' = _annotate varEnv fsEnv e1
                                            e2' = _annotate varEnv'  fsEnv e2
                                            e1'' = ensureTypeConformance RT_Table t1 e1'
                                            e2'' = ensureTypeConformance RT_Tuple t2 e2'
                                        in  For v e1'' e2'' Nothing 


                For v e1 e2 (Just orderBySpecs) ->  
                                        let t1 = _itypeSD varEnv fsEnv e1
                                            varEnv' = ([(v, RT_Tuple)] ++ varEnv)
                                            t2 = _itypeSD varEnv' fsEnv e2
                                            e1' = _annotate varEnv fsEnv e1
                                            e2' = _annotate varEnv'  fsEnv e2
                                            e1'' = ensureTypeConformance RT_Table t1 e1'
                                            e2'' = ensureTypeConformance RT_Tuple t2 e2'
                                        
                                            orderByExps = map fst orderBySpecs 
                                            orderByDirs = map snd orderBySpecs 
                                            orderByExps' = map (_annotate varEnv' fsEnv) orderByExps  
                                            orderBySpecs' = zip orderByExps' orderByDirs
                                        
                                        in  For v e1'' e2'' (Just orderBySpecs') 
                                
                 
                 
                Table tblID tableAttSpecs tableKeySpecs tableOrderSpecs -> Table tblID tableAttSpecs tableKeySpecs tableOrderSpecs  
                
                
                
                FunApp funID (Exprs es) ->  FunApp funID (Exprs es'')
                    where
                    
                        (typesExpected, _) = case lookup funID fsEnv of 
                                                Just sig    -> sig
                                                Nothing     -> error ("function " ++ funID ++ " unknown")
                        
                        
                        typesAvailable = map (_itypeSD varEnv fsEnv) es
                        es' = map (_annotate varEnv fsEnv) es
                        es'' = map (\(f, e) -> f e) (zip (map (\(f,t) -> f t) (zip (map ensureTypeConformance typesExpected) typesAvailable)) es')
                        
                
                FunApp funID (InlineLambda ((varID, e1), e2)) ->  FunApp funID (InlineLambda ((varID, e1'), e2''))
                    where
                    
                        t2 = _itypeSD varEnv fsEnv e2
                        e1' = _annotate ([(varID, RT_Tuple)] ++ varEnv) fsEnv e1
                        e2' = _annotate varEnv fsEnv e2
                        e2'' = ensureTypeConformance RT_Table t2 e2'



    
                Parenthesed e -> Parenthesed (_annotate varEnv fsEnv e)

----------------------------------------------------------------------
----------------------------------------------------------------------
                      -- expected available                        
ensureTypeConformance :: TypeRTSD -> TypeRTSD -> Expr -> Expr
----------------------------------------------------------------------
----------------------------------------------------------------------

ensureTypeConformance t1 t2 e = case (t1, t2) of 
                                    (RT_Tuple, RT_Table) -> (FunApp "box" (Exprs [e]))
                                    (RT_Table, RT_Tuple) -> (FunApp "unbox" (Exprs [e]))
                                    _              -> e
                                  