-----------------------------------------------------------------------------------------
{-| Module      : TypesIsd
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module TypeChecker_RT_GT where

-- =============================================================================
-- imports
-- =============================================================================

import LangCommon
import LangCore



-- =============================================================================
-- types and data
-- =============================================================================


data TypeRTSD = RT_Tuple | RT_Table deriving (Eq, Show)       



type FunSigTypeRTSD = ([TypeRTSD],TypeRTSD)              -- (stripped down) Implementation-Types  Function-Signature
type FunSigTypeRTSDEnv  = [(VarID, FunSigTypeRTSD)] -- (stripped down) Implementation-Types  Function-Signature Environment

type VarEnv = [(VarID, TypeRTSD)]


defaultFunSigTypeRTSDEnv :: FunSigTypeRTSDEnv
defaultFunSigTypeRTSDEnv = [
                            ("unordered", ([RT_Table],RT_Table)),
                            
                            
                            
                            ("append", ([RT_Table, RT_Table],RT_Table)),
                            ("appendSF", ([RT_Tuple, RT_Table, RT_Table],RT_Table)),
                            ("appendS", ([RT_Table, RT_Table],RT_Table)),
                            ("appendST", ([RT_Table],RT_Table)),
                            ("concat", ([RT_Table],RT_Table)),
                            
                            ("box", ([RT_Table],RT_Tuple)),
                            ("unbox", ([RT_Tuple],RT_Table)),
                            
                            
                            ("head", ([RT_Table],RT_Tuple)),
                            ("head2", ([RT_Table],RT_Tuple)),
                            ("tail", ([RT_Table],RT_Table)),
                            ("nth", ([RT_Tuple, RT_Table],RT_Tuple)),
                            ("ul", ([RT_Table],RT_Tuple)),
                            ("take", ([RT_Tuple, RT_Table],RT_Table)),
                            ("drop", ([RT_Tuple, RT_Table],RT_Table)),
                            
                            ("empty", ([RT_Table],RT_Tuple)),
                          	("all",   ([RT_Table],RT_Tuple)),
                          	("any",   ([RT_Table],RT_Tuple)),
						  
                            ("distinctValues", ([RT_Table],RT_Table)),
                            ("the", ([RT_Table],RT_Tuple)),
                            
                            --("zipn", ([RT_Tuple],RT_Table)),
                            ("zip2", ([RT_Table,RT_Table],RT_Table)),
                            --("unzipn", ([RT_Table],RT_Tuple)),
                            ("unzip", ([RT_Table],RT_Tuple)),
                            
                            
                            ("groupWith",   ([RT_Tuple,RT_Tuple,RT_Table],RT_Table)),
                            ("groupWith2",  ([RT_Tuple,RT_Tuple,RT_Table],RT_Table)),
                            ("groupBy",     ([RT_Tuple,RT_Tuple,RT_Table],RT_Table)),
                            ("partition",   ([RT_Tuple,RT_Tuple,RT_Table],RT_Tuple)),
                            
                            ("avg", ([RT_Table],RT_Tuple)),
                            ("avg_", ([RT_Table],RT_Tuple)),
                            ("max", ([RT_Table],RT_Tuple)),
                            ("max_", ([RT_Table],RT_Tuple)),
                            ("min", ([RT_Table],RT_Tuple)),
                            ("min_", ([RT_Table],RT_Tuple)),
                            ("sum", ([RT_Table],RT_Tuple)),
                            ("sum_", ([RT_Table],RT_Tuple)),
                            
                            ("count", ([RT_Table],RT_Tuple)),
                            ("count_", ([RT_Table],RT_Tuple))
                        ]


-- =============================================================================
-- functions
-- =============================================================================

----------------------------------------------------------------------
----------------------------------------------------------------------
itypeSD :: Expr -> TypeRTSD
----------------------------------------------------------------------
----------------------------------------------------------------------

itypeSD e = _itypeSD [] defaultFunSigTypeRTSDEnv e
    
    
    
----------------------------------------------------------------------
----------------------------------------------------------------------
_itypeSD :: VarEnv -> FunSigTypeRTSDEnv -> Expr -> TypeRTSD
----------------------------------------------------------------------
----------------------------------------------------------------------

_itypeSD varEnv fsEnv e = case e of 
        
        Literal _ -> RT_Tuple
        
        OpApp _   -> RT_Tuple
        
        Tuple _   -> RT_Tuple
        
        PosAcc _ _   -> RT_Tuple
        
        List _    -> RT_Table
        
        
        Let varID e1 e2        -> 
                              let 
                                t1 = _itypeSD varEnv fsEnv e1
                              in
                                _itypeSD ([(varID, t1)] ++ varEnv) fsEnv e2 
        
        
        
        Var varID               -> 
                                 case lookup varID varEnv of 
                                            Just sig    -> sig
                                            Nothing     -> error ("variable " ++ varID ++ " unknown")
        
       

        
        If _e1 e2 _maybeE3       -> _itypeSD varEnv fsEnv e2                      
        
        For _varID _e1 _e2 _maybeForOrderByClause     -> RT_Table
   
        
        Table _ _ _ _        -> RT_Table
        
       
        
        
        
        FunApp funID (Exprs _es)        -> let 
                                            (_, rt) = case lookup funID fsEnv of 
                                                        Just sig    -> sig
                                                        Nothing     -> error ("function " ++ funID ++ " unknown")
                                         in 
                                            rt   
                               


        FunApp funID (InlineLambda ((_varID, _e1), _e2))        -> 
                                        let 
                                            (_, rt) = case lookup funID fsEnv of 
                                                        Just sig    -> sig
                                                        Nothing     -> error ("function " ++ funID ++ " unknown")
                                        in 
                                            rt           
                                            
                                            
        Parenthesed e -> _itypeSD varEnv fsEnv e