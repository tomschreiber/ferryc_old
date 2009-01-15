{-# OPTIONS_GHC -fglasgow-exts #-}

-----------------------------------------------------------------------------------------
{-| Module      : Core2Algb
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Core2Algb(

-- =============================================================================
-- exports
-- =============================================================================
    TableInfos (..),
    AttrName,
    Subs, 
    compile,
    wrapWithSerializeOps, removeSerializeOps

) where





-- =============================================================================
-- imports
-- =============================================================================

import LangCommon
import LangCore
import LangAlgb(AttrName, JoinComparisonKind (..), FunctionType1TO1 (..), FunctionTypeAGGR (..), AlgbExp (..), AValue (..), SortDirection (..), AType (..))

import Core2AlgbUtils(  
                        cid, cid2, ord,
                        fuse1,
                        incr2, incr3, decr2, decr3,  
                        asProjList1, asProjList2, asProjList3,
                        asSortList1
                     )

import Utils.AssociationList(keys, diff, retainByKeys) 


import Data.List (zip4)





-- =============================================================================
-- types and data
-- =============================================================================

data TableInfos     =  TI (AlgbExp, [AttrName], [(AttrName, TableInfos)])
                    |  TIDAGNodeID Int
                     deriving (Show)

type Subs  =  [(AttrName, TableInfos)]
type Cols  =  [AttrName]



type Env  = [(VarID, TableInfos)]  -- variable environment



-- =============================================================================
-- common auxiliary-functions
-- =============================================================================


----------------------------------------------------------------------
----------------------------------------------------------------------
suap ::  AlgbExp -> Subs -> Subs -> Subs
----------------------------------------------------------------------
----------------------------------------------------------------------


----------------------------------------------------------------------
-- (SUAP 1)
suap q_paap ((c, TI (q_1, cols1, subs_1)):subs_hat) ((_, TI (q_2, cols2, subs_2)):subs_tilde) = [(c, TI (q', cols, subs'))] ++ subs''
    where
        
        
        cols = fuse1 cols1 cols2  
       
        q = 
            ROWNUM ("item'",  [("iter", LangAlgb.Ascending), ("ord", LangAlgb.Ascending), ("pos", LangAlgb.Ascending)],  Nothing)
            (  
               
                      DISJUNION
                        (
                            
                            ATTACH ("ord", (AT_NAT, AV_NAT 1))
                            (
                                q_1
                            )
                        )
                        (
                            ATTACH ("ord", (AT_NAT, AV_NAT 2))
                            (
                                q_2
                            )
                        )
                
            )
        
        
        q' =
            PROJ  ([("iter","item''"), ("pos","pos")] ++ (asProjList1 (diff cols (keys subs_1))) ++ (asProjList2 (keys subs_1) "item'")) 
            (
	            
	            
	            THETAJOIN [(TJ_EQ, ("ord","ord'")), (TJ_EQ, ("iter",c++"'"))] 
	                    (
	                        q
	                    )
	                    (
	                        PROJ  [("ord'","ord"), ("item''","item'"), (c++"'",c)]
                            (
	                            q_paap
                            )
	                    )
	            
	            
	            
	            {-
	            SEL "res"
                (
	                FUN_NUM_EQ ("res", ("ord","ord'"))
                    (
	                    EQJOIN ("iter",c++"'") 
	                    (
	                        q
	                    )
	                    (
	                        PROJ  [("ord'","ord"), ("item''","item'"), (c++"'",c)]
                            (
	                            q_paap
                            )
	                    )
                    )
                )
                -}
                
            )
              
        
        
        {- 
        q' =
            PROJ  ([("iter","item''"), ("pos","pos")] ++ (asProjList1 (diff cols (keys subs_1))) ++ (asProjList2 (keys subs_1) "item'")) 
            (
                ROWRANK ("item''",  [("ord", LangAlgb.Ascending), ("iter", Ascending)])
                (
                    q
                )
            )
        -}
        
        subs' = 
            suap q subs_1 subs_2
           
        
        subs'' = suap q_paap subs_hat subs_tilde




         
----------------------------------------------------------------------
-- (SUAP 2)
suap _ [] [] = []

----------------------------------------------------------------------
-- (SUAP 3)
suap _ subs [] = subs

----------------------------------------------------------------------
-- (SUAP 4)
suap _ [] subs = subs




----------------------------------------------------------------------
----------------------------------------------------------------------
suapSF ::  Bool -> AlgbExp -> Subs -> Subs -> Subs
----------------------------------------------------------------------
----------------------------------------------------------------------



----------------------------------------------------------------------
-- (SUAPSF 1)
suapSF emptySubLists q_paap ((c, TI (q_1, cols1, subs_1)):subs_hat) ((_, TI (q_2, cols2, subs_2)):subs_tilde) = [(c, TI (q', cols, subs'))] ++ subs''
    where
       
       
        cols = fuse1 cols1 cols2  
       
        q = 
            ROWNUM ("item'",  [("iter", LangAlgb.Ascending), ("ord", LangAlgb.Ascending), ("pos", LangAlgb.Ascending)],  Nothing)
            (
                
                

                        DISJUNION
                        (
                            
                            ATTACH ("ord", (AT_NAT, AV_NAT 1))
                            (
	                            q_1
                            )
                        )
                        (
                            q_2
                        )
                  
            )
        
        
        q' =
            
            if emptySubLists
                
                then
            
            
                    PROJ  ([("iter","item''"), ("pos","pos")] ++ (asProjList1 (diff cols (keys subs_1))) ++ (asProjList2 (keys subs_1) "item'")) 
                    (
        	            
        	            
                        THETAJOIN [(TJ_EQ, ("ord","ord'")), (TJ_EQ, ("iter",c++"'"))] 
                        (
                            q
                        )
                        (
                            PROJ  [("ord'","ord"), ("item''","item'"), (c++"'",c)]
                            (
                                q_paap
                            )
                        )
        	            
        	            
        	            
	                    {-
	                    SEL "res"
                        (
	                        FUN_NUM_EQ ("res", ("ord","ord'"))
                            (
	                            EQJOIN ("iter",c++"'") 
	                            (
	                                q
	                            )
	                            (
	                                PROJ  [("ord'","ord"), ("item''","item'"), (c++"'",c)]
                                    (
	                                    q_paap
                                    )
	                            )
                            )
                        )
                        -}
                        
                    )
              
                else
        
        
                    PROJ  ([("iter","item''"), ("pos","pos")] ++ (asProjList1 (diff cols (keys subs_1))) ++ (asProjList2 (keys subs_1) "item'")) 
                    (
                        ROWRANK ("item''",  [("ord", LangAlgb.Ascending), ("iter", LangAlgb.Ascending)])
                        (
                            q
                        )
                    )
        
        
        
        
        subs' = 
            suapSF emptySubLists q subs_1 subs_2 
          
          
        subs'' = suapSF emptySubLists q_paap subs_hat subs_tilde




         
----------------------------------------------------------------------
-- (SUAPSF 2)
suapSF _ _ [] [] = []
suapSF _ _ subs [] = subs
suapSF _ _ [] subs = subs






----------------------------------------------------------------------
----------------------------------------------------------------------
suapS ::  Integer -> Subs -> Subs -> Subs
----------------------------------------------------------------------
----------------------------------------------------------------------


----------------------------------------------------------------------
-- (suapS 1)
suapS n ((c, TI (q_1, cols1, subs_1)):subs_hat) ((_, TI (q_2, cols2, subs_2)):subs_tilde) = [(c, TI (q', cols, subs'))] ++ subs''
    where
        
      
        cols = fuse1 cols1 cols2 
        
        q' = 
            
            
                        DISJUNION
                        (
                            
                            ATTACH ("ord", (AT_NAT, AV_NAT n))
                            (
                                q_1
                            )
                        )
                        (
                            q_2
                        )
              
        
        subs' =
            suapS n subs_1 subs_2 
           
        
        subs'' = suapS n subs_hat subs_tilde




         
----------------------------------------------------------------------
-- (suapS 2)
suapS _ [] [] = []
suapS _ subs [] = subs
suapS _ [] subs = subs



----------------------------------------------------------------------
----------------------------------------------------------------------
suapST ::  Integer -> Subs -> Subs
----------------------------------------------------------------------
----------------------------------------------------------------------


----------------------------------------------------------------------
-- (suapST 1)
suapST n ((c, TI (q_1, cols, subs_1)):subs_hat) = [(c, TI (q', cols, subs'))] ++ subs''
    where
        
        q' = 
                    ATTACH ("ord", (AT_NAT, AV_NAT n))
                    (
                        q_1
                    )
            

            
           
        
        
        
        
        subs' = 
            suapST n subs_1
            
        subs'' = suapST n subs_hat




         
----------------------------------------------------------------------
-- (suapST 2)
suapST _ [] = []









----------------------------------------------------------------------
----------------------------------------------------------------------
suse ::  AlgbExp -> Subs -> Subs
----------------------------------------------------------------------
----------------------------------------------------------------------

----------------------------------------------------------------------
-- (SUSE 1)
suse q_pase ((c, TI (q, cols, subs)):subs_hat) = [(c, TI (q', cols, subs'))] ++ subs''
    where
        q' =
            PROJ  ([("iter","iter"), ("pos","pos")] ++ (asProjList1 cols) )
            (
	            EQJOIN  ("iter", "iter'")
                (
	                q
                )
                (
	                PROJ  [("iter'",c)]
                    (
	                    q_pase
                    )
                )
            )
            
        subs' = suse q' subs
        
        subs'' = suse q_pase subs_hat  


----------------------------------------------------------------------
-- (SUSE 2)
suse _ [] = []












        

                
           
        





----------------------------------------------------------------------
----------------------------------------------------------------------
abspos ::  Cols -> AlgbExp -> AlgbExp
----------------------------------------------------------------------
----------------------------------------------------------------------

----------------------------------------------------------------------
-- (ABSPOS)
abspos cols q = q'
    where
        q' = 
          PROJ  ([("iter","iter"), ("pos","pos")] ++ (asProjList1 cols) )
		 (
            ROWNUM ("pos",  [("pos'", LangAlgb.Ascending)],  Just "iter")
            (
                PROJ  ([("iter","iter"), ("pos'","pos")] ++ (asProjList1 cols) )
                (
	                q
                )
                
            )
          )
        
        
        
        
        
        
        
----------------------------------------------------------------------
----------------------------------------------------------------------
omap ::  AlgbExp -> ([AlgbExp],Cols) -> AlgbExp
----------------------------------------------------------------------
----------------------------------------------------------------------

----------------------------------------------------------------------
-- omap 1
omap map (q:qs, c:cols) = map''
    where 
        map' = omap map (qs, cols)
        
        map'' =
        
                PROJ  ([("outer","outer"), ("inner","inner"), (c, cid 1)] ++ (asProjList1 cols))
                (
                    EQJOIN  ("inner", "iter")
                    (
	                    map'
                    )
                    (
	                    q
                    )	
                )
                
             
        
        
----------------------------------------------------------------------
-- omap 2        
omap map ([],[]) = map         
        




----------------------------------------------------------------------
----------------------------------------------------------------------
-- 
compileAppendSequence :: Integer -> Env -> AlgbExp -> Expr -> TableInfos
----------------------------------------------------------------------
----------------------------------------------------------------------


----------------------------------------------------------------------
-- (appendS)
compileAppendSequence n env loop (FunApp "appendS" (Exprs [e1, e2])) = TI (q, cols, subs')
    where
        
        TI (q_e1, cols, subs_e1) = _compile env loop e1   
        TI (q_e2,    _, subs_e2) = compileAppendSequence (n+1) env loop e2
        
        q =
            DISJUNION
            (
                ATTACH ("ord", (AT_NAT, AV_NAT n))
                (
                    q_e1
                )
            )
            (
                q_e2
            )
        
              
        
        
        subs' = suapS n subs_e1 subs_e2




----------------------------------------------------------------------
-- (appendST)
compileAppendSequence n env loop (FunApp "appendST" (Exprs [e])) = TI (q, cols_e, subs')
    where
        
        TI (q_e, cols_e, subs_e) = _compile env loop e   
       
        
        q =
            ATTACH ("ord", (AT_NAT, AV_NAT n))
            (
                q_e
            )
        
              
        
        
        subs' = suapST n subs_e






-- =============================================================================
-- Core -> Algebra compilation-functions
-- =============================================================================




loop_0 = LIT_TBL  [[AV_NAT 1]]  [("iter", AT_NAT)]
env_0  = []


----------------------------------------------------------------------
----------------------------------------------------------------------
-- Core -> Algebra
compile :: Expr -> TableInfos
----------------------------------------------------------------------
----------------------------------------------------------------------
compile e = _compile env_0 loop_0 e

    
    
----------------------------------------------------------------------
----------------------------------------------------------------------
--          var    loop    |- e       => (q, cols, subs)
_compile :: Env -> AlgbExp -> Expr -> TableInfos 
----------------------------------------------------------------------
----------------------------------------------------------------------


----------------------------------------------------------------------
-- (BOX)    
_compile env loop  (FunApp "box" (Exprs [e])) =  TI(q_box, [c], [(c, ti_e)])  
    where  
	
	    ti_e = _compile env loop e      
        
	    c = cid 1  
    	
	    q_box =
	        PROJ  [("iter","iter"), ("pos","pos"), (c,"iter")]
	        (   
	            ATTACH ("pos", (AT_NAT, AV_NAT 1))
	            (
                    loop
                )
	        )
		 
		 

----------------------------------------------------------------------
-- (UNBOX)
_compile env loop  (FunApp "unbox" (Exprs [e])) =  TI(q_unbox, cols_sub, subs_sub)  
--_compile env loop  (FunApp "unbox" (Exprs [e])) =  TI(q_sub, cols_sub, subs_sub)  
 
    where  
    	
	    TI (q_e, _, [(c, TI (q_sub, cols_sub, subs_sub))]) = _compile env loop e      

	   	
	    q_unbox =
		    PROJ ( [("iter","iter'"), ("pos","pos")] ++ (asProjList1 cols_sub) )
		    (
                EQJOIN  (c++"'", "iter")
                (
                    PROJ  [("iter'","iter"), (c++"'",c)]
                    (
                        q_e
                    )
                )
                (
                    q_sub
                )
		    )



            
            
        
        
	
	    
        
		    
	    
		 















----------------------------------------------------------------------
-- LiteralExpr 
_compile _env loop (Literal literalExpr) = TI (q, cols, subs)
    where
        
        c = cid(1)
        
        q =
            --PROJ  [("iter","iter"), ("pos","pos"), (c,c)]
            (
                ATTACH ("pos", (AT_NAT, AV_NAT 1))
                (
                    ATTACH (c, typeAndValue)
                    (
                        loop
                    )
                )
            ) 
            where
                typeAndValue = case literalExpr of 
                    IntLiteral v    ->  (AT_INT, AV_INT v)    
                    StringLiteral v ->  (AT_STR, AV_STR v)   
                    BoolLiteral v   ->  (AT_BOOL, AV_BLN v)
                    
                       
            
          
        cols = [c]
        
        subs = []  
        
        
   

        
           		
		

----------------------------------------------------------------------
-- OpAppExpr 1
_compile env loop  (OpApp (BinOp "+" e1 e2)) =  _compile_OperatorOnAtomicValues env loop (FunApp "+" (Exprs [e1, e2]))
_compile env loop  (OpApp (BinOp "-" e1 e2)) =  _compile_OperatorOnAtomicValues env loop (FunApp "-" (Exprs [e1, e2]))
_compile env loop  (OpApp (BinOp "*" e1 e2)) =  _compile_OperatorOnAtomicValues env loop (FunApp "*" (Exprs [e1, e2]))
_compile env loop  (OpApp (BinOp "/" e1 e2)) =  _compile_OperatorOnAtomicValues env loop (FunApp "/" (Exprs [e1, e2]))
_compile env loop  (OpApp (BinOp "%" e1 e2)) =  _compile_OperatorOnAtomicValues env loop (FunApp "%" (Exprs [e1, e2]))

_compile env loop  (OpApp (BinOp "==" e1 e2)) =  _compile_OperatorOnAtomicValues env loop (FunApp "==" (Exprs [e1, e2]))
_compile env loop  (OpApp (BinOp "!=" e1 e2)) =  _compile_OperatorOnAtomicValues env loop (FunApp "!=" (Exprs [e1, e2]))
_compile env loop  (OpApp (BinOp ">" e1 e2)) =  _compile_OperatorOnAtomicValues env loop (FunApp ">" (Exprs [e1, e2]))
_compile env loop  (OpApp (BinOp "<" e1 e2)) =  _compile_OperatorOnAtomicValues env loop (FunApp "<" (Exprs [e1, e2]))

_compile env loop  (OpApp (BinOp "and" e1 e2)) =  _compile_OperatorOnAtomicValues env loop (FunApp "and" (Exprs [e1, e2]))
_compile env loop  (OpApp (BinOp "or" e1 e2)) =  _compile_OperatorOnAtomicValues env loop (FunApp "or" (Exprs [e1, e2]))

----------------------------------------------------------------------
-- OpAppExpr 2
_compile env loop  (OpApp (UnOp "not" e)) =  _compile_OperatorOnAtomicValues env loop (FunApp "not" (Exprs [e]))






----------------------------------------------------------------------
-- TupleExpr 2
_compile env loop (Tuple [e]) = _compile env loop e 





----------------------------------------------------------------------
-- TupleExpr 1
_compile env loop (Tuple (e:es)) = TI (q', cols_e1 ++ cols'_e2, subs_e1 ++ subs'_e2)
    where
        
        TI (q_e1, cols_e1, subs_e1) = _compile env loop e
        TI (q_e2, cols_e2, subs_e2) = _compile env loop (Tuple es)
        
        cols'_e2 = incr2 cols_e2 (length cols_e1)
        subs'_e2 = incr3 subs_e2 (length cols_e1)
        
        
        q' =
            PROJ  ([("iter","iter"), ("pos","pos")] ++ asProjList1 cols_e1 ++ asProjList1 cols'_e2)
            (
                EQJOIN ("iter","iter'")
                    (
                        q_e1
                    )
                    (
                        PROJ  ([("iter'","iter")] ++ asProjList3 cols'_e2 cols_e2)
                        (
                            q_e2
                        )
                    )
            )
        
        


 
 
 
----------------------------------------------------------------------
-- PosAccExpr
_compile env loop (PosAcc e n) = TI (q', cols', subs')
    where
        
        TI (q, cols, subs) = _compile env loop e
        
        c_old = cid(n)
        c_new = cid(1)
        
        cols' = [c_new]
         
        subs' = decr3 (retainByKeys subs [c_old]) (n-1)
        
        q' =
            PROJ  [("iter","iter"), ("pos","pos"), (c_new,c_old)]
            (
                q
            ) 
            



----------------------------------------------------------------------
-- ListExpr 3
_compile env loop (List []) = TI (q, cols, subs)
    where
        
        cols = []
        
        subs = []

        q = 
            EMPTY_TBL  [("iter", AT_NAT), ("pos", AT_NAT)]

----------------------------------------------------------------------
-- ListExpr 2
_compile env loop (List [e]) = _compile env loop e
    

----------------------------------------------------------------------
-- ListExpr 1
_compile env loop (List (e:es)) = _compile env loop (FunApp "append" (Exprs [List [e], List es]))
        
       




----------------------------------------------------------------------
-- LetExpr    
_compile env loop  (Let v e1 e2) =  ti_2  
    where
        
        ti_1 = _compile env loop e1
        
        ti_2 = _compile  ([(v, ti_1)] ++ env) loop e2





----------------------------------------------------------------------
-- VarExpr
_compile env _loop (Var v) = ti
    where
    
        ti = case lookup v env of
                        Just t  -> t
                        Nothing -> error ("variable $" ++ v ++ " unknown")








----------------------------------------------------------------------
-- IfExpr 1
-- if e1 then e2 else ()
_compile env loop (If e1 e2 Nothing) = TI (q_e2, cols, subs_e2)
    where
        
        TI (q_e1, [c], []) = _compile env loop e1
        
        
        
        loop_then =
            PROJ  [("iter","iter")]
            (
                SEL c
                (
                    q_e1
                )
            )
        
       
            
        
        
        env_then = map (select loop_then) env
        
        
        
        TI(q_e2, cols, subs_e2) = _compile env_then loop_then e2
        
        
        
            
        ----------------------------------------------------------------------    
        ----------------------------------------------------------------------    
        select :: AlgbExp -> (VarID, TableInfos) -> (VarID, TableInfos)
        ----------------------------------------------------------------------
        ----------------------------------------------------------------------
        select loop (v, TI(q, cols, subs)) = (v, TI(q', cols, subs'))
            where
            
                q' =    
                    PROJ  ([("iter","iter"), ("pos","pos")] ++ (asProjList1 cols))
                    (
                        EQJOIN  ("iter", "iter'")
                        (
                            q
                        )
                        (
                            PROJ  [("iter'","iter")]
                            (
                                loop
                            )
                        )
                    )
                
                subs' = suse q' subs
                
                 



----------------------------------------------------------------------
-- IfExpr 2
-- if e1 then e2 else e3
_compile env loop (If e1 e2 (Just e3)) = TI (q', cols, subs')
    where
        
        TI (q_e1, [c], []) = _compile env loop e1
        
        
        
        loop_then =
            PROJ  [("iter","iter")]
            (
                SEL c
                (
                    q_e1
                )
            )
        
        loop_else =
            PROJ  [("iter","iter")]
            (
                SEL "res"
                (
                    FUN_BOOL_NOT ("res", c)
                    (
                        q_e1
                    )
                )
            )
            
        
        
        env_then = map (select loop_then) env
        env_else = map (select loop_else) env
        
        
        
        TI(q_e2, cols, subs_e2) = _compile env_then loop_then e2
        TI(q_e3,    _, subs_e3) = _compile env_else loop_else e3
        
        
        
        q =
            ROWNUM ("item'",  [("iter", LangAlgb.Ascending), ("ord", LangAlgb.Ascending), ("pos", LangAlgb.Ascending)],  Nothing)
            (
                DISJUNION
                (
                    ATTACH ("ord", (AT_NAT, AV_NAT 1))
                    (
                        q_e2
                    )
                )
                (
                    ATTACH ("ord", (AT_NAT, AV_NAT 2))
                    (
                        q_e3
                    )
                )
            )
        
        q' =
            PROJ  ([("iter","iter"), ("pos","pos")] ++ (asProjList1 (diff cols (keys subs_e2))) ++ (asProjList2 (keys subs_e2) "item'")) 
            (
                q
            )
            
        -- TODO: CHECK!!!
        subs' = suap q subs_e2 subs_e3
        -- subs' = suap q q subs_e2 subs_e3
        
            
        ----------------------------------------------------------------------    
        ----------------------------------------------------------------------    
        select :: AlgbExp -> (VarID, TableInfos) -> (VarID, TableInfos)
        ----------------------------------------------------------------------
        ----------------------------------------------------------------------
        select loop (v, TI(q, cols, subs)) = (v, TI(q', cols, subs'))
            where
            
                q' =    
                    PROJ  ([("iter","iter"), ("pos","pos")] ++ (asProjList1 cols))
                    (
                        EQJOIN  ("iter", "iter'")
                        (
                            q
                        )
                        (
                            PROJ  [("iter'","iter")]
                            (
                                loop
                            )
                        )
                    )
                
                subs' = suse q' subs
                
                 
        



----------------------------------------------------------------------
-- ForExpr 1 + 2


_compile env loop (For v e1 e2 maybeForOrderByClause) = TI (q, cols_e2, subs_e2)
    where
        
        TI(q_e1, cols_e1, subs_e1) =  _compile env loop e1
        
        
        
        q_v =
            ATTACH ("pos", (AT_NAT, AV_NAT 1))
            (
                PROJ  ([("iter","inner")] ++ (asProjList1 cols_e1))
                (
                    ROWNUM ("inner",  [("iter", LangAlgb.Ascending), ("pos", LangAlgb.Ascending)],  Nothing)
                    (
                        q_e1
                    )
                )	
            )
            
            
            
        loop_v = 
            PROJ  [("iter","iter")]
            (
                q_v
            )
            
        map_v =
            --PROJ  [("outer","iter"), ("inner","inner"), ("sort","pos")]
            PROJ  [("outer","iter"), ("inner","inner")]
            (
                ROWNUM ("inner",  [("iter", LangAlgb.Ascending), ("pos", LangAlgb.Ascending)],  Nothing)
                (
                    q_e1
                )
            )
        
            
        
        env_v = [(v, TI(q_v, cols_e1, subs_e1))] ++ (map lift env)
            where
                ----------------------------------------------------------------------    
                ----------------------------------------------------------------------    
                lift :: (VarID, TableInfos) -> (VarID, TableInfos)
                ----------------------------------------------------------------------
                ----------------------------------------------------------------------
                lift (v, TI(q, cols, subs)) = (v, TI(q', cols, subs))
                    where
                        
                        q' =
                            PROJ  ([("iter","inner"), ("pos","pos")] ++ (asProjList1 cols))
                            (
                                EQJOIN  ("iter", "outer")
                                (
                                    q
                                )
                                (
                                    map_v
                                )
                            )            
        
        
        
        TI(q_e2, cols_e2, subs_e2) = _compile env_v loop_v e2
        
        
        
        (map', rankSortSpecs) = case
            maybeForOrderByClause of 
                Nothing         -> (map_v, [("iter", LangAlgb.Ascending), ("pos", LangAlgb.Ascending)])
            
                Just sortSpecs  -> (map', rankSortSpecs ++ [("iter", LangAlgb.Ascending), ("pos", LangAlgb.Ascending)])
                    where
                        qs = map f (map ( _compile env_v loop_v) (map fst sortSpecs))  
                        f (TI (e, _cols, _subs)) = e
                        cols = map (cid2 "sort") [1..(length qs)]
                        map' = omap map_v (qs, cols)
                        rankSortSpecs = zip cols (map direction (map snd sortSpecs))
                        direction dir = case dir of
                            LangCommon.Ascending ->  LangAlgb.Ascending
                            LangCommon.Descending ->  LangAlgb.Descending
                       
                        
                        
        
        
        q =
            PROJ  ([("iter","outer"), ("pos","pos'")] ++ (asProjList1 cols_e2))
            (
                --ROWNUM ("pos'",  [("iter", LangAlgb.Ascending), ("pos", LangAlgb.Ascending)],  Just "outer")
                RANK ("pos'",  rankSortSpecs)
                (
                    EQJOIN  ("iter", "inner")
                    (
                        q_e2
                    )
                    (
                        map'
                    )
                )
            ) 
   





----------------------------------------------------------------------
-- TableRefExpr  
_compile _env loop  (Table tblName atts keys orderAtts) =  TI(q', cols, subs)  
    where
        
        cols = map cid [1..(length atts)]
            
                
        subs = []
        
        tcols = zip (map fst atts) cols
        
        semanticalInfosTABLEREF = (tblName, tblAttributeInfos, keyInfos)
            where
                
                mappedTypes = map typemap (map snd atts)
                    where
                        typemap atrType = case atrType of
                            ATRT_INT  -> AT_INT
                            ATRT_STR  -> AT_STR  
                            ATRT_BOOL -> AT_BOOL 
                            ATRT_DEC  -> AT_DEC  
                            ATRT_DBL  -> AT_DBL  
                            ATRT_NAT  -> AT_NAT  
                           
                
                keyInfos = map f1 keys
                    where
                        f1 atts = map f2 atts
                            where f2 att = case (lookup att tcols) of
                                            Just n  -> n
                                            Nothing -> error "Oops" 
                      
                tblAttributeInfos = zip3 (map fst atts) cols mappedTypes 
        
        orderAtts' = map f orderAtts
            where
                f (attrName, dir) = case lookup attrName tcols of
                        Just n  -> (n, direction dir) 
                        Nothing -> error "this should not happen"
        
                direction dir = case dir of
                    LangCommon.Ascending ->  LangAlgb.Ascending
                    LangCommon.Descending ->  LangAlgb.Descending
                    
                    
        q' =
            CROSS
            (
                loop
            )
            (
                RANK ("pos",  orderAtts')
                (
                    TABLEREF semanticalInfosTABLEREF
                )	
            )    





----------------------------------------------------------------------
-- ParenthesedExpr
_compile env loop (Parenthesed e) = _compile env loop e 


 
 
 
 
 
 
            
----------------------------------------------------------------------
-- (APPEND)
_compile env loop (FunApp "append" (Exprs [e1, e2])) = TI (q', cols, subs')
    where
        
        TI (q_e1, cols, subs_e1) = _compile env loop e1   
        TI (q_e2, _cols, subs_e2) = _compile env loop e2
        
        q =
            ROWNUM ("item'",  [("iter", LangAlgb.Ascending), ("ord", LangAlgb.Ascending), ("pos", LangAlgb.Ascending)],  Nothing)
            (
                RANK ("pos'",  [("ord", LangAlgb.Ascending), ("pos", LangAlgb.Ascending)])
                (
                    DISJUNION
                    (
                        ATTACH ("ord", (AT_NAT, AV_NAT 1))
                        (
                            q_e1
                        )
                    )
                    (
                        ATTACH ("ord", (AT_NAT, AV_NAT 2))
                        (
                            q_e2
                        )
                    )
                )
            )
            
        
              
        q' = 
            PROJ  ([("iter","iter"), ("pos","pos'")] ++ (asProjList1 (diff cols (keys subs_e1))) ++ (asProjList2  (keys subs_e1) "item'") )
            (
                q
            )
            
        
        subs' = suap q subs_e1 subs_e2





----------------------------------------------------------------------
-- (appendSF)
_compile env loop (FunApp "appendSF" (Exprs [Literal (BoolLiteral emptySubLists), e1, e2])) = TI (q', cols, subs')
    where
        
        TI (q_e1, cols, subs_e1) = _compile env loop e1   
        TI (q_e2,    _, subs_e2) = compileAppendSequence 2 env loop e2
        
        q =
            ROWNUM ("item'",  [("iter", LangAlgb.Ascending), ("ord", LangAlgb.Ascending), ("pos", LangAlgb.Ascending)],  Nothing)
            (
                RANK ("pos'",  [("ord", LangAlgb.Ascending), ("pos", LangAlgb.Ascending)])
                (
                    DISJUNION
                    (
                        ATTACH ("ord", (AT_NAT, AV_NAT 1))
                        (
                            q_e1
                        )
                    )
                    (
                        q_e2
                    )
                )
            )
            
        
              
        q' = 
            PROJ  ([("iter","iter"), ("pos","pos'")] ++ (asProjList1 (diff cols (keys subs_e1))) ++ (asProjList2  (keys subs_e1) "item'") )
            (
                q
            )
            
        
        subs' = suapSF emptySubLists q subs_e1 subs_e2




----------------------------------------------------------------------
-- (CONCAT)
_compile env loop (FunApp "concat" (Exprs [e])) = TI (q, cols_sub, subs_sub)
    where
        
        TI (q_e, _, [(c, TI (q_sub, cols_sub, subs_sub))]) = _compile env loop e 
        
        q =
            PROJ  ([("iter","iter'"), ("pos","pos''")] ++ (asProjList1 cols_sub))
            (
	            RANK ("pos''",  [("pos'", LangAlgb.Ascending), ("pos", LangAlgb.Ascending)])
                (   
	                EQJOIN  (c++"'", "iter")
                    (
	                    PROJ  [("iter'","iter"), ("pos'","pos"), (c ++ "'",c)]
                        (
	                        q_e
                        )
                    )
                    (
	                    q_sub
                    )
	                
	                
                )
            )
            
  
  
  
----------------------------------------------------------------------
-- (UNORDERED)    
_compile env loop  (FunApp "unordered" (Exprs [e])) =  TI(q', cols, subs)  
    where
        
        TI(q, cols, subs) = _compile env loop e
        
        q' =
            ATTACH ("pos", (AT_NAT, AV_NAT 42))
            (
                PROJ  ([("iter","iter")] ++ asProjList1 cols) 
	            (   
	                q    
	            )
            )
  
  
        
       


----------------------------------------------------------------------
-- (HEAD)
_compile env loop (FunApp "head" (Exprs [e])) = TI (q'', cols, subs')
    where
        
        TI (q, cols, subs) = _compile env loop e
        
        q' = abspos cols q 
        
        q'' =
            PROJ  ([("iter","iter"), ("pos","pos")] ++ (asProjList1 cols))
            (
                SEL "res"
                (
                    FUN_NUM_EQ ("res", ("pos","one"))
                    (
                        ATTACH ("one", (AT_NAT, AV_NAT 1))
                        (
                            q'
                        )
                    )
                )
            )
            
        subs' = suse q'' subs
        
        
----------------------------------------------------------------------
-- (HEAD 2)
_compile env loop (FunApp "head2" (Exprs [e])) = TI (q', cols, subs')
    where
        
        TI (q, cols, subs) = _compile env loop e
        
        
        
        q' =
            
            POS_SELECT (1,  [("pos", LangAlgb.Ascending)],  Just "iter")
            (
	            q
            )
            
           
            
        subs' = suse q' subs        
        
        
----------------------------------------------------------------------
-- (TAIL)
_compile env loop (FunApp "tail" (Exprs [e])) = TI (q'', cols, subs')
    where
        
        TI (q, cols, subs) = _compile env loop e
        
        q' = abspos cols q 
        
        q'' =
            PROJ  ([("iter","iter"), ("pos","pos")] ++ (asProjList1 cols))
            (
               SEL "res"
                (
                    FUN_NUM_GT ("res", ("pos","one"))
                    (
                        ATTACH ("one", (AT_NAT, AV_NAT 1))
                        (
                            q'
                        )
                    )
                )
            )
            
        subs' = suse q' subs
        



----------------------------------------------------------------------
-- (NTH)
_compile env loop (FunApp "nth" (Exprs [e1, e2])) = TI (q', cols, subs')
    where
        
        TI (q_e1, [c], _) = _compile env loop e1
        TI (q_e2, cols, subs) = _compile env loop e2
        
        q_e2' = abspos cols q_e2
        
        q' =
            PROJ  ([("iter","iter"),("pos","pos")] ++ (asProjList1 cols))
            (
                SEL "res"
                (
                    FUN_NUM_EQ ("res", ("pos'",c++"'"))
                    (
                        EQJOIN  ("iter", "iter'")
                        (
                            CAST ("pos'", "pos", AT_INT)
                            (    
                                q_e2'
                            )
                        )
                        (
                            PROJ  [("iter'","iter"), (c++"'",c)]
                            (
                                q_e1
                            )
                        )
                    )
                )
            )
            
            
        subs' = suse q' subs


----------------------------------------------------------------------
-- (UL)
_compile env loop (FunApp "ul" (Exprs [e])) = _compile env loop e
  

----------------------------------------------------------------------
-- (TAKE)
_compile env loop (FunApp "take" (Exprs [e1, e2])) = TI (q', cols, subs')
    where
        
        TI (q_e1, [c], _) = _compile env loop e1
        TI (q_e2, cols, subs) = _compile env loop e2
        
        q_e2' = abspos cols q_e2
        
        q' =            
            PROJ  ([("iter","iter"), ("pos","pos")] ++ (asProjList1 cols))
            (
                SEL "res"
                (
                    FUN_NUM_GT ("res", (c++"'","pos'"))
                    (
                        EQJOIN  ("iter", "iter'")
                        (
                            CAST ("pos'", "pos", AT_INT)
                            (    
                                q_e2'
                            )
                        )
                        (
                            PROJ  [("iter'","iter"), (c++"'","res")]
                            (
                                FUN_1TO1 (FT1TO1_ADD, "res", [c,"one"])
                                (
                                    ATTACH ("one", (AT_INT, AV_INT 1))
                                    (
                                        q_e1
                                    )    
                                )
                            )
                        )
                    )
                )
            )
           
            
        subs' = suse q' subs


----------------------------------------------------------------------
-- (DROP)
_compile env loop (FunApp "drop" (Exprs [e1, e2])) = TI (q', cols, subs')
    where
        
        TI (q_e1, [c], _) = _compile env loop e1
        TI (q_e2, cols, subs) = _compile env loop e2
        
        q_e2' = abspos cols q_e2
        
        q' =            
            PROJ  ([("iter","iter"), ("pos","pos")] ++ (asProjList1 cols))
            (   
                SEL "res"
                (
                    FUN_NUM_GT ("res", ("pos'", c++"'"))
                    (
                        EQJOIN  ("iter", "iter'")
                        (
                            CAST ("pos'", "pos", AT_INT)
                            (    
                                q_e2'
                            )
                        )
                        (
                            PROJ  [("iter'","iter"), (c++"'",c)]
                            (
                                q_e1
                                
                            )
                        )
                    )
                )
                
            )
           
            
        subs' = suse q' subs





----------------------------------------------------------------------
-- (EMPTY)
_compile env loop (FunApp "empty" (Exprs [e])) = TI (q'', [cid 1], [])
    where
        
        TI (q_e, cols_e, subs_e) = _compile env loop e
       
        
        q =            
            ATTACH (cid 1, (AT_BOOL, AV_BLN True))
            (
                DIFFERENCE
                (
	                loop
                )
                (
	                PROJ  [("iter","iter")]
                    (
                        q_e
                    )    
                )
            )
           
        
        q' =            
            ATTACH (cid 1, (AT_BOOL, AV_BLN False))
            (
                DISTINCT 
                (
	                PROJ  [("iter","iter")]
                    (
                        q_e
                    )    
                )
            ) 
        
        q'' =            
            ATTACH ("pos", (AT_NAT, AV_NAT 1))
            (
                DISJUNION
                (
	                q
                )
                (
	                q'
                )
            ) 
         
            
       



----------------------------------------------------------------------
-- (DistinctValues)
_compile env loop (FunApp "distinctValues" (Exprs [e])) = TI (q, cols_e, subs_e)
    where
        
        TI (q_e, cols_e, subs_e) = _compile env loop e
        
        q =            
            ATTACH ("pos", (AT_NAT, AV_NAT 42))
            (
	            DISTINCT 
                (
                    PROJ  ([("iter","iter")] ++ (asProjList1 cols_e))
                    (
                        q_e
                    )
                )
            )
            
            
 ----------------------------------------------------------------------
-- (the
_compile env loop (FunApp "the" (Exprs [e])) = _compile env loop (FunApp "distinctValues" (Exprs [e]))







----------------------------------------------------------------------
-- (ZIP*)
--_compile env loop (FunApp "zipn" (Exprs [e])) = TI (q_j, cols_j, subs_j)
--    where
--    
--        TI (q_e, cols_e, subs_e) = _compile env loop e
--        
--        TI (q_j, cols_j, subs_j) = join subs_e  




----------------------------------------------------------------------
-- (ZIP2)
_compile env loop (FunApp "zip2" (Exprs [e1,e2])) = TI (q, cols_e_1 ++ cols_e_2', subs_e_1 ++ subs_e_2')
    where

        TI (q_e_1, cols_e_1, subs_e_1) = _compile env loop e1
        TI (q_e_2, cols_e_2, subs_e_2) = _compile env loop e2
        
        q_e_1' = abspos cols_e_1 q_e_1
        q_e_2' = abspos cols_e_2 q_e_2
        
        cols_e_2' = incr2 cols_e_2 (length cols_e_1)
        subs_e_2' = incr3 subs_e_2 (length cols_e_1)
        
        q =
            PROJ  ([("iter","iter"), ("pos","pos")] ++ asProjList1 cols_e_1 ++ asProjList1 cols_e_2')
            (    
                SEL "res"
                    (
	                    FUN_NUM_EQ ("res", ("pos","pos'"))
                        (
                            EQJOIN  ("iter", "iter'")
                            (
	                            --PROJ  ([("iter","iter"), ("pos","pos")] ++ asProjList1 cols_e_1)
                                --(
	                                q_e_1'
	                            --)
                            )
                            (
	                            PROJ  ([("iter'","iter"), ("pos'","pos")] ++ asProjList3 cols_e_2' cols_e_2)
                                (
                                    q_e_2'
                                )
                            )
                        )
                    )
                )
        
        


----------------------------------------------------------------------
-- (UNZIP)
_compile env loop (FunApp "unzip" (Exprs [e])) = TI (q, cols_e, subs)
    where
    
        TI (q_e, cols_e, subs_e) = _compile env loop e
        
        q = 
            PROJ  ([("iter","iter"), ("pos","pos")] ++ (asProjList2 cols_e "iter"))
            (
	            ATTACH ("pos", (AT_NAT, AV_NAT 1))
                (
                    loop    
                )
            )
            
        subs = map f cols_e
        f c =
            (
                c,
                TI (
                        PROJ  [("iter","iter"), ("pos","pos"), (cid 1, c)]
                        (
	                        q_e
                        ), 
                        [cid 1], 
                        decr3 (retainByKeys subs_e [c]) ((ord c) - 1)
                   )
            )    


----------------------------------------------------------------------
-- (GROUPWITH)

_compile env loop (FunApp "groupWith" (InlineLambda ((v, e_g), e))) = TI (q_2, [cid 1], [(cid 1, TI (q_3, cols_e, subs_e))])
    where
        
        TI(q_e, cols_e, subs_e) =  _compile env loop e
        
        
        
        q_v =
            ROWNUM ("inner",  [("iter", LangAlgb.Ascending), ("pos", LangAlgb.Ascending)],  Nothing)
            (
                q_e
            )
                	
            
            
        loop_v = 
            PROJ  [("iter","inner")]
            (
                q_v
            )
            
        map_v =
            PROJ  [("outer","iter"), ("inner","inner")]
            (
                q_v
            )
        
       
       
        q_v' =
             ATTACH ("pos", (AT_NAT, AV_NAT 1))
            (
                PROJ  ([("iter","inner")] ++ (asProjList1 cols_e))
                (
                    q_v
                )     
            ) 
            
            
        
        env_v = [(v, TI(q_v', cols_e, subs_e))] ++ (map lift env)
            where
                ----------------------------------------------------------------------    
                ----------------------------------------------------------------------    
                lift :: (VarID, TableInfos) -> (VarID, TableInfos)
                ----------------------------------------------------------------------
                ----------------------------------------------------------------------
                lift (v, TI(q, cols, subs)) = (v, TI(q', cols, subs))
                    where
                        
                        q' =
                            PROJ  ([("iter","inner"), ("pos","pos")] ++ (asProjList1 cols))
                            (
                                EQJOIN  ("iter", "outer")
                                (
                                    q
                                )
                                (
                                    map_v
                                )
                            )            
        
        
        
        TI(q_e_g, cols_e_g, subs_e_g) = _compile env_v loop_v e_g
        
        cols_e_g' = incr2 cols_e_g (length cols_e)
        
        q_1 =
            ROWRANK ("grpKey",  [("iter", LangAlgb.Ascending)] ++ asSortList1 cols_e_g')
            (
                EQJOIN  ("inner", "iter'")
                (
                    q_v
                )
                (
                    PROJ  ([("iter'","iter")] ++ (asProjList3 cols_e_g' cols_e_g))
                    (
                        q_e_g
                    )
                )
            )
            

        q_2 =
            DISTINCT 
            (
                PROJ  ([("iter","iter"), ("pos","grpKey"), (cid 1, "grpKey")])
                (
                    q_1
                )
            )
            

        q_3 =
            PROJ  ([("iter","grpKey"), ("pos","pos")] ++ (asProjList1 cols_e))
            (
                q_1
            )
            



----------------------------------------------------------------------
-- (GROUPWITH2)

_compile env loop (FunApp "groupWith2" (InlineLambda ((v, e_g), e))) = TI (q_2, (cols_e_g ++ [(cid ((length cols_e_g)+1))]), [((cid ((length cols_e_g)+1)), TI (q_3, cols_e, subs_e))])
    where
        
        TI(q_e, cols_e, subs_e) =  _compile env loop e
        
        
        
        q_v =
            ROWNUM ("inner",  [("iter", LangAlgb.Ascending), ("pos", LangAlgb.Ascending)],  Nothing)
            (
                q_e
            )
                	
            
            
        loop_v = 
            PROJ  [("iter","inner")]
            (
                q_v
            )
            
        map_v =
            PROJ  [("outer","iter"), ("inner","inner")]
            (
                q_v
            )
        
       
       
        q_v' =
             ATTACH ("pos", (AT_NAT, AV_NAT 1))
            (
                PROJ  ([("iter","inner")] ++ (asProjList1 cols_e))
                (
                    q_v
                )     
            ) 
            
            
        
        env_v = [(v, TI(q_v', cols_e, subs_e))] ++ (map lift env)
            where
                ----------------------------------------------------------------------    
                ----------------------------------------------------------------------    
                lift :: (VarID, TableInfos) -> (VarID, TableInfos)
                ----------------------------------------------------------------------
                ----------------------------------------------------------------------
                lift (v, TI(q, cols, subs)) = (v, TI(q', cols, subs))
                    where
                        
                        q' =
                            PROJ  ([("iter","inner"), ("pos","pos")] ++ (asProjList1 cols))
                            (
                                EQJOIN  ("iter", "outer")
                                (
                                    q
                                )
                                (
                                    map_v
                                )
                            )            
        
        
        
        TI(q_e_g, cols_e_g, subs_e_g) = _compile env_v loop_v e_g
        
        cols_e_g' = incr2 cols_e_g (length cols_e)
        
        q_1 =
            ROWRANK ("grpKey",  [("iter", LangAlgb.Ascending)] ++ asSortList1 cols_e_g')
            (
                EQJOIN  ("inner", "iter'")
                (
                    q_v
                )
                (
                    PROJ  ([("iter'","iter")] ++ (asProjList3 cols_e_g' cols_e_g))
                    (
                        q_e_g
                    )
                )
            )
            

        q_2 =
            DISTINCT 
            (
                PROJ  ([("iter","iter"), ("pos","grpKey")] ++  (asProjList3 cols_e_g cols_e_g') ++ [(cid ((length cols_e_g)+1), "grpKey")])
                (
                    q_1
                )
            )
            

        q_3 =
            PROJ  ([("iter","grpKey"), ("pos","pos")] ++ (asProjList1 cols_e))
            (
                q_1
            )
            








----------------------------------------------------------------------
-- (PARTITON)

_compile env loop (FunApp "partition" (InlineLambda ((v, e_p), e))) = TI (q, [cid 1, cid 2], [(cid 1, TI (q_t, cols_e, subs_e_t)), (cid 2, TI (q_f, cols_e, subs_e_f))])
    where
        
        TI(q_e, cols_e, subs_e) =  _compile env loop e
        
        
        
        q_v =
            ROWNUM ("inner",  [("iter", LangAlgb.Ascending), ("pos", LangAlgb.Ascending)],  Nothing)
            (
                q_e
            )
                	
            
            
        loop_v = 
            PROJ  [("iter","inner")]
            (
                q_v
            )
            
        map_v =
            PROJ  [("outer","iter"), ("inner","inner")]
            (
                q_v
            )
        
       
       
        q_v' =
             ATTACH ("pos", (AT_NAT, AV_NAT 1))
            (
                PROJ  ([("iter","inner")] ++ (asProjList1 cols_e))
                (
                    q_v
                )     
            ) 
            
            
        
        env_v = [(v, TI(q_v', cols_e, subs_e))] ++ (map lift env)
            where
                ----------------------------------------------------------------------    
                ----------------------------------------------------------------------    
                lift :: (VarID, TableInfos) -> (VarID, TableInfos)
                ----------------------------------------------------------------------
                ----------------------------------------------------------------------
                lift (v, TI(q, cols, subs)) = (v, TI(q', cols, subs))
                    where
                        
                        q' =
                            PROJ  ([("iter","inner"), ("pos","pos")] ++ (asProjList1 cols))
                            (
                                EQJOIN  ("iter", "outer")
                                (
                                    q
                                )
                                (
                                    map_v
                                )
                            )            
        
        
        
        TI(q_e_p, ["item1"], []) = _compile env_v loop_v e_p
        
        
        c__e_p = cid ((length cols_e) + 1)
        
        q_e__e_p =
            PROJ  ([("iter","iter"), ("pos", "pos")] ++ (asProjList1 cols_e) ++ [(c__e_p ,c__e_p )])
            (
                EQJOIN  ("inner", "iter'")
                (
                    q_v
                )
                (
                    PROJ ([("iter'","iter"), (c__e_p , "item1")]) 
                    (
                        q_e_p
                    )
                )
            )
        
        q_t =
            PROJ  ([("iter","iter"), ("pos", "pos")] ++ (asProjList1 cols_e))
            (         
                SEL c__e_p
                (
	                q_e__e_p
                )
            )    
            
        q_f =
            PROJ  ([("iter","iter"), ("pos", "pos")] ++ (asProjList1 cols_e))
            (         
                SEL "res"
                (
                    FUN_BOOL_NOT ("res", c__e_p)
                    (
                        q_e__e_p
                    )
                )
            )    
        
        
        subs_e_t = suse q_t subs_e
        
        subs_e_f = suse q_f subs_e
        
        
        q =
            PROJ  [("iter","iter"), ("pos","pos"), ("item1","iter"), ("item2","iter")]
            (   
                ATTACH ("pos", (AT_NAT, AV_NAT 1))
                (
                    loop
                )
            ) 
            




----------------------------------------------------------------------
-- (AGGR 1)
_compile env loop (FunApp "avg" (Exprs args)) =_compile_AggrOperationOnLists env loop (FunApp "avg" (Exprs args))
_compile env loop (FunApp "avg_" (Exprs args)) =_compile_AggrOperationOnLists env loop (FunApp "avg_" (Exprs args))
_compile env loop (FunApp "max" (Exprs args)) =_compile_AggrOperationOnLists env loop (FunApp "max" (Exprs args))
_compile env loop (FunApp "max_" (Exprs args)) =_compile_AggrOperationOnLists env loop (FunApp "max_" (Exprs args))
_compile env loop (FunApp "min" (Exprs args)) =_compile_AggrOperationOnLists env loop (FunApp "min" (Exprs args))
_compile env loop (FunApp "min_" (Exprs args)) =_compile_AggrOperationOnLists env loop (FunApp "min_" (Exprs args))
_compile env loop (FunApp "sum" (Exprs args)) =_compile_AggrOperationOnLists env loop (FunApp "sum" (Exprs args))
_compile env loop (FunApp "sum_" (Exprs args)) =_compile_AggrOperationOnLists env loop (FunApp "sum_" (Exprs args))

----------------------------------------------------------------------
-- (AGGR 2)
_compile env loop (FunApp "count" (Exprs args)) =_compile_AggrOperationOnLists env loop (FunApp "count" (Exprs args))
_compile env loop (FunApp "count_" (Exprs args)) =_compile_AggrOperationOnLists env loop (FunApp "count_" (Exprs args))
        





         
            





-- =============================================================================
-- Operation-Dispatch
-- =============================================================================


                     -- resultAttr      leftOperand     rightOperand    childNode   AlgBinOp
type AlgBinOpProvider = AttrName ->     AttrName ->     AttrName ->     AlgbExp ->  AlgbExp

                     -- resultAttr      operand         childNode   AlgUnOp
type AlgUnOpProvider =  AttrName ->     AttrName ->     AlgbExp ->  AlgbExp


                      --  resultAttr      operand         partitioningAttrName   childNode   AlgAggrOp
type AlgAggrOpProvider1 = AttrName ->     AttrName ->     AttrName ->            AlgbExp ->  AlgbExp
type AlgAggrOpProvider2 = AttrName ->                     AttrName ->            AlgbExp ->  AlgbExp


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

----------------------------------------------------------------------
----------------------------------------------------------------------
_compile_OperatorOnAtomicValues env loop (FunApp funName (Exprs args)) = case funName of
    
    ------------------------------------------------------------------------------------------------
    -- Binary Operations on atomic values
    ------------------------------------------------------------------------------------------------
    
    "+"             ->     _compile_BINOP env loop (algBinOpProvider_FUN_1TO1 FT1TO1_ADD)       args
    "-"             ->     _compile_BINOP env loop (algBinOpProvider_FUN_1TO1 FT1TO1_SUBTRACT)  args
    "*"             ->     _compile_BINOP env loop (algBinOpProvider_FUN_1TO1 FT1TO1_MULTIPLY)  args
    "/"             ->     _compile_BINOP env loop (algBinOpProvider_FUN_1TO1 FT1TO1_DIVIDE)    args
    "%"             ->     _compile_BINOP env loop (algBinOpProvider_FUN_1TO1 FT1TO1_MODULO)    args
    
    ------------------------------------------------------------------------------------------------
    "=="            ->     _compile_BINOP env loop  algBinOpProvider args
                            where
                                algBinOpProvider resultAttrName leftOperand rightOperand algExpr =
                                    FUN_NUM_EQ (resultAttrName,(leftOperand,rightOperand))
                                    (
                                        algExpr    
                                    )    
 
    "!="           ->     _compile_BINOP env loop  algBinOpProvider args
                            where
                                algBinOpProvider resultAttrName leftOperand rightOperand algExpr =
                                    FUN_BOOL_NOT (resultAttrName, resultAttrName ++ "'")
                                    (
                                        FUN_NUM_EQ (resultAttrName ++ "'",(leftOperand,rightOperand))
                                        (
                                            algExpr    
                                        )    
                                    )
    
    ">"            ->     _compile_BINOP env loop  algBinOpProvider args
                                where
                                    algBinOpProvider resultAttrName leftOperand rightOperand algExpr =
                                        FUN_NUM_GT (resultAttrName,(leftOperand,rightOperand))
                                        (
                                            algExpr    
                                        )    
    
    "<"            ->     _compile_BINOP env loop  algBinOpProvider (reverse args)
                                where
                                    algBinOpProvider resultAttrName leftOperand rightOperand algExpr =
                                        FUN_NUM_GT (resultAttrName,(leftOperand,rightOperand))
                                        (
                                            algExpr    
                                        )    

    ------------------------------------------------------------------------------------------------
    
    "and"           ->     _compile_BINOP env loop  algBinOpProvider args
                        where
                            algBinOpProvider resultAttrName leftOperand rightOperand algExpr =
                                FUN_BOOL_AND (resultAttrName,(leftOperand,rightOperand))
                                (
                                    algExpr    
                                )
    
    "or"            ->     _compile_BINOP env loop  algBinOpProvider args
                            where
                                algBinOpProvider resultAttrName leftOperand rightOperand algExpr =
                                    FUN_BOOL_OR (resultAttrName,(leftOperand,rightOperand))
                                    (
                                        algExpr    
                                    ) 
       
    ------------------------------------------------------------------------------------------------
    -- Unary Operations on atomic values
    ------------------------------------------------------------------------------------------------

    "not"           ->     _compile_UNOP env loop  algUnOpProvider args
                            where
                                algUnOpProvider resultAttrName operand algExpr =
                                    FUN_BOOL_NOT (resultAttrName, operand)
                                    (
                                        algExpr    
                                    )    

    
    
----------------------------------------------------------------------
----------------------------------------------------------------------
algBinOpProvider_FUN_1TO1 functionType1TO1 resultAttrName leftOperand rightOperand algExpr =
    FUN_1TO1 (functionType1TO1, resultAttrName, [leftOperand,rightOperand])
    (
        algExpr    
    )


----------------------------------------------------------------------
----------------------------------------------------------------------
_compile_BINOP env loop (algOpProvider::(AlgBinOpProvider)) [e1,e2] = TI (q, cols, subs)
    where
        
        c = cid(1) 
        
        TI (q_e1, _, _) = _compile env loop e1
        TI (q_e2, _, _) = _compile env loop e2
        
        q =
            PROJ  [("iter","iter"), ("pos","pos"),(c,"res")]
            (
                algOpProvider "res" c (c++"'")
                (
                    EQJOIN  ("iter", "iter'")
                    (
                        q_e1
                    )
                    (
                        PROJ  [("iter'","iter"), (c++"'",c)]
                        (
                            q_e2
                        )
                    )
                )
            )
          
        cols = [c]
        
        subs = [] 
        
       



----------------------------------------------------------------------
----------------------------------------------------------------------
_compile_UNOP env loop (algOpProvider::(AlgUnOpProvider)) [e] = TI (q, cols, subs)
    where
        
        c = cid(1) 
        
        TI (q_e, _, _) = _compile env loop e
       
        q =
            PROJ  [("iter","iter"), ("pos","pos"),(c,"res")]
            (
                algOpProvider "res" c
                (
                    q_e
                )
            )
          
        cols = [c]
        
        subs = [] 
        
     



-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


----------------------------------------------------------------------
----------------------------------------------------------------------
_compile_AggrOperationOnLists env loop (FunApp funName (Exprs args)) = case funName of
    
    ------------------------------------------------------------------------------------------------
    -- Aggregation Operations on lists
    ------------------------------------------------------------------------------------------------
    
    "avg"             ->     _compile_AGGR1 env loop (algAggrOpProvider1_FUN_AGGR FTAGGR_AVG) algEmptyListHandlingProviderERRORString   args
    "avg_"            ->     _compile_AGGR1 env loop (algAggrOpProvider1_FUN_AGGR FTAGGR_AVG) algEmptyListHandlingProviderNOTHING       args

    "max"             ->     _compile_AGGR1 env loop (algAggrOpProvider1_FUN_AGGR FTAGGR_MAX) algEmptyListHandlingProviderERRORString   args
    "max_"            ->     _compile_AGGR1 env loop (algAggrOpProvider1_FUN_AGGR FTAGGR_MAX) algEmptyListHandlingProviderNOTHING       args

    "min"             ->     _compile_AGGR1 env loop (algAggrOpProvider1_FUN_AGGR FTAGGR_MIN) algEmptyListHandlingProviderERRORString   args
    "min_"            ->     _compile_AGGR1 env loop (algAggrOpProvider1_FUN_AGGR FTAGGR_MIN) algEmptyListHandlingProviderNOTHING       args
   
    "sum"             ->     _compile_AGGR1 env loop (algAggrOpProvider1_FUN_AGGR FTAGGR_SUM) algEmptyListHandlingProviderZERO          args
    "sum_"            ->     _compile_AGGR1 env loop (algAggrOpProvider1_FUN_AGGR FTAGGR_SUM) algEmptyListHandlingProviderNOTHING       args
    
    "count"           ->     _compile_AGGR2 env loop algAggrOpProvider_FUN_AGGR_COUNT algEmptyListHandlingProviderZERO args
    --"count"           ->     _compile_AGGR2 env loop algAggrOpProvider_FUN_AGGR_COUNT algEmptyListHandlingProviderNOTHING args
    "count_"          ->     _compile_AGGR2 env loop algAggrOpProvider_FUN_AGGR_COUNT algEmptyListHandlingProviderNOTHING args
                                


----------------------------------------------------------------------
----------------------------------------------------------------------
algAggrOpProvider1_FUN_AGGR functionTypeAGGR res operand part algExpr =
    FUN_AGGR (functionTypeAGGR, (res,operand), Just part)
    (
        algExpr    
    )  
 

algAggrOpProvider_FUN_AGGR_COUNT res part algExpr =
    FUN_AGGR_COUNT (res, Just part)
    (
        algExpr    
    )  

 
algEmptyListHandlingProviderZERO q c loop =
    DISJUNION
    (
        q
    )
    (
        ATTACH (c, (AT_INT, AV_INT 0))
        (
            DIFFERENCE
            (
                loop
            )
            (
                PROJ  [("iter","iter")]
                (
                    q
                )    
            )
        )
    )

algEmptyListHandlingProviderERRORString q c loop =
    DISJUNION
    (
        q
    )
    (
        ATTACH (c, (AT_STR, AV_STR "error"))
        (
            DIFFERENCE
            (
                loop
            )
            (
                PROJ  [("iter","iter")]
                (
                    q
                )    
            )
        )
    )        
 
algEmptyListHandlingProviderNOTHING q c loop =
    q
    
----------------------------------------------------------------------
----------------------------------------------------------------------
_compile_AGGR1 env loop (algOpProvider::(AlgAggrOpProvider1)) algEmptyListHandlingProvider [e] = TI (q', cols, subs)
    where
        
        c = cid(1) 
        
        TI (q_e, _, _) = _compile env loop e
    
        
        q = 
            algOpProvider c c "iter"
            (
                q_e    
            )
            
        
        q' = 
            
            ATTACH ("pos", (AT_NAT, AV_NAT 1))
            (
	            
	            algEmptyListHandlingProvider q c loop
            )
        
            
          
        cols = [c]
        
        subs = [] 
        
       
   
    
----------------------------------------------------------------------
----------------------------------------------------------------------
_compile_AGGR2 env loop (algOpProvider::(AlgAggrOpProvider2)) algEmptyListHandlingProvider [e] = TI (q', cols, subs)
    where
        
        c = cid(1) 
        
        TI (q_e, _, _) = _compile env loop e
    
        
        q = 
            algOpProvider c "iter"
            (   
                q_e    
            )
                
                
        q' = 
            
            ATTACH ("pos", (AT_NAT, AV_NAT 1))
            (
	            
	            algEmptyListHandlingProvider q c loop
                    
            )
            
        
            
            
          
        cols = [c]
        
        subs = [] 
        
       
       






-- =============================================================================
-- Wrapping/Unwrapping of Serialize Ops
-- =============================================================================


                 
----------------------------------------------------------------------
----------------------------------------------------------------------
wrapWithSerializeOps :: TableInfos -> TableInfos
----------------------------------------------------------------------
----------------------------------------------------------------------
wrapWithSerializeOps (TI (algbExp, cols, subs)) =
    TI (serializeWrappedAlgbExp, cols, subs')    

    where
    
    
    serializeWrappedAlgbExp = SERIALIZE_REL semanticalInfos (NIL) algbExp
    semanticalInfos = ("iter", "pos", cols)
    -- semanticalInfos = ("pos", cols)
    
    
    subs' = map f subs
        where
           
            f (c, ti) = (c, wrapWithSerializeOps ti) 
            
            
----------------------------------------------------------------------
----------------------------------------------------------------------
removeSerializeOps :: TableInfos -> TableInfos
----------------------------------------------------------------------
----------------------------------------------------------------------
removeSerializeOps (TI ((SERIALIZE_REL _semanticalInfos (NIL) algbExp), cols, subs)) =
    TI (algbExp, cols, subs')    

    where
    
    subs' = map f subs
        where
           
            f (c, ti) = (c, removeSerializeOps ti) 
