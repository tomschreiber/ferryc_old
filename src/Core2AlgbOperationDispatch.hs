-----------------------------------------------------------------------------------------
{-| Module      : Core2Algb
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Core2AlgbOperationDispatch(

-- =============================================================================
-- exports
-- =============================================================================
    _compile_OperatorOnAtomicValues,
    

) where





-- =============================================================================
-- imports
-- =============================================================================

import LangCore(Expr (..), ATRType (..),  VarName, FunName) 
import LangAlgb(AttrName, JoinComparisonKind (..), FunctionType1TO1 (..), FunctionTypeAGGR (..), AlgbExp (..), AValue (..), SortDirection (..), AType (..))

import Core2AlgbUtils(  
                        cid,
                        incr1, incr2, incr3, decr1, decr2, decr3,  
                        asProjList1, asProjList2, asProjList3
                     )

import Utils.AssociationList(keys, diff, retainByKeys) 


import Data.List (zip4)





-- =============================================================================
-- types and data
-- =============================================================================


                     -- resultAttr      leftOperand     rightOperand    childNode   AlgBinOp
type AlgBinOpProvider = AttrName ->     AttrName ->     AttrName ->     AlgbExp ->  AlgbExp

                     -- resultAttr      operand         childNode   AlgUnOp
type AlgUnOpProvider =  AttrName ->     AttrName ->     AlgbExp ->  AlgbExp


                      --  resultAttr      operand         partitioningAttrName   childNode   AlgAggrOp
type AlgAggrOpProvider1 = AttrName ->     AttrName ->     AttrName ->            AlgbExp ->  AlgbExp
type AlgAggrOpProvider2 = AttrName ->                     AttrName ->            AlgbExp ->  AlgbExp


            
-- =============================================================================
-- functions
-- =============================================================================                
                
----------------------------------------------------------------------
----------------------------------------------------------------------
_compile_OperatorOnAtomicValues env loop (FUNAPP funName args) = case funName of
    
    ------------------------------------------------------------------------------------------------
    -- Binary Operations on atomic values
    ------------------------------------------------------------------------------------------------
    
    "+"             ->     _compile_BINOP env loop (algBinOpProvider_FUN_1TO1 FT1TO1_ADD)       args
    "-"             ->     _compile_BINOP env loop (algBinOpProvider_FUN_1TO1 FT1TO1_SUBTRACT)  args
    "*"             ->     _compile_BINOP env loop (algBinOpProvider_FUN_1TO1 FT1TO1_MULTIPLY)  args
    "/"             ->     _compile_BINOP env loop (algBinOpProvider_FUN_1TO1 FT1TO1_DIVIDE)    args
    
    ------------------------------------------------------------------------------------------------
    "="            ->     _compile_BINOP env loop  algBinOpProvider args
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
    FUN_1TO1 (functionType1TO1, (resultAttrName,(leftOperand,rightOperand)))
    (
        algExpr    
    )


----------------------------------------------------------------------
----------------------------------------------------------------------
_compile_BINOP env loop (algOpProvider::(AlgBinOpProvider)) [e1,e2] = TI (q, cols, subs, tcols)
    where
        
        c = cid(1) 
        
        TI (q_e1, _, _, _) = _compile env loop e1
        TI (q_e2, _, _, _) = _compile env loop e2
        
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
        
        tcols = [] 



----------------------------------------------------------------------
----------------------------------------------------------------------
_compile_UNOP env loop (algOpProvider::(AlgUnOpProvider)) [e] = TI (q, cols, subs, tcols)
    where
        
        c = cid(1) 
        
        TI (q_e, _, _, _) = _compile env loop e
       
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
        
        tcols = [] 



----------------------------------------------------------------------
----------------------------------------------------------------------
_compile_AggrOperationOnLists env loop (FUNAPP funName args) = case funName of
    
    ------------------------------------------------------------------------------------------------
    -- Aggregation Operations on lists
    ------------------------------------------------------------------------------------------------
    
    "avg"             ->     _compile_AGGR1 env loop (algAggrOpProvider1_FUN_AGGR FTAGGR_AVG)       args
    "max"             ->     _compile_AGGR1 env loop (algAggrOpProvider1_FUN_AGGR FTAGGR_MAX)       args
    "min"             ->     _compile_AGGR1 env loop (algAggrOpProvider1_FUN_AGGR FTAGGR_MIN)       args
    "sum"             ->     _compile_AGGR1 env loop (algAggrOpProvider1_FUN_AGGR FTAGGR_SUM)       args
    
    "count"           ->     _compile_AGGR2 env loop algAggrOpProvider2 args
                                where
                                    algAggrOpProvider2 res part algExpr =
                                        FUN_AGGR_COUNT (res, Just part)
                                        (
                                            algExpr    
                                        )  
    


----------------------------------------------------------------------
----------------------------------------------------------------------
algAggrOpProvider1_FUN_AGGR functionTypeAGGR res operand part algExpr =
    FUN_AGGR (functionTypeAGGR, (res,operand), Just part)
    (
        algExpr    
    )  
    
    
    
----------------------------------------------------------------------
----------------------------------------------------------------------
_compile_AGGR1 env loop (algOpProvider::(AlgAggrOpProvider1)) [e] = TI (q, cols, subs, tcols)
    where
        
        c = cid(1) 
        
        TI (q_e, _, _, _) = _compile env loop e
    
        
        q = 
            ATTACH ("pos", (AT_NAT, AV_NAT 1))
            (
                PROJ  [("iter","part"), (c,"res")]
                (
                    algOpProvider "res" c "iter"
                    (
                        q_e    
                    )
                )
            )
            
            
          
        cols = [c]
        
        subs = [] 
        
        tcols = [] 
   
    
----------------------------------------------------------------------
----------------------------------------------------------------------
_compile_AGGR2 env loop (algOpProvider::(AlgAggrOpProvider2)) [e] = TI (q, cols, subs, tcols)
    where
        
        c = cid(1) 
        
        TI (q_e, _, _, _) = _compile env loop e
    
        
        q = 
            ATTACH ("pos", (AT_NAT, AV_NAT 1))
            (
                PROJ  [("iter","part"), (c,"res")]
                (
                    algOpProvider "res" "iter"
                    (   
                        q_e    
                    )
                )
            )
            
            
          
        cols = [c]
        
        subs = [] 
        
        tcols = [] 
           
                 
