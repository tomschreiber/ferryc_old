-----------------------------------------------------------------------------------------
{-| Module      : Normalize
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Normalize where


-- =============================================================================
-- imports
-- =============================================================================


import LangCommon
import Lang
import LangCore

import Data.List(intersperse, isPrefixOf) 


type Env = [(VarID, (Int,[(ATRName, Int)]))]


-- =============================================================================
-- Surface -> Core
-- =============================================================================


stableCols :: FunID -> Bool
stableCols f = elem f   [
                            "unordered",    
                            "head",    
                            "tail",    
                            "nth",    
                            "ul",    
                            "take",    
                            "drop",    
                            "distinctValues",    
                            "groupBy"    
                        ]


----------------------------------------------------------------------
----------------------------------------------------------------------
-- 
p :: Env -> LangCore.Expr -> (Int, [(ATRName, Int)])
----------------------------------------------------------------------
----------------------------------------------------------------------

p env (LangCore.Literal _) = (0,[])

p env (LangCore.OpApp _) = (0,[])

p env (LangCore.Tuple _) = (0,[])

p env (LangCore.PosAcc _ _) = (0,[])

p env (LangCore.List _ ) = (0,[])

p env (LangCore.Let varID e1 e2) =  let
                                        pE1 = p env e1
                                        env' = [(varID, pE1)] ++ env
                                    in
                                        p env' e2
                                            
p env (LangCore.Var varID) =  case lookup varID env of 
                                            Just m    -> m
                                            Nothing     -> error ("variable " ++ varID ++ " unknown1")


p env (LangCore.If e1 e2 (_maybeE3)) = p env e2


p env (LangCore.For varID e1 e2 (_maybeForOrderByClause)) =     let
                                                                    pE1 = p env e1
                                                                    env' = [(varID, pE1)] ++ env
                                                                in
                                                                    p env' e2

p env (LangCore.Table _tblID tableAttSpecs _tableKeySpecs _tableOrderSpecs) = (0,m)
                where 
                    m = map f (zip [1..] tableAttSpecs)
                    f (pos, (atrName, _atrType)) = (atrName, pos)





p env  (LangCore.FunApp "groupWith" (LangCore.InlineLambda ((varID, e1), e2))) = 
    let 
        (nest, pmap_e2) = p env e2
    in
        (nest + 1, pmap_e2) 


p env  (LangCore.FunApp "groupWith2" (LangCore.InlineLambda ((varID, e1), e2))) = 
    let 
        (nest, pmap_e2) = p env e2
    in
        (nest + 1, pmap_e2) 

p env  (LangCore.FunApp "partition" (LangCore.InlineLambda ((varID, e1), e2))) = 
    let 
        (nest, pmap_e2) = p env e2
    in
        (nest + 1, pmap_e2) 
  

p env  (LangCore.FunApp funID (LangCore.InlineLambda ((varID, e1), e2))) = 
    if stableCols funID
        then
            p env e2
    else
        (0,[])
        
        
p env  (LangCore.FunApp funID (LangCore.Exprs args)) = 
--p env  (LangCore.FunApp funID (LangCore.Exprs [arg])) = 
    if stableCols funID
        then
            if (length args) == 1
                then
                    p env (args!!0)
                else
                    p env (args!!1)
--            p env arg    
        
        else if(isPrefixOf "unzip" funID)
            then
                let
                    (nest, pmap_e) = p env (args!!0)
                    --(nest, pmap_e) = p env (arg)
                    nest' = maximum [0, nest - 1]
                in
                    (nest', pmap_e)  
    
            else (0,[])  

p env (LangCore.FunApp _ _) = (0,[])




p env (LangCore.Parenthesed e) = p env e 



normalize :: Lang.Expr -> Bool -> LangCore.Expr
normalize e groupByAsMacro = nExpr groupByAsMacro []  e 

----------------------------------------------------------------------
----------------------------------------------------------------------
-- Core -> Algebra
nExpr ::  Bool -> Env -> Lang.Expr -> LangCore.Expr
----------------------------------------------------------------------
----------------------------------------------------------------------

nExpr groupByAsMacro env (Lang.Literal (Lang.IntLiteral v)) = LangCore.Literal (LangCore.IntLiteral v)
nExpr groupByAsMacro env (Lang.Literal (Lang.StringLiteral v)) = LangCore.Literal (LangCore.StringLiteral v)
nExpr groupByAsMacro env (Lang.Literal (Lang.BoolLiteral v)) = LangCore.Literal (LangCore.BoolLiteral v)


nExpr groupByAsMacro env (Lang.OpApp (Lang.UnOp name e)) = LangCore.OpApp (LangCore.UnOp name (nExpr groupByAsMacro env e))
nExpr groupByAsMacro env (Lang.OpApp (Lang.BinOp name e1 e2)) = LangCore.OpApp (LangCore.BinOp name (nExpr groupByAsMacro env e1) (nExpr groupByAsMacro env e2))

nExpr groupByAsMacro env (Lang.Tuple exprs) = LangCore.Tuple (map (nExpr groupByAsMacro env) exprs)

nExpr groupByAsMacro env (Lang.PosAcc e i) = LangCore.PosAcc (nExpr groupByAsMacro env e) i

nExpr groupByAsMacro env (Lang.List exprs) = LangCore.List (map (nExpr groupByAsMacro env) exprs) 


nExpr groupByAsMacro env (Lang.Let [(varID, e1)] e2) = let
                                            nE1 = nExpr groupByAsMacro env e1
                                            pNE1 = p env nE1
                                            env' = [(varID, pNE1)] ++ env
                                            nE2 = nExpr groupByAsMacro env' e2
                                        in
                                            LangCore.Let varID nE1 nE2   

nExpr groupByAsMacro env (Lang.Let ((varID, e1):letBindings) e2) = nExpr groupByAsMacro env (Lang.Let [(varID, e1)] (Lang.Let letBindings e2))


nExpr groupByAsMacro env (Lang.Var varID) = LangCore.Var varID


nExpr groupByAsMacro env (Lang.If e1 e2 e3) =  let
                                    nE1 = nExpr groupByAsMacro env e1
                                    nE2 = nExpr groupByAsMacro env e2
                                    nE3 = case e3 of
                                        Nothing -> Nothing
                                        Just e  -> Just (nExpr groupByAsMacro env e)
                                in
                                    case e3 of
                                        Just e -> 
                                            if (p env nE2) == (p env (nExpr groupByAsMacro env e))
                                                then
                                                    LangCore.If nE1 nE2 nE3
                                                else
                                                    error "different or missing TableRef-expressions in if-branches..."
                                            
                                        Nothing ->
                                            LangCore.If nE1 nE2 nE3    
    
                
   
   
   
   
 
 
 
-------------------------------------------------------------------------------
-- FOR - INLINE FILTERING 1 
nExpr groupByAsMacro env  
    (
        Lang.For 
        [
            Lang.Binding [(v, (Lang.FunApp "filter" (Lang.InlineLambda ((v', e_f), e_1))))],
            Return e_2
        ]
    ) = 
    
    nExpr groupByAsMacro env 
    (   
        Lang.For 
        [
           Lang.Binding [(v, e_1)],
           Lang.Return (Lang.If e_f e_2 Nothing)  
        ]
    ) 
 
-------------------------------------------------------------------------------
-- FOR - INLINE FILTERING 2 
nExpr groupByAsMacro env  
    (
        Lang.For 
        [
            Lang.Binding [(v, (Lang.FunApp "filter" (Lang.InlineLambda ((v', e_f), e_1))))],
            Lang.OrderBy orderSpecs,
            Return e_2
        ]
    ) = 
    
    nExpr groupByAsMacro env 
    (   
        Lang.For 
        [
           Lang.Binding [(v, e_1)],
           Lang.OrderBy orderSpecs,
           Lang.Return (Lang.If e_f e_2 Nothing)  
        ]
    )  
        
 
-------------------------------------------------------------------------------
-- FOR - FOR 1
 
nExpr groupByAsMacro env  (Lang.For [Lang.Binding [(v, e1)], Lang.Return e2]) =
    let
        nE1 = nExpr groupByAsMacro env e1
        pNE1 = p env nE1
        env' = [(v, pNE1)] ++ env
        nE2 = nExpr groupByAsMacro env' e2
          
    in
        LangCore.For v nE1 nE2 Nothing  
 

-------------------------------------------------------------------------------
-- FOR - FOR 2
 
nExpr groupByAsMacro env  (Lang.For [Lang.Binding [(v, e1)], Lang.OrderBy orderSpecs, Lang.Return e2]) = 
    
    let
        nE1 = nExpr groupByAsMacro env e1
        pNE1 = p env nE1
        env' = [(v, pNE1)] ++ env
        nE2 = nExpr groupByAsMacro env' e2
        
        orderSpecs' = map f orderSpecs
        f (e, maybeDirection) = (nExpr groupByAsMacro env' e, direction maybeDirection)
        direction dir = case dir of
            Just d -> d
            Nothing -> Ascending 
          
    in
        LangCore.For v nE1 nE2 (Just orderSpecs')  
           
                        




 
-------------------------------------------------------------------------------
-- FOR - WHERE 
nExpr groupByAsMacro env  (Lang.For ((Lang.Binding [(v, e_1)]):(Lang.Where e_2):forClauses)) = 
    
    nExpr groupByAsMacro env 
    (
        
        Lang.For 
        (
           (Lang.Binding [(v, e_1')]):forClauses 
            
        )
        
    )
        where
            e_1' =
                Lang.FunApp "filter" (Lang.InlineLambda ((v, e_2), e_1))
                
                    
                     
    




-------------------------------------------------------------------------------
-- FOR - GROUP BY 
nExpr groupByAsMacro env  (Lang.For ((Lang.Binding [(v, e_1)]):(Lang.GroupBy es_g):forClauses)) = 
    
    nExpr groupByAsMacro env 
    (
        
        Lang.For 
        (
           (Lang.Binding [(v, e_1')]):forClauses 
            
        )
        
    )
        where
            e_1' =
                Lang.FunApp "groupBy" (Lang.InlineLambda((v, (Lang.Tuple es_g)), e_1)) 
                   




-------------------------------------------------------------------------------
-- FOR - Flattening
nExpr groupByAsMacro env  (Lang.For ((Lang.Binding ((v, e):bindings)):forClauses)) = 
    
    nExpr groupByAsMacro env 
    (
        Lang.FunApp "concat"  (Lang.Exprs
        [
            Lang.For 
            [
                Lang.Binding [(v, e)], 
                Lang.Return (Lang.For ((Lang.Binding bindings):forClauses))
            ]
        ])
    )       
   
   
   
   
   
   
   
   
                
nExpr groupByAsMacro env (Lang.Table tblID tableAttSpecs tableKeySpecs []) = nExpr groupByAsMacro env (Lang.Table tblID tableAttSpecs tableKeySpecs tableOrderSpecs)
                                                                 where
                                                                    tableOrderSpecs = map f (head tableKeySpecs)
                                                                    f atrName = (atrName, Nothing)        
                                                                    
nExpr groupByAsMacro env (Lang.Table tblID tableAttSpecs tableKeySpecs tableOrderSpecs) = LangCore.Table tblID tableAttSpecs tableKeySpecs nTableOrderSpecs
        where
            nTableOrderSpecs = map f tableOrderSpecs
            f (attrName, maybeSortDirection) =
                case maybeSortDirection of
                    Just dir -> (attrName, dir)
                    Nothing -> (attrName, Ascending) 
                                                                                                                                           
                                                                 
                                                                    
 
nExpr groupByAsMacro env (Lang.NomAcc e s) = let
                                nE = nExpr groupByAsMacro env e
                                (nest,mE)= p env nE
                              in
                                case lookup s mE of 
                                            Just i    -> LangCore.PosAcc nE i
                                            Nothing   -> error ("table-column " ++ s ++ " unknown2")       
                                            
                                            
                                            
                                            
    
    
    
nExpr groupByAsMacro env  (Lang.FunApp "all" (Lang.Exprs [e])) = 

    nExpr groupByAsMacro env 
    (
		Lang.FunApp "empty"
        (
            Lang.Exprs
            [
                Lang.For 
                [
       		        Binding [("v", e)],
			        Where (Lang.OpApp (Lang.UnOp "not" (Lang.Var "v"))),
			        Return (Lang.Literal (Lang.IntLiteral 1))
                ]
            ]
        )
    )    

nExpr groupByAsMacro env  (Lang.FunApp "any" (Lang.Exprs [e])) = 

   nExpr groupByAsMacro env 
   (
       Lang.OpApp 
       (
           Lang.UnOp "not" 
           ( 
 	           Lang.FunApp "empty"
               (
                   Lang.Exprs
                   [
                       Lang.For 
                       [
      		               Binding [("v", e)],
 		                   Where (Lang.Var "v"),
 		                   Return (Lang.Literal (Lang.IntLiteral 1))
                       ]
                   ]
               )
           )
        )
    )	    
    
    
    
nExpr groupByAsMacro env  (Lang.FunApp "filter" (Lang.InlineLambda ((v, e_1), e_2))) = 
    
    nExpr groupByAsMacro env 
    (
        
        Lang.For 
        [
            Binding [(v, e_2)], 
            Return 
            (
                Lang.If e_1 (Lang.Var v) Nothing     
            )
        ]
        
    )    
         
nExpr groupByAsMacro env  (Lang.FunApp "map" (Lang.InlineLambda ((v, e_1), e_2))) = 
    
    nExpr groupByAsMacro env 
    (
        
        Lang.For 
        [
            Binding [(v, e_2)], 
            Return  e_1
        ]
        
    )    
                                                    

nExpr groupByAsMacro env  (Lang.FunApp "groupBy" (Lang.InlineLambda ((varID, e1), e2))) =
 
    if groupByAsMacro
        then
            nExpr groupByAsMacro env 
            (
                
                Lang.For 
                [
                    Binding [(varID, (Lang.FunApp "groupWith" (Lang.InlineLambda ((varID, e1), e2))))], 
                    Return  (Lang.FunApp "unzip" (Lang.Exprs [Lang.Var varID]))
                ]
                
            ) 
        else
            let
                nE2 = nExpr groupByAsMacro env e2
                pNE2 = p env nE2
                env' = [(varID, pNE2)] ++ env
                nE1 = nExpr groupByAsMacro env' e1
            in
                (LangCore.FunApp "groupBy" (LangCore.InlineLambda ((varID, nE1), nE2)))
    


--nExpr groupByAsMacro env  (Lang.FunApp funID (Lang.Exprs exprs)) = 
--    if(isPrefixOf "zip" funID)
--        then
--            (LangCore.FunApp "zipn" (LangCore.Exprs [ LangCore.Tuple (map (nExpr groupByAsMacro env) exprs) ]  ))
--        else
--            if(isPrefixOf "unzip" funID)
--                then
--                    (LangCore.FunApp "unzipn" (LangCore.Exprs (map (nExpr groupByAsMacro env) exprs)))
--                else    
--                    (LangCore.FunApp funID (LangCore.Exprs (map (nExpr groupByAsMacro env) exprs)))



nExpr groupByAsMacro env  (Lang.FunApp "zip" (Lang.Exprs [e1,e2])) = 
    nExpr groupByAsMacro env  (Lang.FunApp "zip2" (Lang.Exprs [e1,e2])) 

nExpr groupByAsMacro env  (Lang.FunApp "zip" (Lang.Exprs (e1:exprs))) = 
    nExpr groupByAsMacro env  (Lang.FunApp "zip2" (Lang.Exprs [e1, (Lang.FunApp "zip" (Lang.Exprs exprs))])) 


nExpr groupByAsMacro env  (Lang.FunApp funID (Lang.Exprs exprs)) = 
    (LangCore.FunApp funID (LangCore.Exprs (map (nExpr groupByAsMacro env) exprs)))

nExpr groupByAsMacro env  (Lang.FunApp funID (Lang.InlineLambda ((varID, e1), e2))) 
            =   
                let
                    nE2 = nExpr groupByAsMacro env e2
                    pNE2 = p env nE2
                    env' = [(varID, pNE2)] ++ env
                    nE1 = nExpr groupByAsMacro env' e1
                in
                    (LangCore.FunApp funID (LangCore.InlineLambda ((varID, nE1), nE2)))


            
            
            
 
 
nExpr groupByAsMacro env (Lang.Parenthesed e) = LangCore.Parenthesed (nExpr groupByAsMacro env e)                                                                                                          