-----------------------------------------------------------------------------------------
{-| Module      : Props.ONE
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module AlgbRewrites.AlgbRewrite2 where



import LangAlgb
import Data.List

import Props.ONE
import Props.DENSE
import Props.UNIQUE
import Props.COLS


import Core2AlgbUtils

rewrite :: AlgbExp -> AlgbExp



rewrite  (ROWNUM semInfos e) =
    let
        e' = rewrite e
    in
        case e' of
            EMPTY_TBL schema -> EMPTY_TBL schema
            _                -> ROWNUM semInfos e'          



rewrite  (ROWID semInfos e) = 
    let
        e' = rewrite e
    in
        case e' of
            EMPTY_TBL schema -> EMPTY_TBL schema
            _                -> ROWID semInfos e'       
 


rewrite  (ROWRANK semInfos e) = 
    let
        e' = rewrite e
    in
        case e' of
            EMPTY_TBL schema -> EMPTY_TBL schema
            _                -> ROWRANK semInfos e'       



rewrite  (RANK semInfos e)  = 
    let
        e' = rewrite e
    in
        case e' of
            EMPTY_TBL schema -> EMPTY_TBL schema
            _                -> RANK semInfos e'   



rewrite  (PROJ semInfos e) = 
    let
        e' = rewrite e
    in
        case e' of
            EMPTY_TBL schema -> EMPTY_TBL schema
            _                -> (PROJ semInfos e')   
                                            
 
                                
rewrite (SEL semInfos e)  = 
    let
        e' = rewrite e
    in
        case e' of
            EMPTY_TBL schema -> EMPTY_TBL schema
            _                -> (SEL semInfos e') 
            
rewrite (POS_SELECT semInfos e)  = 
    let
        e' = rewrite e
    in
        case e' of
            EMPTY_TBL schema -> EMPTY_TBL schema
            _                -> (POS_SELECT semInfos e') 


rewrite (CROSS e1 e2)  = 
    let
        e1' = rewrite e1
        e2' = rewrite e2
        
        maybeEmptyTable = 
            case e1' of
                EMPTY_TBL schema -> Just (EMPTY_TBL schema)
                _                -> 
                            case e2' of
                                EMPTY_TBL schema -> Just (EMPTY_TBL schema)
                                _                -> Nothing              
    in
        case maybeEmptyTable of
            Just (EMPTY_TBL schema) -> EMPTY_TBL schema
            _                       -> CROSS e1' e2'
           



rewrite (EQJOIN semInfos e1 e2)  = 
    let
        e1' = rewrite e1
        e2' = rewrite e2
        
        maybeEmptyTable = 
            case e1' of
                EMPTY_TBL schema -> Just (EMPTY_TBL schema)
                _                -> 
                            case e2' of
                                EMPTY_TBL schema -> Just (EMPTY_TBL schema)
                                _                -> Nothing              
    in
        case maybeEmptyTable of
            Just (EMPTY_TBL schema) -> EMPTY_TBL schema
            _                       -> EQJOIN semInfos e1' e2'
           



rewrite (SEMIJOIN semInfos e1 e2)  = 
    let
        e1' = rewrite e1
        e2' = rewrite e2
        
        maybeEmptyTable = 
            case e1' of
                EMPTY_TBL schema -> Just (EMPTY_TBL schema)
                _                -> 
                            case e2' of
                                EMPTY_TBL schema -> Just (EMPTY_TBL schema)
                                _                -> Nothing              
    in
        case maybeEmptyTable of
            Just (EMPTY_TBL schema) -> EMPTY_TBL schema
            _                       -> SEMIJOIN semInfos e1' e2'
           


rewrite (THETAJOIN semInfos e1 e2)  = 
    let
        e1' = rewrite e1
        e2' = rewrite e2
        
        maybeEmptyTable = 
            case e1' of
                EMPTY_TBL schema -> Just (EMPTY_TBL schema)
                _                -> 
                            case e2' of
                                EMPTY_TBL schema -> Just (EMPTY_TBL schema)
                                _                -> Nothing              
    in
        case maybeEmptyTable of
            Just (EMPTY_TBL schema) -> EMPTY_TBL schema
            _                       -> THETAJOIN semInfos e1' e2'


rewrite (DISJUNION e1 e2)  = 
    let
        e1' = rewrite e1
        e2' = rewrite e2
        
        maybeEmptyTable1 = 
            case e1' of
                EMPTY_TBL schema -> Just (EMPTY_TBL schema)
                _                -> Nothing 
                
        maybeEmptyTable2 = 
            case e2' of
                EMPTY_TBL schema -> Just (EMPTY_TBL schema)
                _                -> Nothing 
                    
    in
        case (maybeEmptyTable1, maybeEmptyTable2) of
            (Just (EMPTY_TBL schema1), Just (EMPTY_TBL schema2)) -> EMPTY_TBL schema1
            (Just (EMPTY_TBL schema), Nothing) -> e2'
            (Nothing, Just (EMPTY_TBL schema)) -> e1'
            (Nothing, Nothing) -> DISJUNION e1' e2'


rewrite (DIFFERENCE e1 e2)  = 
    let
        e1' = rewrite e1
        e2' = rewrite e2
        
        maybeEmptyTable1 = 
            case e1' of
                EMPTY_TBL schema -> Just (EMPTY_TBL schema)
                _                -> Nothing 
                
        maybeEmptyTable2 = 
            case e2' of
                EMPTY_TBL schema -> Just (EMPTY_TBL schema)
                _                -> Nothing 
                    
    in
        case (maybeEmptyTable1, maybeEmptyTable2) of
            (Just (EMPTY_TBL schema1), Just (EMPTY_TBL schema2)) -> EMPTY_TBL schema1
            (Just (EMPTY_TBL schema), Nothing) -> EMPTY_TBL schema
            (Nothing, Just (EMPTY_TBL schema)) -> e1'
            (Nothing, Nothing) -> DIFFERENCE e1' e2'


rewrite (DISTINCT e)  = 
    let
        e' = rewrite e
    in
        case e' of
            EMPTY_TBL schema -> EMPTY_TBL schema
            _                -> (DISTINCT e') 

          
rewrite (LIT_TBL semanticalInfosLIT_TBL  schemaInfos) = (LIT_TBL semanticalInfosLIT_TBL  schemaInfos)  
                         
                            
rewrite (EMPTY_TBL semInfos) = (EMPTY_TBL semInfos)


rewrite (TABLEREF semInfos) = (TABLEREF semInfos)


rewrite (ATTACH semInfos e)   = 
    let
        e' = rewrite e
    in
        case e' of
            EMPTY_TBL schema -> EMPTY_TBL schema
            _                -> (ATTACH semInfos e') 
        
 
 
rewrite (CAST semInfos e)   = 
    let
        e' = rewrite e
    in
        case e' of
            EMPTY_TBL schema -> EMPTY_TBL schema
            _                -> (CAST semInfos e') 
                                                  
                                                        
                                                            
rewrite  (FUN_NUM_EQ semInfos e)   = 
    let
        e' = rewrite e
    in
        case e' of
            EMPTY_TBL schema -> EMPTY_TBL schema
            _                -> (FUN_NUM_EQ semInfos  e') 
                                                      




rewrite  (FUN_NUM_GT semInfos e)   = 
    let
        e' = rewrite e
    in
        case e' of
            EMPTY_TBL schema -> EMPTY_TBL schema
            _                -> (FUN_NUM_GT semInfos  e') 
            
                  
rewrite  (FUN_1TO1 semInfos e)   = 
    let
        e' = rewrite e
    in
        case e' of
            EMPTY_TBL schema -> EMPTY_TBL schema
            _                -> (FUN_1TO1 semInfos  e')  
               
rewrite  (FUN_BOOL_AND semInfos e)   = 
    let
        e' = rewrite e
    in
        case e' of
            EMPTY_TBL schema -> EMPTY_TBL schema
            _                -> (FUN_BOOL_AND semInfos  e') 
                 
rewrite  (FUN_BOOL_OR semInfos e)   = 
    let
        e' = rewrite e
    in
        case e' of
            EMPTY_TBL schema -> EMPTY_TBL schema
            _                -> (FUN_BOOL_OR semInfos  e') 
            
                
rewrite  (FUN_BOOL_NOT semInfos e)   = 
    let
        e' = rewrite e
    in
        case e' of
            EMPTY_TBL schema -> EMPTY_TBL schema
            _                -> (FUN_BOOL_NOT semInfos  e')     

rewrite  (FUN_AGGR semInfos e)   = 
    let
        e' = rewrite e
    in
        case e' of
            EMPTY_TBL schema -> EMPTY_TBL schema
            _                -> (FUN_AGGR semInfos  e') 
              
rewrite  (FUN_AGGR_COUNT semInfos e)   = 
    let
        e' = rewrite e
    in
        case e' of
            EMPTY_TBL schema -> EMPTY_TBL schema
            _                -> (FUN_AGGR_COUNT semInfos  e') 


rewrite (SERIALIZE_REL semInfos e1 e2) = (SERIALIZE_REL semInfos (rewrite e1) (rewrite e2))

rewrite (NIL) = (NIL)


rewrite _ = error("OOPS")                                                               

     



