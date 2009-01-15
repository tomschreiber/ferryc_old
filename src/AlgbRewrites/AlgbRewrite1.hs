-----------------------------------------------------------------------------------------
{-| Module      : Props.ONE
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module AlgbRewrites.AlgbRewrite1 where



import LangAlgb
import Data.List

import Props.ONE
import Props.DENSE
import Props.UNIQUE
import Props.COLS


import Core2AlgbUtils

rewrite :: AlgbExp -> AlgbExp



rewrite  (ROWNUM (resultAttrName,  [(sortAttrName, Ascending)],  Nothing) e) = 
     let
        e' = rewrite e
     in   
        if      (elem sortAttrName (one e')) && 
                (elem sortAttrName (dense e')) && 
                (elem sortAttrName (unique e'))
             then
                let
                    colsE' = cols e'
                    projectionInfos = ([(resultAttrName,sortAttrName)] ++ asProjList1 colsE')
                in
                    (PROJ  projectionInfos e')
                      
             
             else
                ROWNUM (resultAttrName,  [(sortAttrName, Ascending)],  Nothing) e' 



rewrite  (ROWNUM (resultAttrName,  [],  Nothing) e) = 
     let
        e' = rewrite e
        colsE' = cols e'
        col = find f colsE'
        f c = 
            (elem c (one e')) && 
            (elem c (dense e')) && 
            (elem c (unique e'))     
     in   
        
        case col of
            Nothing -> ROWNUM (resultAttrName,  [],  Nothing) e'
            Just c  ->
                        let
                            projectionInfos = ([(resultAttrName,c)] ++ asProjList1 colsE')
                        in
                            (PROJ  projectionInfos e')       
        
       
rewrite  (ROWNUM semInfos e) = (ROWNUM semInfos (rewrite e))    
                             




rewrite  (ROWID semInfos e) = (ROWID semInfos (rewrite e))
 




rewrite  (ROWRANK (resultAttrName,  [(sortAttrName, Ascending)]) e) = 
            let
                e' = rewrite e
            in    
                 if
                    (elem sortAttrName (one e')) && 
                    (elem sortAttrName (dense e')) 
                 then
                    let
                        colsE' = cols e'
                        projectionInfos = ([(resultAttrName,sortAttrName)] ++ asProjList1 colsE')
                    in
                        (PROJ  projectionInfos e') 
                       
                 
                 else
                    (ROWRANK (resultAttrName,   [(sortAttrName, Ascending)]) e')
                                    
                        
                   
                             

rewrite  (ROWRANK semInfos e) = (ROWRANK semInfos (rewrite e))






rewrite  (RANK semInfos e) = (RANK semInfos (rewrite e))


rewrite  (PROJ projInfos e) = (PROJ projInfos (rewrite e))
                                            
                                
rewrite (SEL semInfos e) = (SEL semInfos (rewrite e))
rewrite (POS_SELECT semInfos e) = (POS_SELECT semInfos (rewrite e))


rewrite (CROSS e1 e2) = (CROSS (rewrite e1) (rewrite e2)) 

rewrite (EQJOIN semInfos e1 e2) = (EQJOIN semInfos (rewrite e1) (rewrite e2))
rewrite (SEMIJOIN semInfos e1 e2) = (SEMIJOIN semInfos (rewrite e1) (rewrite e2))
rewrite (THETAJOIN semInfos e1 e2) = (THETAJOIN semInfos (rewrite e1) (rewrite e2))

rewrite (DISJUNION e1 e2) = (DISJUNION (rewrite e1) (rewrite e2))

rewrite (DIFFERENCE e1 e2) = (DIFFERENCE (rewrite e1) (rewrite e2))

rewrite (DISTINCT e) = (DISTINCT (rewrite e))

          
rewrite (LIT_TBL semanticalInfosLIT_TBL  schemaInfos) = (LIT_TBL semanticalInfosLIT_TBL  schemaInfos)  
                         
                            
rewrite (EMPTY_TBL semInfos) = (EMPTY_TBL semInfos)

rewrite (TABLEREF semInfos) = (TABLEREF semInfos)

rewrite (ATTACH semInfos e) = (ATTACH semInfos (rewrite e))
        
 
rewrite (CAST semInfos e) = (CAST semInfos (rewrite e))
                                                  
                                                        
                                                            
rewrite  (FUN_NUM_EQ semInfos e) = (FUN_NUM_EQ semInfos (rewrite e))      
rewrite  (FUN_NUM_GT semInfos e) = (FUN_NUM_GT semInfos (rewrite e))      
rewrite  (FUN_1TO1 semInfos e) = (FUN_1TO1 semInfos (rewrite e))     
rewrite  (FUN_BOOL_AND semInfos e) = (FUN_BOOL_AND semInfos (rewrite e))     
rewrite  (FUN_BOOL_OR semInfos e) = (FUN_BOOL_OR semInfos (rewrite e))      
rewrite  (FUN_BOOL_NOT semInfos e) = (FUN_BOOL_NOT semInfos (rewrite e))     

rewrite  (FUN_AGGR semInfos e) = (FUN_AGGR semInfos (rewrite e))      
rewrite  (FUN_AGGR_COUNT semInfos e) = (FUN_AGGR_COUNT semInfos (rewrite e)) 


rewrite (SERIALIZE_REL semInfos e1 e2) = (SERIALIZE_REL semInfos (rewrite e1) (rewrite e2))

rewrite (NIL) = (NIL)


rewrite _ = error("OOPS")                                                               

     



