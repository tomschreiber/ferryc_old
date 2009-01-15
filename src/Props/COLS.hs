-----------------------------------------------------------------------------------------
{-| Module      : Props.ONE
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Props.COLS where



import LangAlgb
import Data.List


cols :: AlgbExp -> [AttrName]

cols  (ROWNUM (resultAttrName,  _,  _) e) = [resultAttrName] ++ (cols e) 

cols  (ROWID resultAttrName e) = [resultAttrName] ++ (cols e) 
 
cols  (ROWRANK (resultAttrName,  _) e) = [resultAttrName] ++ (cols e)

cols  (RANK (resultAttrName,  _) e) = [resultAttrName] ++ (cols e)

cols  (PROJ projInfos e) = map fst projInfos
                                                  
                                
cols (SEL _ e) = cols e
cols (POS_SELECT _ e) = cols e

cols (CROSS e1 e2) = (cols e1) ++ (cols e2)

cols (EQJOIN _ e1 e2) = (cols e1) ++ (cols e2)
cols (SEMIJOIN _ e1 e2) = (cols e1)
cols (THETAJOIN _ e1 e2) = (cols e1) ++ (cols e2)

cols (DISJUNION e1 e2) = cols e1

cols (DIFFERENCE e1 e2) = cols e1

cols (DISTINCT e) = cols e

          -- [...,[(AV_NAT 1, AV_NAT 2, ...)],...]    
cols (LIT_TBL _semanticalInfosLIT_TBL  schemaInfos) =   
                                  -- [(AttrName, AType)]
                        map fst schemaInfos
                            
cols (EMPTY_TBL _) = []


                -- (external attribute name, internal attribute name, attribute type)
                -- [(AttrName, AttrName, AType)]
cols (TABLEREF (_, tblAttributeInfos, _)) = map f tblAttributeInfos
                                                where
                                                    f (ae, ai, at) = ai

cols (ATTACH (resultAttrName, _) e) = [resultAttrName] ++ (cols e)

        
 
cols (CAST (resultAttrName, _attrName , _aType) e) = [resultAttrName] ++ (cols e)
                                                  
                                                        
                                                            
cols  (FUN_NUM_EQ (resultAttrName, _) e) = [resultAttrName] ++ (cols e)      
cols  (FUN_NUM_GT (resultAttrName, _) e) = [resultAttrName] ++ (cols e)      
cols  (FUN_1TO1 (_, resultAttrName, _) e) = [resultAttrName] ++ (cols e)      
cols  (FUN_BOOL_AND (resultAttrName, _) e) = [resultAttrName] ++ (cols e)      
cols  (FUN_BOOL_OR (resultAttrName, _) e) = [resultAttrName] ++ (cols e)      
cols  (FUN_BOOL_NOT (resultAttrName, _) e) = [resultAttrName] ++ (cols e)      

cols  (FUN_AGGR (_, (resultAttrName, _), _) e) = [resultAttrName] ++ (cols e)      
cols  (FUN_AGGR_COUNT (resultAttrName, _) e) = [resultAttrName] ++ (cols e) 




cols _ = error("OOPS")     
