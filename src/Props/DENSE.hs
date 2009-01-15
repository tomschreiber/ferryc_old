-----------------------------------------------------------------------------------------
{-| Module      : Props.ONE
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Props.DENSE where



import LangAlgb
import Data.List
import Props.COLS


dense :: AlgbExp -> [AttrName]

dense  (ROWNUM (resultAttrName,  _,  _) e) = [resultAttrName] ++ (dense e) 

dense  (ROWID _ e) = dense e
 
dense  (ROWRANK (resultAttrName,  _) e) = [resultAttrName] ++ dense e

dense  (RANK _ e) = dense e

dense  (PROJ projInfos e) = 
                            let
                                densesE = dense e
                                densesE' = map fst (filter f projInfos)
                                    where
                                        f (newAttrName, oldAttrName) = elem oldAttrName densesE
                            in
                                densesE'                      
                                
dense (SEL _ _) = []

dense (POS_SELECT _ e) = cols e


dense (CROSS e1 e2) = []

dense (EQJOIN _ _ _) = []
dense (SEMIJOIN _ _ _) = []
dense (THETAJOIN _ _ _) = []

dense (DISJUNION e1 e2) = []

dense (DIFFERENCE _ _) = []

dense (DISTINCT e) = dense e

          -- [...,[(AV_NAT 1, AV_NAT 2, ...)],...]    
dense (LIT_TBL semanticalInfosLIT_TBL  schemaInfos) =   
                                  -- [(AttrName, AType)]
                        let
                            l1 = zip schemaInfos  (transpose semanticalInfosLIT_TBL) 
                            l2 = map fst (map fst (filter f l1))
                           -- f ((attrName, attrType), attrValues) = denseValues attrValues
                            f ((attrName, attrType), attrValues) = if(attrType == AT_NAT)
                                                                        then
                                                                            snd (denseValues attrValues)
                                                                        else
                                                                            False    
                        in
                            l2    
                            
dense (EMPTY_TBL _) = []

dense (TABLEREF _) = []

dense (ATTACH (resultAttrName, (aType, aValue)) e) = dense e
        
 
dense (CAST (resultAttrName, attrName , aType) e) = dense e
                                                  
                                                        
                                                            
dense  _ = []       




denseValues values =
                    let
                        
                        values' = sort values
                        
                        f (maybeVal, stillDenseAscending) (AV_NAT val) = case (maybeVal, stillDenseAscending) of
                                                    (Nothing,_) -> (Just (AV_NAT val), True)
                                                    (_, False)  -> (maybeVal, False)
                                                    (Just (AV_NAT previousVal), True) -> (Just (AV_NAT val), ((previousVal == val) || (previousVal + 1 == val)))
                                
                                
                    in
                        foldl f (Nothing, True) values            
                   