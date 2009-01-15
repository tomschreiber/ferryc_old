-----------------------------------------------------------------------------------------
{-| Module      : Props.ONE
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Props.ONE where


import Props.COLS
import LangAlgb
import Data.List


one :: AlgbExp -> [AttrName]

one  (ROWNUM (resultAttrName,  _,  _) e) = [resultAttrName] ++ (one e) 

one  (ROWID _ e) = one e
 
one  (ROWRANK (resultAttrName,  _) e) = [resultAttrName] ++ (one e)

one  (RANK _ e) = one e

one  (PROJ projInfos e) = 
                            let
                                onesE = one e
                                onesE' = map fst (filter f projInfos)
                                    where
                                        f (newAttrName, oldAttrName) = elem oldAttrName onesE
                            in
                                onesE'                      
                                
one (SEL _ _) = []

one (POS_SELECT _ _) = []

one (CROSS e1 e2) = (one e1) ++ (one e2)

one (EQJOIN _ _ _) = []
one (SEMIJOIN _ _ _) = []
one (THETAJOIN _ _ _) = []

one (DISJUNION e1 e2) = nub ((one e1) ++ (one e2))

one (DIFFERENCE _ _) = []

one (DISTINCT e) = one e
        

          -- [...,[(AV_NAT 1, AV_NAT 2, ...)],...]    
one (LIT_TBL semanticalInfosLIT_TBL  schemaInfos) =   
                                  -- [(AttrName, AType)]
                        let
                            l1 = zip schemaInfos  (transpose semanticalInfosLIT_TBL) 
                            l2 = map fst (map fst (filter f l1))
                                where
                                    f ((attrName, attrType), attrValues) = ((attrType == AT_NAT) && (elem (AV_NAT 1) attrValues))
                                    
                                    
                        in
                            l2
                            
one (EMPTY_TBL _) = []

one (TABLEREF _) = []

one (ATTACH (resultAttrName, (aType, aValue)) e) = l ++ (one e)
        where l = if(aType == AT_NAT && aValue == (AV_NAT 1)) then [resultAttrName] else []
 
 
one (CAST (resultAttrName, attrName , aType) e) = let
                                                    oneE = one e
                                                    l = if(aType == AT_NAT && (elem attrName oneE)) then [resultAttrName] else []
                                                  in l ++ oneE
                                                  
                                                        
                                                            
one  _ = []       
