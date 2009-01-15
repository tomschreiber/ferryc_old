-----------------------------------------------------------------------------------------
{-| Module      : Props.ONE
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Props.UNIQUE where

import Props.ONE
import Props.DENSE
import Props.COLS


import LangAlgb
import Data.List


unique :: AlgbExp -> [AttrName]

unique  (ROWNUM (resultAttrName,  _,  _) e) = [resultAttrName] ++ (unique e) 

unique  (ROWID _ e) = unique e
 
unique  (ROWRANK (resultAttrName,  sortInfos) e) = list ++ unique e
                where
                    list =  if length sortInfos == 1
                                then
                                    let 
                                        (sortAttrName, sortDir) = head sortInfos
                                    in
                                        if 
                                            sortDir == Ascending && 
                                            (elem sortAttrName (unique e)) 
                                        then
                                            [resultAttrName]
                                        else
                                            []
                                else
                                    []
                    

unique  (RANK _ e) = unique e

unique  (PROJ projInfos e) = 
                            let
                                uniquesE = unique e
                                uniquesE' = map fst (filter f projInfos)
                                    where
                                        f (newAttrName, oldAttrName) = elem oldAttrName uniquesE
                            in
                                uniquesE'                      
                                
unique (SEL _ e) = unique e
unique (POS_SELECT _ e) = cols e

unique (CROSS e1 e2) = []

unique (EQJOIN _ _ _) = []
unique (SEMIJOIN _ _ _) = []
unique (THETAJOIN _ _ _) = []

unique (DISJUNION e1 e2) = []

unique (DIFFERENCE _ _) = []

unique (DISTINCT e) = list
        where
            list =
                let
                    uniqueE = unique e
                    colsE = cols e
                in
                    if (length colsE == 1)
                        then
                            let 
                                headColsE = head colsE
                            in
                                if (not(elem headColsE uniqueE))
                                    then
                                        if 
                                            elem headColsE (one e) &&
                                            elem headColsE (dense e)
                                        then
                                            [headColsE]
                                        else
                                            uniqueE
                                    else
                                        uniqueE     
                        else
                            uniqueE



          -- [...,[(AV_NAT 1, AV_NAT 2, ...)],...]    
unique (LIT_TBL semanticalInfosLIT_TBL  schemaInfos) =   
                                  -- [(AttrName, AType)]
                        let
                            l1 = zip schemaInfos  (transpose semanticalInfosLIT_TBL) 
                            l2 = map fst (map fst (filter f l1))
                            f ((attrName, attrType), attrValues) = if(attrType == AT_NAT)
                                                                        then
                                                                            uniqueValues attrValues
                                                                        else
                                                                            False    
                        in
                            l2   
                            
unique (EMPTY_TBL _) = []


unique (TABLEREF (_, _, keyInfos)) = list
                    where
                        singleKeys = filter f keyInfos
                        f keys = length keys == 1
                        list = concat singleKeys
                        

unique (ATTACH (resultAttrName, (aType, aValue)) e) = unique e
        
 
unique (CAST (resultAttrName, attrName , aType) e) = unique e
                                                  
                                                        
                                                            
unique  _ = []       




uniqueValues values = (nub values) == values
                           
                   