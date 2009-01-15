-----------------------------------------------------------------------------------------
{-| Module      : Props.ONE
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Props.SCHEMA where



import LangAlgb
import Data.List


schema :: AlgbExp -> [(AttrName, AType)]


schema  (ROWNUM (resultAttrName,  _,  _) e) = [(resultAttrName, AT_NAT)] ++ (schema e) 

schema  (ROWID resultAttrName e) = [(resultAttrName, AT_NAT)] ++ (schema e) 
 
schema  (ROWRANK (resultAttrName,  _) e) = [(resultAttrName, AT_NAT)] ++ (schema e)

schema  (RANK (resultAttrName,  _) e) = [(resultAttrName, AT_NAT)] ++ (schema e)

schema  (PROJ projInfos e) = 
                                let
                                    schema_e = schema e
                                
                                    schemaPROJ = map f projInfos
                                       
                                    f (newAttrName, oldAttrName) = 
                                        (newAttrName, aType)
                                            where
                                                Just aType = lookup oldAttrName schema_e   
                                in
                                    schemaPROJ    
                                                          
                                
schema (SEL _ e) = schema e
schema (POS_SELECT _ e) = schema e

schema (CROSS e1 e2) = (schema e1) ++ (schema e2)

schema (EQJOIN _ e1 e2) = (schema e1) ++ (schema e2)
schema (SEMIJOIN _ e1 e2) = (schema e1)
schema (THETAJOIN _ e1 e2) = (schema e1) ++ (schema e2)

schema (DISJUNION e1 e2) = schema e1

schema (DIFFERENCE e1 e2) = schema e1

schema (DISTINCT e) = schema e

schema (LIT_TBL _semanticalInfosLIT_TBL  schemaInfos) = schemaInfos

                            
schema (EMPTY_TBL _) = []


                -- (external attribute name, internal attribute name, attribute type)
                -- [(AttrName, AttrName, AType)]
schema (TABLEREF (_, tblAttributeInfos, _)) = map f tblAttributeInfos
                                                where
                                                    f (ae, ai, at) = (ai, at)




schema (ATTACH (resultAttrName, (aType, _aValue)) e) = [(resultAttrName, aType)] ++ (schema e)

        
 
schema (CAST (resultAttrName, _attrName , aType) e) = [(resultAttrName, aType)] ++ (schema e)
                                                  
                                                        
                                                            
schema  (FUN_NUM_EQ (resultAttrName, _) e) = [(resultAttrName, AT_BOOL)] ++ (schema e)      
schema  (FUN_NUM_GT (resultAttrName, _) e) = [(resultAttrName, AT_BOOL)]  ++ (schema e)      

schema  (FUN_1TO1 (_functionType1TO1, resultAttrName, args) e) = [(resultAttrName, AT_INT)] ++ (schema e)      

schema  (FUN_BOOL_AND (resultAttrName, _) e) = [(resultAttrName, AT_BOOL)]  ++ (schema e)      
schema  (FUN_BOOL_OR (resultAttrName, _) e) = [(resultAttrName, AT_BOOL)]  ++ (schema e)      
schema  (FUN_BOOL_NOT (resultAttrName, _) e) = [(resultAttrName, AT_BOOL)]  ++ (schema e)      



schema  (FUN_AGGR (functionTypeAGGR, (resultAttrName, attrName), _) e) = [(resultAttrName, aType)]  ++ schema_e
    where
        schema_e = schema e 
        Just aType = lookup attrName schema_e       


schema  (FUN_AGGR_COUNT (resultAttrName, _) e) = [(resultAttrName, AT_INT)]  ++ (schema e) 



schema (SERIALIZE_REL _ e1 _e2) = schema e1 

schema (NIL) = []

schema _ = error("OOPS")     
