-----------------------------------------------------------------------------------------
{-| Module      : Algb2DOT
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Algb2XML(

-- =============================================================================
-- exports
-- =============================================================================
    
    algbExp2XML    


) where


-- =============================================================================
-- imports
-- =============================================================================

import LangAlgb(AlgbExp (..))

import Algb2DAG(AlgbExpDAGMemorizer, algbExp2DAGCustom)



import Algb2XMLUtils(getNodeKindName,getEdgeElements, getPropertiesElement, getSemanticContentElement)

import AttributeNameTransformation(AttributeNameTransformer)


import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.TypeDefs



-- =============================================================================
-- functions
-- =============================================================================


----------------------------------------------------------------------
----------------------------------------------------------------------
algbExp2XML ::  ArrowXml a =>  AlgbExp -> AlgbExpDAGMemorizer -> AttributeNameTransformer -> a XmlTree XmlTree
----------------------------------------------------------------------
----------------------------------------------------------------------

algbExp2XML ae algbExpDAGMemorizer ant = 


    mkelem "logical_query_plan"[sattr "unique_names" "true"]
            nodeElements


        where
        
            
                            
            (_id, aes) = algbExp2DAGCustom ae algbExpDAGMemorizer
            --(_id, aes) = algbExp2PseudoDAG ae
            
            l = zip [0..] aes
                    
            nodeElements = map f l
                where
            
                    f (id, ae)=
                         mkelem "node"[sattr "id" (show id), sattr "kind" (getNodeKindName ae)]
                            (      
                                   propertiesElement
                                ++ semanticContentElement
                                ++ edgeElements    
                            )
                            where
                                propertiesElement = case getPropertiesElement ae ant of
                                    Just e  -> [e]
                                    Nothing -> []   
                                
                                semanticContentElement = case getSemanticContentElement ae ant of
                                    Just e  -> [e]
                                    Nothing -> []   
                             
                                edgeElements = getEdgeElements ae
                                                   
                                             
                            