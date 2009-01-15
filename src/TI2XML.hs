-----------------------------------------------------------------------------------------
{-| Module      : Algb2Dot
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module TI2XML (

-- =============================================================================
-- exports
-- =============================================================================
    ti2XML

) where


-- =============================================================================
-- imports
-- =============================================================================

import Core2Algb(TableInfos (..))


import TI2DAG(TIDAGMemorizer, ti2DAGCustom)
import Algb2DAG(AlgbExpDAGMemorizer)



import TI2XMLUtils(getColsElement, getSubsElement)
import Algb2XML(algbExp2XML)

import AttributeNameTransformation (AttributeNameTransformer)


import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.TypeDefs


-- =============================================================================
-- functions
-- =============================================================================


----------------------------------------------------------------------
----------------------------------------------------------------------

ti2XML :: ArrowXml a =>  TableInfos -> TIDAGMemorizer -> AlgbExpDAGMemorizer -> AttributeNameTransformer -> a XmlTree XmlTree
----------------------------------------------------------------------
----------------------------------------------------------------------

ti2XML ti tiDAGMemorizer algbExpDAGMemorizer ant = 


     mkelem "query_plan"[]
            tiElements

    
    where
    
        
        (_id, tis) = ti2DAGCustom ti tiDAGMemorizer
        
        tiElements = map f (zip [0..] tis)
            where
        
                f (id, TI(ae, cols, subs)) =
                    mkelem "table_infos"[sattr "id" (show id)]
                    [
                        algbExp2XML ae algbExpDAGMemorizer ant,
                        getColsElement (TI(ae, cols, subs)),
                        getSubsElement (TI(ae, cols, subs))    
                    ]
                         
                        
                        