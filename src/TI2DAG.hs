-----------------------------------------------------------------------------------------
{-| Module      : TI2DAG
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module TI2DAG(
-- =============================================================================
-- exports
-- =============================================================================
    TIDAGMemorizer,
    
    tiPseudoDAGMemorizer,
    
    ti2DAGCustom


) where



-- =============================================================================
-- imports
-- =============================================================================

import Core2Algb(TableInfos (..))
import LangAlgb(AttrName)




-- =============================================================================
-- types
-- =============================================================================


type TIDAGMemorizer = TableInfos -> [TableInfos] -> (Int, [TableInfos])


-- =============================================================================
-- functions
-- =============================================================================


----------------------------------------------------------------------
------------------------------------------------------------------------
-- compute a pseudo-DAG from TableInfos tree
tiPseudoDAGMemorizer :: TIDAGMemorizer
----------------------------------------------------------------------
----------------------------------------------------------------------

tiPseudoDAGMemorizer t ts = (length ts, ts ++ [t])


----------------------------------------------------------------------
----------------------------------------------------------------------
ti2DAGCustom :: TableInfos -> TIDAGMemorizer ->  (Int, [TableInfos])
----------------------------------------------------------------------
----------------------------------------------------------------------

ti2DAGCustom ti memo = _ti2DAGCustom ti [] 
    where
        ----------------------------------------------------------------------
        ----------------------------------------------------------------------
        -- dag a ts:
        --  (1) turn arguments of a into DAG
        --  (2) check if a (with arguments replaced by DAGs) is already memoized in ts
        --  (3) return a's position in the list of memoized nodes ts and new ts
        _ti2DAGCustom :: TableInfos ->  [TableInfos] -> (Int, [TableInfos])
        ----------------------------------------------------------------------
    
    
                    -- TI (AlgbExp, [AttrName], [(AttrName, TableInfos)])
        _ti2DAGCustom (TI(a, cols, subs)) ts = memo (TI(a, cols, subs'')) ts'
            where
                
                -- associate each TableInfos-node in subs with the corresponding DAGNodeID-node-id
                subs' = zip subs ids
                
                -- replace each TableInfos-node in subs with an corresponding DAGNodeID-node
                subs'' = map f subs'
                    where
                        ----------------------------------------------------------------------
                        f :: ((AttrName, TableInfos), Int) -> (AttrName, TableInfos)
                        ----------------------------------------------------------------------
                        f ((c, _ti), id) = (c, TIDAGNodeID id)
                
             
                -- recursive (postorder traversal) calculation of DAGNodeID-node-ids for each TableInfos-node in subs
                (ts', ids) = foldl f (ts, []) subs
                    where
                
                        f (ts, ids) (_c, ti) = (ts', ids ++ [id]) 
                            where
                                (id, ts') = _ti2DAGCustom ti ts


