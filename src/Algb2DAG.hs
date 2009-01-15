-----------------------------------------------------------------------------------------
{-| Module      : Algb2DAG
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Algb2DAG (
-- =============================================================================
-- exports
-- =============================================================================
    AlgbExpDAGMemorizer,
    
    algbExpDAGMemorizer,
    algbExpPseudoDAGMemorizer,
    
    algbExp2DAGCustom

) where


-- =============================================================================
-- imports
-- =============================================================================

import LangAlgb(AlgbExp (..))


import List (elemIndex)


-- =============================================================================
-- types
-- =============================================================================


type AlgbExpDAGMemorizer = AlgbExp -> [AlgbExp] -> (Int, [AlgbExp])


-- =============================================================================
-- functions
-- =============================================================================


----------------------------------------------------------------------
------------------------------------------------------------------------
-- compute a DAG from algebra expression tree
algbExpDAGMemorizer :: AlgbExpDAGMemorizer
----------------------------------------------------------------------
----------------------------------------------------------------------

algbExpDAGMemorizer a ts = case elemIndex a ts of
	            Just i  -> (i,         ts)
	            Nothing -> (length ts, ts ++ [a])



----------------------------------------------------------------------
------------------------------------------------------------------------
-- compute a pseudo-DAG from algebra expression tree
algbExpPseudoDAGMemorizer :: AlgbExpDAGMemorizer
----------------------------------------------------------------------
----------------------------------------------------------------------

algbExpPseudoDAGMemorizer a ts = (length ts, ts ++ [a])






----------------------------------------------------------------------
----------------------------------------------------------------------
--algbExp2DAG :: AlgbExp -> (Int, [AlgbExp])
----------------------------------------------------------------------
----------------------------------------------------------------------
--algbExp2DAG e = algbExp2DAGCustom e algbExpDAGMemorizer




----------------------------------------------------------------------
----------------------------------------------------------------------
--algbExp2PseudoDAG :: AlgbExp -> (Int, [AlgbExp])
----------------------------------------------------------------------
----------------------------------------------------------------------
--algbExp2PseudoDAG e = algbExp2DAGCustom e algbExpPseudoDAGMemorizer




----------------------------------------------------------------------
----------------------------------------------------------------------
algbExp2DAGCustom :: AlgbExp -> AlgbExpDAGMemorizer ->  (Int, [AlgbExp])
----------------------------------------------------------------------
----------------------------------------------------------------------

algbExp2DAGCustom e memo = _algbExp2DAGCustom e [] 
    where
        ----------------------------------------------------------------------
        ----------------------------------------------------------------------
        -- dag a ts:
        --  (1) turn arguments of a into DAG
        --  (2) check if a (with arguments replaced by DAGs) is already memoized in ts
        --  (3) return a's position in the list of memoized nodes ts and new ts
        _algbExp2DAGCustom :: AlgbExp ->  [AlgbExp] -> (Int, [AlgbExp])
        ----------------------------------------------------------------------


        _algbExp2DAGCustom (ROWNUM semInfos e) ts = memo (ROWNUM semInfos (ADAGNodeID dagNodeID_e)) ts'
            where
                (dagNodeID_e, ts') = _algbExp2DAGCustom e ts


        _algbExp2DAGCustom (ROWID semInfos e) ts = memo (ROWID semInfos (ADAGNodeID dagNodeID_e)) ts'
            where
                (dagNodeID_e, ts') = _algbExp2DAGCustom e ts

        _algbExp2DAGCustom (ROWRANK semInfos e) ts = memo (ROWRANK semInfos (ADAGNodeID dagNodeID_e)) ts'
            where
                (dagNodeID_e, ts') = _algbExp2DAGCustom e ts

        
        _algbExp2DAGCustom (RANK semInfos e) ts = memo (RANK semInfos (ADAGNodeID dagNodeID_e)) ts'
            where
                (dagNodeID_e, ts') = _algbExp2DAGCustom e ts


        _algbExp2DAGCustom (PROJ semInfos e) ts = memo (PROJ semInfos (ADAGNodeID dagNodeID_e)) ts'
            where
                (dagNodeID_e, ts') = _algbExp2DAGCustom e ts


        _algbExp2DAGCustom (SEL semInfos e) ts = memo (SEL semInfos (ADAGNodeID dagNodeID_e)) ts'
            where
                (dagNodeID_e, ts') = _algbExp2DAGCustom e ts

        _algbExp2DAGCustom (POS_SELECT semInfos e) ts = memo (POS_SELECT semInfos (ADAGNodeID dagNodeID_e)) ts'
            where
                (dagNodeID_e, ts') = _algbExp2DAGCustom e ts



        _algbExp2DAGCustom (CROSS e1 e2) ts = memo (CROSS (ADAGNodeID dagNodeID_e1) (ADAGNodeID dagNodeID_e2)) ts''
            where
                (dagNodeID_e1, ts')  = _algbExp2DAGCustom e1 ts
                (dagNodeID_e2, ts'') = _algbExp2DAGCustom e2 ts'


        _algbExp2DAGCustom (EQJOIN semInfos e1 e2) ts = memo (EQJOIN semInfos (ADAGNodeID dagNodeID_e1) (ADAGNodeID dagNodeID_e2)) ts''
            where
                (dagNodeID_e1, ts')  = _algbExp2DAGCustom e1 ts
                (dagNodeID_e2, ts'') = _algbExp2DAGCustom e2 ts'


        _algbExp2DAGCustom (SEMIJOIN semInfos e1 e2) ts = memo (SEMIJOIN semInfos (ADAGNodeID dagNodeID_e1) (ADAGNodeID dagNodeID_e2)) ts''
            where
                (dagNodeID_e1, ts')  = _algbExp2DAGCustom e1 ts
                (dagNodeID_e2, ts'') = _algbExp2DAGCustom e2 ts'



        _algbExp2DAGCustom (THETAJOIN semInfos e1 e2) ts = memo (THETAJOIN semInfos (ADAGNodeID dagNodeID_e1) (ADAGNodeID dagNodeID_e2)) ts''
            where
                (dagNodeID_e1, ts')  = _algbExp2DAGCustom e1 ts
                (dagNodeID_e2, ts'') = _algbExp2DAGCustom e2 ts'


        _algbExp2DAGCustom (DISJUNION e1 e2) ts = memo (DISJUNION (ADAGNodeID dagNodeID_e1) (ADAGNodeID dagNodeID_e2)) ts''
            where
                (dagNodeID_e1, ts')  = _algbExp2DAGCustom e1 ts
                (dagNodeID_e2, ts'') = _algbExp2DAGCustom e2 ts'

    
        _algbExp2DAGCustom (DIFFERENCE e1 e2) ts = memo (DIFFERENCE (ADAGNodeID dagNodeID_e1) (ADAGNodeID dagNodeID_e2)) ts''
            where
                (dagNodeID_e1, ts')  = _algbExp2DAGCustom e1 ts
                (dagNodeID_e2, ts'') = _algbExp2DAGCustom e2 ts'


        _algbExp2DAGCustom (DISTINCT e1 ) ts = memo (DISTINCT (ADAGNodeID dagNodeID_e1)) ts'
            where
                (dagNodeID_e1, ts')  = _algbExp2DAGCustom e1 ts
                


        _algbExp2DAGCustom (LIT_TBL semInfos schemaInfos) ts = memo (LIT_TBL semInfos schemaInfos) ts


        _algbExp2DAGCustom (EMPTY_TBL schemaInfos) ts = memo (EMPTY_TBL schemaInfos) ts
         
         
        _algbExp2DAGCustom (TABLEREF semInfos) ts = memo (TABLEREF semInfos) ts 
           

        _algbExp2DAGCustom (ATTACH semInfos e) ts = memo (ATTACH semInfos (ADAGNodeID dagNodeID_e)) ts'
            where
                (dagNodeID_e, ts') = _algbExp2DAGCustom e ts


        _algbExp2DAGCustom (CAST semInfos e) ts = memo (CAST semInfos (ADAGNodeID dagNodeID_e)) ts'
            where
                (dagNodeID_e, ts') = _algbExp2DAGCustom e ts


        _algbExp2DAGCustom (FUN_NUM_EQ semInfos e) ts = memo (FUN_NUM_EQ semInfos (ADAGNodeID dagNodeID_e)) ts'
            where
                (dagNodeID_e, ts') = _algbExp2DAGCustom e ts


        _algbExp2DAGCustom (FUN_NUM_GT semInfos e) ts = memo (FUN_NUM_GT semInfos (ADAGNodeID dagNodeID_e)) ts'
            where
                (dagNodeID_e, ts') = _algbExp2DAGCustom e ts


        _algbExp2DAGCustom (FUN_1TO1 semInfos e) ts = memo (FUN_1TO1 semInfos (ADAGNodeID dagNodeID_e)) ts'
            where
                (dagNodeID_e, ts') = _algbExp2DAGCustom e ts


        _algbExp2DAGCustom (FUN_BOOL_AND semInfos e) ts = memo (FUN_BOOL_AND semInfos (ADAGNodeID dagNodeID_e)) ts'
            where
                (dagNodeID_e, ts') = _algbExp2DAGCustom e ts
                
        _algbExp2DAGCustom (FUN_BOOL_OR semInfos e) ts = memo (FUN_BOOL_OR semInfos (ADAGNodeID dagNodeID_e)) ts'
            where
                (dagNodeID_e, ts') = _algbExp2DAGCustom e ts

        _algbExp2DAGCustom (FUN_BOOL_NOT semInfos e) ts = memo (FUN_BOOL_NOT semInfos (ADAGNodeID dagNodeID_e)) ts'
            where
                (dagNodeID_e, ts') = _algbExp2DAGCustom e ts



        _algbExp2DAGCustom (FUN_AGGR semInfos e) ts = memo (FUN_AGGR semInfos (ADAGNodeID dagNodeID_e)) ts'
            where
                (dagNodeID_e, ts') = _algbExp2DAGCustom e ts


        _algbExp2DAGCustom (FUN_AGGR_COUNT semInfos e) ts = memo (FUN_AGGR_COUNT semInfos (ADAGNodeID dagNodeID_e)) ts'
            where
                (dagNodeID_e, ts') = _algbExp2DAGCustom e ts


        _algbExp2DAGCustom (SERIALIZE_REL semInfos e1 e2) ts = memo (SERIALIZE_REL semInfos (ADAGNodeID dagNodeID_e1) (ADAGNodeID dagNodeID_e2)) ts''
            where
                (dagNodeID_e1, ts')  = _algbExp2DAGCustom e1 ts
                (dagNodeID_e2, ts'') = _algbExp2DAGCustom e2 ts'

        _algbExp2DAGCustom (NIL) ts = memo (NIL) ts
                
