-----------------------------------------------------------------------------------------
{-| Module      : Algb2DOTChildNodeIDs
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Algb2DOTChildNodeIDs(

-- =============================================================================
-- exports
-- =============================================================================
    getChildNodeIDs


) where


-- =============================================================================
-- imports
-- =============================================================================
    


import LangAlgb(AlgbExp (..))


----------------------------------------------------------------------
----------------------------------------------------------------------
getChildNodeIDs :: AlgbExp -> [Int]
----------------------------------------------------------------------



getChildNodeIDs ( ROWNUM        _semInfos                   (ADAGNodeID cid) )                      = [cid]
getChildNodeIDs ( ROWID        _semInfos                   (ADAGNodeID cid) )                      = [cid]
getChildNodeIDs ( RANK          _semInfos                   (ADAGNodeID cid) )                      = [cid]
getChildNodeIDs ( ROWRANK       _semInfos                   (ADAGNodeID cid) )                      = [cid]

getChildNodeIDs ( PROJ          _semInfos                   (ADAGNodeID cid) )                      = [cid]
getChildNodeIDs ( SEL           _semInfos                   (ADAGNodeID cid) )                      = [cid]
getChildNodeIDs ( POS_SELECT  _semInfos                     (ADAGNodeID cid) )                      = [cid]

getChildNodeIDs ( CROSS                                     (ADAGNodeID cid1) (ADAGNodeID cid2) )   = [cid1, cid2]
getChildNodeIDs ( EQJOIN        _semInofs                   (ADAGNodeID cid1) (ADAGNodeID cid2) )   = [cid1, cid2]
getChildNodeIDs ( SEMIJOIN     _semInofs                    (ADAGNodeID cid1) (ADAGNodeID cid2) )   = [cid1, cid2]
getChildNodeIDs ( THETAJOIN     _semInofs                   (ADAGNodeID cid1) (ADAGNodeID cid2) )   = [cid1, cid2]

getChildNodeIDs ( DISJUNION                                 (ADAGNodeID cid1) (ADAGNodeID cid2) )   = [cid1, cid2]
getChildNodeIDs ( DIFFERENCE                                (ADAGNodeID cid1) (ADAGNodeID cid2) )   = [cid1, cid2]
getChildNodeIDs ( DISTINCT                                  (ADAGNodeID cid1))                      = [cid1]

getChildNodeIDs ( LIT_TBL       _semInfos   _schemaInfos )                                          = []
getChildNodeIDs ( EMPTY_TBL                 _schemaInfos )                                          = []

getChildNodeIDs ( TABLEREF      _semInfos                )                                          = []

getChildNodeIDs ( ATTACH        _semInfos                   (ADAGNodeID cid) )                      = [cid]

getChildNodeIDs ( CAST          _semInfos                   (ADAGNodeID cid) )                      = [cid]


getChildNodeIDs ( FUN_NUM_EQ    _semInfos                   (ADAGNodeID cid) )                      = [cid]
getChildNodeIDs ( FUN_NUM_GT    _semInfos                   (ADAGNodeID cid) )                      = [cid]

getChildNodeIDs ( FUN_1TO1      _semInfos                   (ADAGNodeID cid) )                      = [cid]

getChildNodeIDs ( FUN_BOOL_AND      _semInfos                   (ADAGNodeID cid) )                      = [cid]
getChildNodeIDs ( FUN_BOOL_OR  _semInfos                   (ADAGNodeID cid) )                      = [cid]
getChildNodeIDs ( FUN_BOOL_NOT  _semInfos                   (ADAGNodeID cid) )                      = [cid]

getChildNodeIDs ( FUN_AGGR  _semInfos                   (ADAGNodeID cid) )                      = [cid]
getChildNodeIDs ( FUN_AGGR_COUNT  _semInfos                   (ADAGNodeID cid) )                      = [cid]



getChildNodeIDs ( SERIALIZE_REL  _semInfos                 (ADAGNodeID cid1) (ADAGNodeID cid2) )   = [cid2]
   
getChildNodeIDs ( NIL )   = []
   
