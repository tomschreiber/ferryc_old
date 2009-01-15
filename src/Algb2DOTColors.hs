-----------------------------------------------------------------------------------------
{-| Module      : Algb2DOTColors
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Algb2DOTColors(

-- =============================================================================
-- exports
-- =============================================================================
    Color,
    
    getColorByAlgbExp,
    getColorByName
        

) where

-- =============================================================================
-- imports
-- =============================================================================
    


import LangAlgb(AlgbExp (..))


-- =============================================================================
-- types
-- =============================================================================
    


type ColorName = String
type R = Integer
type G = Integer
type B = Integer
type RGBColor = (R, G, B)
type HEXColor = String

type Color = (ColorName, (RGBColor, HEXColor))


-- =============================================================================
-- functions
-- =============================================================================
    
----------------------------------------------------------------------
----------------------------------------------------------------------
getColorByAlgbExp :: AlgbExp -> Color
----------------------------------------------------------------------
----------------------------------------------------------------------
getColorByAlgbExp (ROWNUM        _semInfos _e)            = getColorByName  "cred03" 
getColorByAlgbExp (ROWID         _semInfos _e)            = getColorByName  "cpink01" 
getColorByAlgbExp (ROWRANK       _semInfos _e)            = getColorByName  "cred03" 
getColorByAlgbExp (RANK          _semInfos _e)            = getColorByName  "cred02" 

getColorByAlgbExp (PROJ          _semInfos _e)            = getColorByName  "cgray01" 
getColorByAlgbExp (SEL           _semInfos _e)            = getColorByName  "cblue01"       
getColorByAlgbExp (POS_SELECT  _semInfos _e)              = getColorByName  "cred04" 

getColorByAlgbExp (CROSS         _e1 _e2)                 = getColorByName  "cred05" 
getColorByAlgbExp (EQJOIN        _semInfos _e1 _e2)       = getColorByName  "cgreen01" 
getColorByAlgbExp (SEMIJOIN      _semInfos _e1 _e2)       = getColorByName  "cgreen06" 
getColorByAlgbExp (THETAJOIN     _semInfos _e1 _e2)       = getColorByName  "cgreen05" 

getColorByAlgbExp (DISJUNION     _e1 _e2)                 = getColorByName  "cgray05" 
getColorByAlgbExp (DIFFERENCE     _e1 _e2)                 = getColorByName "corange01" 

getColorByAlgbExp (DISTINCT      _e1)                     = getColorByName  "corange01" 

getColorByAlgbExp (LIT_TBL       _semInfos _schemaInfos)  = getColorByName  "cgray03" 
getColorByAlgbExp (EMPTY_TBL     _schemaInfos)            = getColorByName  "cgray03" 

getColorByAlgbExp (TABLEREF     _semInfos)                = getColorByName  "cgray03" 

getColorByAlgbExp (ATTACH        _semInfos _e)            = getColorByName  "cgray01" 

getColorByAlgbExp (CAST          _semInfos _e)            = getColorByName  "cgray03" 


getColorByAlgbExp (FUN_NUM_EQ    _semInfos _e)            = getColorByName  "cblue01" 
getColorByAlgbExp (FUN_NUM_GT    _semInfos _e)            = getColorByName  "cblue01" 

getColorByAlgbExp (FUN_1TO1    _semInfos _e)              = getColorByName  "cgray03" 

getColorByAlgbExp (FUN_BOOL_AND  _semInfos _e)            = getColorByName  "cgray03" 
getColorByAlgbExp (FUN_BOOL_OR  _semInfos _e)             = getColorByName  "cgray03" 
getColorByAlgbExp (FUN_BOOL_NOT  _semInfos _e)            = getColorByName  "cgray03" 

getColorByAlgbExp (FUN_AGGR  _semInfos _e)                = getColorByName  "cgray04" 
getColorByAlgbExp (FUN_AGGR_COUNT  _semInfos _e)          = getColorByName  "cgray04" 



getColorByAlgbExp (SERIALIZE_REL _semInfos _e1 _e2)       = getColorByName  "cgray03" 

getColorByAlgbExp (NIL)       = getColorByName  "cgray03" 




----------------------------------------------------------------------
----------------------------------------------------------------------
getColorByName :: ColorName -> Color
----------------------------------------------------------------------
----------------------------------------------------------------------
getColorByName name = case lookup name colors of
                    Just color  -> (name ,color)
                    Nothing -> error ("color with name " ++ name ++ " unknown")



----------------------------------------------------------------------
----------------------------------------------------------------------
colors :: [Color]
----------------------------------------------------------------------
----------------------------------------------------------------------
colors =    [

                -----------------------------------------------------------------------------------------
                -- http://www.pangloss.com/seidel/ClrHlpr/mixer.cgi
                -----------------------------------------------------------------------------------------
                
                -----------------------------------------------------------------------------------------
                -----------------------------------------------------------------------------------------
                -- whites
                
                ("cwhite01",    ((255, 255, 255), "#FFFFFF")),   
                    -- #FFFFFF  -- white    -- nil, dummy

        
                -----------------------------------------------------------------------------------------
                -----------------------------------------------------------------------------------------
                -- grays
                
                ("cgray01",     ((238, 238, 238), "#EEEEEE")),  -- attach, project
                
                ("cgray02",     ((224, 224, 224), "#E0E0E0")),  -- roots, fragment, frag_union, empty_frag
                ("cgray03",     ((192, 192, 192), "#C0C0C0")),  -- serialize, lit_tbl, empty_tbl, fun_1to1, 
                                                                -- bool_and, bool_or, bool_not, type, 
                                                                -- type_assert, cast, seqty1, all, doc_tbl, 
                                                                -- cond_err, string_join
                
                ("cgray04",     ((160, 160, 160), "#A0A0A0")),  -- avg, max, min, sum, count
                
                ("cgray05",     ((141, 141, 141), "#909090")),  -- disjunion
                
                
                -----------------------------------------------------------------------------------------
                -----------------------------------------------------------------------------------------
                -- greens
                
                ("cgreen01",    ((  0, 255,   0), "#00FF00")),  -- eqjoin
                
                ("cgreen02",    ((  0, 208,   0), "#00D000")),  -- merge_adjacent
                ("cgreen03",    ((  0, 204,   5), "#00Cc059")), -- element, element_tag, attribute, 
                                                                -- textnode, docnode, comment, processi           
                ("cgreen04",    ((  0, 204,   0), "#00CC00")),  -- eqjoin_unq, 
                
                ("cgreen05",    ((  0, 170,   0), "#00AA00")),  -- thetajoin 
                ("cgreen06",    ((  0, 153,   0), "#009900")),  -- semijoin
                


                -----------------------------------------------------------------------------------------
                -----------------------------------------------------------------------------------------
                -- blues
                
                ("cblue01",     ((  0, 221, 221), "#00DDDD")),  -- select, num_eq, num_gt
                ("cblue02",     ((223, 255, 255), "#DFFFFF")),  -- proxy, proxy_base
                ("cblue03",     ((204, 204, 255), "#CCCCFF")),  -- doc_access
                ("cblue04",     (( 30, 144, 255), "#1E90FF")),  -- scjoin
                ("cblue05",     (( 30, 144, 153), "#1E9099")),  -- dup_scjoin 
                
                
                -----------------------------------------------------------------------------------------
                -----------------------------------------------------------------------------------------
                -- pinks
                
                ("cpink01",     ((255, 153, 153), "#FF9999")),  -- ROWID
                
                ("cpink02",     ((255,   0, 255), "#FF00FF")),  -- rec_fix, rec_param
                ("cpink03",     ((187,   0, 187), "#BB00BB")),  -- rec_arg, rec_base
                
                
                
                -----------------------------------------------------------------------------------------
                -----------------------------------------------------------------------------------------
                -- oranges
                
                ("corange01",   ((255, 165,   0), "#FFA500")),  -- intersect, difference, distinct
                
                
                -----------------------------------------------------------------------------------------
                -----------------------------------------------------------------------------------------
                -- reds
                
                ("cred01",      ((255,  85,   0), "#FF5500")),  -- trace, trace_msg, trace_map
                ("cred02",      ((255,   0,   0), "#FF3333")),  -- rank
                ("cred03",      ((255,   0,   0), "#FF0000")),  -- rownum
                ("cred04",      ((204,   34, 34), "#CC2222")),  -- pos_select
                ("cred05",      ((153,   0,   0), "#990000"))   -- cross
                                                    

            ]

