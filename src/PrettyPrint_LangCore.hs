-----------------------------------------------------------------------------------------
{-| Module      : PrettyPrint
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module PrettyPrint_LangCore where

import Text.PrettyPrint.HughesPJ

import LangCommon
import LangCore


prettyprint :: Expr -> String
prettyprint e = render (pp e)




pp :: Expr -> Doc

pp e = case e of

    Literal (IntLiteral i) -> text (show i)
    Literal (StringLiteral s) -> text ("\"" ++ s ++ "\"")
    Literal (BoolLiteral b) -> text (case b of {True -> "true"; False -> "false"})
    
    OpApp (UnOp name e) -> (text name) <+> (pp e)
    
    OpApp (BinOp name e1 e2) -> (pp e1) <+> (text name) <+> (pp e2)
    
    Tuple es -> parens (hcat es'')
                where
                    es' = map pp es
                    es'' = punctuate comma es' 


    PosAcc e i -> (pp e) <> text "." <> (text (show i))
    
    List es -> brackets (hcat es'')
                where
                    es' = map pp es
                    es'' = punctuate comma es' 
       
    
    
    
    Let varID e1 e2 -> (text "let") $+$ (nest 4 ((text varID) <+> (text "=") <+> (pp e1))) $+$ (text "in") $+$ (nest 4 (pp e2))
     
    
    Var varID -> (text varID)
    
    If e1 e2 maybeE3 -> (text "if") $+$ (nest 4 (pp e1)) $+$ (text "then") $+$ (nest 4 (pp e2)) $+$ (text "else") $+$ (nest 4 (e3')) 
        where
            e3' = case maybeE3 of
                Just e -> pp e
                Nothing -> text "()"
    
    
    For varID e1 e2 maybeForOrderByClause -> (text "for") <+> ((text varID) <+> (text "in") <+> (pp e1)) $+$ forClauses
        where
            
            orderByClause = case maybeForOrderByClause of
                Just orderSpecs -> orderSpecs' orderSpecs
                Nothing -> empty
                
            orderSpecs' orderSpecs =  nest 4 ((text "order by") <+> exprs''') 
                where
                    exprs' = map f orderSpecs
                    f (expr, dir) = (pp expr)  <+> direction dir
                    direction dir = case dir of
                        Ascending -> text "ascending" 
                        Descending -> text "descending"                                 
                    exprs'' = punctuate (comma) exprs'           
                    exprs''' = hcat exprs'' 
            
            returnClause = nest 4 (text "return" $+$ (nest 4 (pp e2)))     
            
            
            forClauses = orderByClause $+$ returnClause  
               
    
    
    Table tblID tblAttSpecs tblKeySpecs tblOrderSpecs -> (text "table") <+> (text tblID) <+> tblAttSpecs'' $+$ tblKeySpecs' $+$ tblOrderSpecs' 
                where
                    tblAttSpecs' = map f tblAttSpecs
                    f (atrName, atrType) = (text atrName) <+> (atrTypeDoc atrType)
                    atrTypeDoc atrType = case atrType of
                                    ATRT_INT -> text "int" 
                                    ATRT_STR -> text "string" 
                                    ATRT_BOOL -> text "bool" 
                    tblAttSpecs'' = parens (hcat (punctuate comma tblAttSpecs'))
    
                    tblKeySpecs' = (text "with keys") <+> (parens (hcat (punctuate comma (map f2 tblKeySpecs))))
                    f2 atts = parens (hcat (punctuate comma (atts' atts)))
                    atts' atts = map f3 atts
                    f3 attName = text attName
                    
                     
                    tblOrderSpecs' = case tblOrderSpecs of
                                    [] -> error("oops")  
                                    _  ->  (text "with order") <+> (parens (hcat (punctuate comma (map f4 tblOrderSpecs))))
                                        
                                           where 
                                            f4 (atrName, dir) = (text atrName)  <+> direction dir
                                            direction dir = case dir of
                                                Ascending -> text "ascending" 
                                                Descending -> text "descending" 
                                               
      
      
      
    
    
    
    FunApp funID funArgs ->  (text funID) <> (parens args)
        where
            args = case funArgs of
                Exprs exprs -> hcat (punctuate comma (map pp exprs))                           
                InlineLambda ((varID, e1), e2) -> (text varID) <> (text " -> ") <> (pp e1) <> (text ", ") <> (pp e2)  
  
  
    Parenthesed expr -> parens (pp expr)
  
 