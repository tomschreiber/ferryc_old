-----------------------------------------------------------------------------------------
{-| Module      : PrettyPrint
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module PrettyPrint_Lang where

import Text.PrettyPrint.HughesPJ

import LangCommon
import Lang


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


    PosAcc e i -> (pp e) <> text "." <> (integer (toInteger i))
    
    List es -> brackets (hcat es'')
                where
                    es' = map pp es
                    es'' = punctuate comma es' 
       
    
    
    
    Let bindings e -> (text "let") $+$ (nest 4 (bindings''')) $+$ (text "in") $+$ (nest 4 (pp e))
                where
                    bindings' = map f bindings
                    f (varName, expr) = (text varName) <+> (text "=") <+> (pp expr)
                    bindings''  =  punctuate (comma) bindings'
                    bindings''' =  vcat bindings''
                    
    
    
    Var varName -> (text varName)
    
    If e1 e2 (Just e3) -> (text "if") $+$ (nest 4 (pp e1)) $+$ (text "then") $+$ (nest 4 (pp e2)) $+$ (text "else") $+$ (nest 4 (pp e3)) 
    
    
    For forClauses -> (text "for") <+> bindings''' $+$ forClauses''
        where
            Binding bindings = head forClauses
            bindings' = map f bindings
            f (varName, expr) = (text varName) <+> (text "in") <+> (pp expr)
            bindings''  =  punctuate (comma) bindings'
            bindings''' =  hcat bindings''
            
            forClauses' = map prettyForClause (tail forClauses)
            forClauses'' = vcat forClauses'
               
    
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
                                    [] -> empty
                                    _  ->  (text "with order") <+> (parens (hcat (punctuate comma (map f4 tblOrderSpecs))))
                                        
                                           where 
                                            f4 (atrName, dir) = (text atrName)  <+> direction dir
                                            direction dir = case dir of
                                                Just Ascending -> text "ascending" 
                                                Just Descending -> text "descending" 
                                                Nothing -> empty
      
      
      
      
    NomAcc e s -> (pp e) <> text "." <> (text s)
    
   
    
    FunApp funID funArgs ->  (text funID) <> (parens args)
        where
            args = case funArgs of
                Exprs exprs -> hcat (punctuate comma (map pp exprs))                           
                InlineLambda ((varID, e1), e2) -> (text varID) <> (text " -> ") <> (pp e1) <> (text ", ") <> (pp e2)  
  
  
    Parenthesed expr -> parens (pp expr)
  
 {- 
  data ForClause 
    =   Binding [(VarName, Expr)]
    |   Where Expr
    |   GroupBy [Expr]
    |   OrderBy [(Expr, Maybe SortDirection)]
    |   Return Expr                   
    
  -}
  


prettyForClause  :: ForClause  -> Doc
prettyForClause e = case e of
                    
        Where expr ->  nest 4 ((text "where") <+> (pp expr))           
        
        GroupBy exprs ->  nest 4 ((text "group by") <+> exprs''') 
            where
                exprs' = map pp exprs          
                exprs'' = punctuate (comma) exprs'           
                exprs''' = hcat exprs'' 
                
        OrderBy exprs ->  nest 4 ((text "order by") <+> exprs''') 
            where
                exprs' = map f exprs
                f (expr, dir) = (pp expr)  <+> direction dir
                direction dir = case dir of
                    Just Ascending -> text "ascending" 
                    Just Descending -> text "descending" 
                    Nothing -> empty           
                exprs'' = punctuate (comma) exprs'           
                exprs''' = hcat exprs'' 
                
        Return expr  -> nest 4 (text "return" $+$ (nest 4 (pp expr)))         