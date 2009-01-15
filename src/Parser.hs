

module Parser(myparse ) where


import LangCommon
import Lang
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( javaStyle )
          




--myparse :: FilePath -> String -> (Either ParseError  Expr)
myparse filePath input =   do{ 
         case parse program filePath input of
           Left err -> do{ 
                            return (Left err)
                           }
         
           Right expr  -> do{ 
                            return (Right expr)
                           }
      }


          
-----------------------------------------------------------
-- A program is simply an expression.
-----------------------------------------------------------
program
    = do{ whiteSpace
        ; e <- expr
        ; eof
        ; return e
        }




 
 
expr' :: Expr -> Parser Expr
expr' e =  do{    symbol "."
                
                 ;choice[ 
                            do{
                         
                              i <- natural
                            ; e' <- expr' (PosAcc e ((fromInteger i)::Int)) 
                            ; return e'
                        
                            },
                            do{ 
                             s <- identifier
                            ; e' <- expr' (NomAcc e s) 
                            ; return e'
                          } 
                    ]
                
               
              }
              
           

           <|>
           
           do{    
                return e
              } 

----------------------------------------------------------------
-- All types of expression(s)
----------------------------------------------------------------



expr :: Parser Expr
expr = choice 
       [ 
       
        try opAppExpr
       
        ,try parenthesedExpr 
         
       
            
        , literalExpr
       
        ,  tupleExpr
       
     
       
        , listExpr
       
       
      ,  letExpr
       
       --, varExpr
       
       ,  ifExpr
       
      ,  forExpr
     
      , tableRefExpr 
      
      --, funAppExpr
      , funAppOrVarExpr 
       
       ]
       <?> "expression"
       
----------------------------------------------------------------       
----------------------------------------------------------------       
-- LiteralExpr
----------------------------------------------------------------       
----------------------------------------------------------------       
literalExpr :: Parser Expr 
literalExpr = choice [intLiteral, strLiteral, boolLiteral]

intLiteral  = do{ i <- integer; e <- (expr' (Literal (IntLiteral i))); return e} 


strLiteral  = do{ s <- stringLiteral; e <- (expr' (Literal (StringLiteral s))); return e }

boolLiteral = choice
              [ 
                do{ reserved "true"; e <-(expr' (Literal (BoolLiteral True))); return e },
                do{ reserved "false"; e <-(expr' (Literal (BoolLiteral False))); return e }
              ]
       
----------------------------------------------------------------
----------------------------------------------------------------
-- OpAppExpr
---------------------------------------------------------------- 
---------------------------------------------------------------- 
opAppExpr :: Parser Expr                 
opAppExpr = buildExpressionParser operators simpleExpr
    where
        operators =
            [ [ unop "not"]
            , [ binop "*"  AssocLeft, binop "/"  AssocLeft ]
            , [ binop "+"  AssocLeft, binop "-"  AssocLeft ]
            , [ binop "%"  AssocLeft]
            , [ binop "==" AssocNone, binop "!=" AssocNone, binop "<="  AssocNone
              , binop "<" AssocNone, binop ">="  AssocNone, binop ">" AssocNone ]
            , [ binop "and" AssocRight ] -- Right for shortcircuiting
            , [ binop "or" AssocRight ] -- Right for shortcircuiting
            ]
            where
              binop name assoc   = Infix (do{ reservedOp name
                                          ; return (\e1 e2 -> OpApp (BinOp name e1 e2)) 
                                          }) assoc
              unop name     = Prefix  (do{ reservedOp name
                                          ; return (\e -> OpApp (UnOp name e))
                                          })                   
 
        
        simpleExpr = choice 
                     [
                        intLiteral,
                        strLiteral,
                        boolLiteral,
                        ifExpr, 
                        letExpr,
                        --tupleExpr, 
                        --varExpr,
                        parenthesedExpr,
                        --parens expr,
                        --funAppExpr
                        funAppOrVarExpr
                     ]

 

 
 
----------------------------------------------------------------
----------------------------------------------------------------
-- TupleExpr
----------------------------------------------------------------
----------------------------------------------------------------
tupleExpr :: Parser Expr
tupleExpr = do{ 
                --exps <- parens (commaSep1 expr)
                --;e <- (expr' (Tuple exps)); return e
              
                 ;symbol "("
                 ; hd <- expr;
                 ;symbol ","
                 ; tl <- commaSep1 expr
                 ; symbol ")"
                 ;e <- (expr' (Tuple (hd:tl))); return e
              }
 

        



----------------------------------------------------------------
----------------------------------------------------------------
-- ListExpr
----------------------------------------------------------------
----------------------------------------------------------------
listExpr :: Parser Expr
listExpr = do{ exps <- squares (commaSep expr)
                ;e <- (expr' (List exps)); return e
             }
 


----------------------------------------------------------------
----------------------------------------------------------------
-- LetExpr
----------------------------------------------------------------
----------------------------------------------------------------
letExpr :: Parser Expr
letExpr = do{ reserved "let"
            ; bindings <- letBindingClauses
            ; reserved "in"
            ; e <- expr
            ; e' <- (expr' (Let bindings e)); return e'
            }
       

----------------------------------------------------------------
-- LetBindingClauses
----------------------------------------------------------------
letBindingClauses
    = commaSep1 letBindingClause

letBindingClause
    = do{ --symbol "$"
          id <- identifier
        ; symbol "="
        ; e <- expr
        ; return (id, e)
        }


----------------------------------------------------------------
----------------------------------------------------------------
-- VarExpr
----------------------------------------------------------------
----------------------------------------------------------------
varExpr :: Parser Expr
varExpr = do{ --symbol "$"
          id <- identifier
        ; e' <- (expr' (Var id)); return e'
        }




----------------------------------------------------------------
----------------------------------------------------------------
-- IfExpr
----------------------------------------------------------------
----------------------------------------------------------------

ifExpr :: Parser Expr
ifExpr = do{ reserved "if"
             ; e1 <- expr
             ; reserved "then"
             ; e2 <- expr
             ; reserved "else"
             ; e3 <- expr
             ; e' <- (expr' (If e1 e2 (Just e3))); return e'
             }



----------------------------------------------------------------
----------------------------------------------------------------
-- ForExpr
----------------------------------------------------------------
----------------------------------------------------------------

forExpr :: Parser Expr
forExpr = do{ reserved "for"
             ; bindings <- forBindingClauses
             ; where1 <- option [] (do{ whereClauseExpr <- whereClause
                                                ; return ([whereClauseExpr]) 
                                                })
             ; groupBy <- option ([],[]) (do{            groupByClauseExpr <- groupByClause
                                                    ; where2 <- option [] (do{ whereClauseExpr <- whereClause
                                                                                ; return ([whereClauseExpr]) 
                                                                                })    
                                                
                                                    ; return ([groupByClauseExpr], where2) 
                                                })
             
             ; orderBy <- option [] (do{ orderByClauseExpr <- orderByClause
                                                ; return ([orderByClauseExpr]) 
                                                })                                   
             ; reserved "return"
             ; e2 <- expr
             ; e' <- (expr' (For ([bindings] ++ where1 ++ (fst groupBy) ++ (snd groupBy) ++ orderBy ++ [Return e2]))); return e'
             }


forBindingClauses
    = do { bindings <- commaSep1 forBindingClause
           ; return (Binding bindings) 
         }

forBindingClause
    = do{ --symbol "$"
          id <- identifier
        ; reserved "in"
        ; e <- expr
        ; return (id, e)
        }
        
whereClause
    = do{ reserved "where"
        ; e <- expr
        ; return (Where e)
        } 
        
groupByClause
    = do{ reserved "group by"
        ; es <- commaSep1 expr
        ; return (GroupBy es)
        }  
        
        
orderByClause
    = do{ reserved "order by"
        ; orderSpec <- commaSep1 orderSpec
        ; return (OrderBy orderSpec)
        }                        

orderSpec
    = do{ e <- expr
          ; orderModifier <- option Nothing (do{ orderModifierExpr <- orderModifier
                                                ; return (Just orderModifierExpr) 
                                                })
        ; return (e, orderModifier)
        }  


orderModifier = choice 
                [
                    do{reserved "ascending"
                        ; return Ascending
                      }  
                    , 
                    do{reserved "descending"
                        ; return Descending
                      }  
                ]



----------------------------------------------------------------
----------------------------------------------------------------
-- TableRefExpr
----------------------------------------------------------------
----------------------------------------------------------------

tableRefExpr :: Parser Expr
tableRefExpr = do{ reserved "table"
                 --; id <- identifier
                 ; id <- tableidentifier
                 ; tblAttSpecs <- tableAttSpecs
                 ; reserved "with keys"
                 ; tblKeySpecs <- tableKeySpecs
                 ; order <- option [] (do{ reserved "with order"
                                          ;tblOrderSpecs <- tableOrderSpecs
                                          ; return tblOrderSpecs 
                                        })
            
          
             ; e' <- (expr' (Table id tblAttSpecs tblKeySpecs order)); return e'
             }

tableidentifier
    = choice [
                 try qualifiedTableName,
                 try unqualifiedTableName
             ]
    
qualifiedTableName
    = do {
           schema <- identifier
           ; symbol "."
           ; tname <- identifier
           ; return (schema ++ "." ++ tname)
         }
  
unqualifiedTableName
    =  do {
            tid <- identifier
            ; return tid
          }  

tableAttSpecs
    = do {
           tblAttSpecs <- parens (commaSep1 tableAttSpec)
           ; return tblAttSpecs
         }

tableAttSpec
    = do {
            attName <- identifier
           ; attType <- tableAttType
           ; return (attName, attType)
         }


tableAttType
    = choice [
                 do {reserved "int"; return ATRT_INT}
                ,do {reserved "string"; return ATRT_STR}
                ,do {reserved "bool"; return ATRT_BOOL}
            ]



tableKeySpecs
    = do {
           tblKeySpecs <- parens (commaSep1 tableKeySpec)
           ; return tblKeySpecs
         }

tableKeySpec
    = do {
            tblAttSpecs <- parens (commaSep1 identifier)
            ; return tblAttSpecs  
         }


tableOrderSpecs
    = do {
           tblOrderSpecs <- parens (commaSep1 tableOrderSpec)
           ; return tblOrderSpecs
         }

                       

tableOrderSpec
    = do{ tblAttName <- identifier
          ; orderModifier <- option Nothing (do{ orderModifierExpr <- orderModifier
                                                ; return (Just orderModifierExpr) 
                                                })
        ; return (tblAttName, orderModifier)
        }  




----------------------------------------------------------------
----------------------------------------------------------------
-- FunAppOrVarExpr
----------------------------------------------------------------
----------------------------------------------------------------
funAppOrVarExpr :: Parser Expr
funAppOrVarExpr = do{ id <- identifier
                   ;  e<- test1 id 
                ; e' <- (expr' e); return e'
        }


test1 id
    = choice [
                 try  (test2 id)
                 ,
                 test3 id
                 
            ]
            
test2 id = do { 
                    ;args <- parens funArgs 
                    ;return (FunApp id args)
               }  
  
test3 id = do { 
                    
                    ;return (Var id)
               }         

----------------------------------------------------------------
----------------------------------------------------------------
-- FunAppExpr
----------------------------------------------------------------
----------------------------------------------------------------
funAppExpr :: Parser Expr
funAppExpr = do{ id <- identifier
                ;args <- parens funArgs 
                ; e' <- (expr' (FunApp id args)); return e'
        }



funArgs
    = choice [
                 try inlineLambda,
                 try normalExprs
            ]
normalExprs
    = do  { exprs <- commaSep expr
            ;return (Exprs exprs);
          }
 
inlineLambda
    = do {  --symbol "$"
             id <- identifier
            ;symbol "->"
            ;e1 <- expr
            ;symbol ","
            ;e2 <- expr
            ; return (InlineLambda ((id, e1), e2))
          }

----------------------------------------------------------------
----------------------------------------------------------------
-- (Expr)
----------------------------------------------------------------
----------------------------------------------------------------
parenthesedExpr :: Parser Expr
parenthesedExpr  = do{ e <- parens expr
                ;e' <- (expr' (Parenthesed e)); return e'
              } 

-----------------------------------------------------------
-- The lexer
-----------------------------------------------------------
lexer     = P.makeTokenParser ferryDef

ferryDef  = P.LanguageDef
          { 
              P.commentStart   = "/*"
            , P.commentEnd     = "*/"
            , P.commentLine    = "//"
            , P.nestedComments = True
            , P.identStart     = alphaNum <|> oneOf "_"
            , P.identLetter    = alphaNum <|> oneOf "_"
            , P.opStart        = P.opLetter ferryDef
            , P.opLetter       = oneOf (concat (P.reservedOpNames ferryDef))
            , P.reservedOpNames= [ "<", "<=", ">", ">=", "==", "+", "*", "-", "/", "and", "or", "not", "."]
            , P.reservedNames  = [ "else", "for", "where", "order by", "group by", "ascending", "descending",  
                                 "if", "in", "let","return", 
                                 "table", "with keys", "with order", 
                                 "int", "string", "bool", 
                                 "then", "$"]
            , P.caseSensitive  = True   
           }

parens          = P.parens lexer    
braces          = P.braces lexer    
semiSep         = P.semiSep lexer  
semiSep1        = P.semiSep1 lexer    
commaSep        = P.commaSep lexer
commaSep1       = P.commaSep1 lexer
brackets        = P.brackets lexer
squares        =  P.squares lexer
whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
identifier      = P.identifier lexer    
reserved        = P.reserved lexer    
reservedOp      = P.reservedOp lexer
integer         = P.integer lexer    
natural         = P.natural lexer    
charLiteral     = P.charLiteral lexer    
stringLiteral   = P.stringLiteral lexer    
