{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

-----------------------------------------------------------------------------------------
{-| Module      : Main
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

-- This should be stripped down.  Significantly.

module Main where

import Lang
import LangCore

import Text.ParserCombinators.Parsec.Error
import Parser

import PrettyPrint_Lang
import PrettyPrint_LangCore


import Normalize

import TypeChecker_RT_GT
import Annotater

import Core2AlgbUtils

import TI2DAG(tiPseudoDAGMemorizer, ti2DAGCustom)
import Algb2DAG(algbExpDAGMemorizer, algbExpPseudoDAGMemorizer)

import LangAlgb(AlgbExp (..), AValue (..), SortDirection (..), AType (..))

import Props.ONE
import Props.DENSE
import Props.UNIQUE
import Props.COLS
import Props.SCHEMA
import AlgbRewrites.AlgbRewrite1
import AlgbRewrites.TIRewrite1
import AlgbRewrites.TIRewrite2



import CoreRewrite_Append (rewrite)
import CoreRewrite_Append2 (rewrite)
import CoreRewrite_AppendOptimized (rewrite)
import CoreRewrite_EmptySubListsAwareAppend (rewrite)
import CoreRewrite_EmptySubListsAwareAggregates (rewrite)

import Text.PrettyPrint.HughesPJ


import Core2Algb(TableInfos (..), AttrName, compile, wrapWithSerializeOps, removeSerializeOps)
import TI2Dot(ti2dot)
--import TI2Dot_LaTex(ti2dot)

import TI2XML(ti2XML)
import Algb2XML(algbExp2XML  )


import XML2TI (importQueryPlan)
import XML2Alg (importAlgebraPlan2)

import Database.HDBC(SqlValue)
import DB.DB

import System.Environment
import System.Exit
import System.Cmd
import System.Environment
import System.Directory
import System.Info

import System.IO
import System.Process

import Data.List


import Text.XML.HXT.Arrow
import Text.XML.HXT.Arrow.XmlArrow

import AttributeNameTransformation(getPFUniqueName)





main = do {
  args <- getArgs
  
   
  
  
  ; args' <- normalizeArgs args
  ; Main.init args'
  --if (length args) == 0 
  --  then
  --      error("use one of, or a proper combination of -I, -D or -X") 
  --  else do 
   
  ;run args'
   
 }  
        


init args = do {
        --appTWrkDir <- appTmpWrkDir
        --;createDirectoryIfMissing True appTWrkDir
        if (elem "-l" args)
            then
                do
                    logDir <- getLogDir args
                    ; createDirectoryIfMissing True logDir
                                
            else
                do return ()
        
        
                        
        }

normalizeArgs args =
            -- if -E, then we ensure that the stopbit is set, and has at most the value 6 
            if (elem "-E" args)
                        then
                            
                             let 
                                indexMaybe = elemIndex "-s" args
                             in
                                case indexMaybe of 
                                    Just i -> let
                                                stopBit = (read (args !! (i+1)))::Integer
                                              in
                                                if stopBit > 6 
                                                    then
                                                        let 
                                                            argsPrefix = take i args
                                                            argsSuffix = drop (i + 2) args
                                                            args' = (argsPrefix ++ argsSuffix ++ ["-s"] ++ ["6"])
                                                          
                                                        in
                                                            do return args'         
                                                    else
                                                        do return args 
                                    
                                    
                                    
                                    Nothing -> do return (args ++ ["-s"] ++ ["6"])
                                   
        
        
        
    else
        do return args;
    


algbExp2SQL args algbExp = let
                             xmlTree = algbExp2XML algbExp algbExpDAGMemorizer getPFUniqueName
                          in  
                             do {
                                
                                [xmlString] <- runX (
                                                root [] [xmlTree] 
                                                >>>
                                                writeDocumentToString [(a_indent,v_1)]
                                               )
                                               
                                ; pfsqlPath <- getPFSQLPath args
                                
								;let 
                                    
                                    pfsqlCommandLine = pfsqlPath
       
                                 in
                                    do {
                                        
                                        
                                       
                                        (stdin,stdout,stderr,pHandle) <- runInteractiveCommand pfsqlCommandLine  
                                                
                                        ;hPutStr stdin xmlString
                                        ;hClose stdin
                                        
                                        
                                        
                                        ;sqlString <- hGetContents stdout
                                        --;hClose stdout
                                        
                                        ;checkOOPS stderr pfsqlCommandLine xmlString  
                                            
                                        ;return sqlString
                                               
                                        
                                       } 
                                       
                                }    








                        
                        
annotatedTableInfosDAG2SQLQueriesString args [] = do
             return [] 

annotatedTableInfosDAG2SQLQueriesString args ((id, (_ti, sqlCodeString, _)):tl) =
                                            let 
                                                prefix = 
                                                     "\n" 
                                                  ++ "--------------------------------------------------------------------------------\n" 
                                                  ++ "-- q" ++ (show id) ++ "\n"
                                                  ++ "--------------------------------------------------------------------------------\n"
                                                  ++ "\n" 
                                             in
                                                do
                                                    
                                                    sqlCodeStrings <- annotatedTableInfosDAG2SQLQueriesString args tl
                                                    
                                                    return (prefix ++ sqlCodeString ++ sqlCodeStrings)    
  
 
 
annotateTableInfosDAGWithSQLCode args [] = do
                            return []  

annotateTableInfosDAGWithSQLCode args ((id, TI(ae, cols, subs)):tl) =
                                           
                                                do
                                                    sqlCodeString <- algbExp2SQL args ae
                                                    annotatedTail <- annotateTableInfosDAGWithSQLCode args tl
                                                    
                                                    return ([(id, (TI(ae, cols, subs), sqlCodeString, []))] ++ annotatedTail)     
                                    
 
   
 
 
 
annotatedTableInfosDAG2SQLQueriesResult args [] = do
                                return [] 
                                 
 
 
  
 
 
annotatedTableInfosDAG2SQLQueriesResult args ((id, (TI((SERIALIZE_REL  (iterAttrName, posAttrName, itemAttrNames) (NIL) e), cols, _), _, rowMaps)):tl) =
                                            let 
                                                prefix = 
                                                     "\n" 
                                                  ++ "--------------------------------------------------------------------------------\n" 
                                                  ++ "-- q" ++ (show id) ++ "\n"
                                                  ++ "--------------------------------------------------------------------------------\n"
                                                  ++ "\n" 
                                             
                                                schema = Props.SCHEMA.schema e
                                             
                                                sqlResultString = ppSQLQueryResult (iterAttrName, posAttrName, itemAttrNames) cols schema rowMaps
                                                  
                                            in    
                                                do
                                                    sqlResultStrings <- annotatedTableInfosDAG2SQLQueriesResult args tl    
                                                    return (prefix ++ sqlResultString ++ "\n" ++ sqlResultStrings)    
   
 
 
annotateTableInfosDAGWithSQLResult args tableInfosDAG = do
                                                           odbcConnectionString <- (getODBCConnectionString args)
                                                           con <- getConnection odbcConnectionString  
                                                           tableInfosDAG' <- _annotateTableInfosDAGWithSQLResult args con tableInfosDAG
                                                           closeConnection con
                                                           return tableInfosDAG'    
 
 
 
_annotateTableInfosDAGWithSQLResult args con [] = do
                            return []    
 
_annotateTableInfosDAGWithSQLResult args con ((id, (TI(ae, cols, subs), sqlCode, _)):tl) =
                                           
                                                do 
                                                    rowMaps <- DB.DB.execute con sqlCode 
                                                    annotatedTail <- _annotateTableInfosDAGWithSQLResult args con tl
                                                    
                                                    return ([(id, (TI(ae, cols, subs), sqlCode, rowMaps))] ++ annotatedTail)     
         
                                                    
 


data Result =    RValue String
               | RTuple [Result] 
               | RList  [Result]
                deriving (Show, Eq)
                  
hasSubLists r = case r of
                    RValue v -> False
                    RTuple vs  -> or (map (\v -> case v of {RList vs -> hasSubLists (RList vs); _ -> False}) vs)
                    RList vs   -> or (map (\v -> case v of {RList _ -> True; _ -> False}) vs)
   
renderResult = render . ppResult
 
ppResult :: Result -> Doc
ppResult (RValue v)              = text v
ppResult (RTuple vs)             = parens   $ hcat $ contents $ vs
ppResult (RList vs@(RValue _:_)) = brackets $ hcat $ contents $ vs
ppResult (RList vs)              = brackets $ vcat $ contents $ vs

contents :: [Result] -> [Doc]
contents = punctuate comma . map ppResult 
  
  
getTypedShowVal t s = case t of
        AT_INT -> s
        AT_STR -> show s
        AT_BOOL ->
            case s of 
                "0" -> "false"
                "1" -> "true"
                _   -> error("expected 0 or 1")
        AT_DEC -> s 
        AT_DBL -> s 
        AT_NAT -> s 
        
 
getResult  (TI((SERIALIZE_REL  (iterAttrName, posAttrName, itemAttrNames) (NIL) e), cols, subs)) rowMaps  dagList key t =
                                            let 
                                                schema = Props.SCHEMA.schema e 
                                                
                                                
                                                rowMaps' = if key == "-1"
                                                    then
                                                        rowMaps
                                                    else
                                                        filter f1 rowMaps
                                                 
                                                f1 rowMap =
                                                    let
                                                        Just tIter = lookup iterAttrName schema 
                                                        rawIterVal =  getStringValFromColVal (getColValFromRowMapByColNamePrefix (iterAttrName ++ "_" ++ (show tIter)) rowMap)        
                                                    in
                                                        key == rawIterVal
                                                        
                                                concreteRowValues  = map f rowMaps'
                                                f rowMap =
                                                    let
                                                        
                                                        rawColValuesWithTypes = map (f2 rowMap) itemAttrNames
                                                        f2 rowMap itemAttrName = 
                                                            let
                                                                Just itemAttrType = lookup itemAttrName schema
                                                                itemAttrRawStringValue = getStringValFromColVal (getColValFromRowMapByColNamePrefix (itemAttrName ++ "_" ++ (show itemAttrType)) rowMap) 
                                                            in    
                                                                (itemAttrRawStringValue,itemAttrType)  
                                                        
                                                        
                                                        rawColValues' = zip cols rawColValuesWithTypes
                                                        concreteColValues = map (getValue t subs dagList) rawColValues'
                                                                                                               
                                                        s' = if ((length cols > 1) && (length rawColValuesWithTypes > 1))
                                                            then                                                                
                                                                RTuple concreteColValues 
                                                            else                                                                
                                                                head concreteColValues 
                                                        
                                                    in
                                                        s'    
                                                
                                               
                                                s' = if key == "-1"
                                                    then
                                                        case  t of
                                                            RT_Table -> RList concreteRowValues
                                                            RT_Tuple -> head concreteRowValues
                                                    else
                                                        RList concreteRowValues
                                                
                                               
                                            in    
                                               s'    
    
 

getValue  t subs dagList (c, (val_c,t_c)) =
                    
                        let 
                            subTI = lookup c subs
                        in
                            case subTI of
                                Nothing -> RValue (getTypedShowVal t_c val_c)
                                
                                Just (TIDAGNodeID id) ->
                                    let
                                        Just (tableInfos, _, rowMaps) = lookup id dagList
                                    in
                                        getResult tableInfos rowMaps dagList val_c t     
 
 

ppSQLQueryResult :: (String, String, [String])  -- table column names of iter, pos, and item columns 
                    -> [String]                 -- item column names for display
                    -> [(String, AType)]        -- table schema [(column name, type)]
                    -> [[(String, SqlValue)]]   -- rows [[(column name, value)]]
                    -> String                   -- pretty-printed SQL result

ppSQLQueryResult (iter, pos, items) cols schema rows =
  unlines $ map (concat . intersperse " | ") $ table
  where
  headings :: [String]
  headings = [iter, pos] ++ items
  
  tuples :: [[String]]
  tuples = map tuple rows
    where
    -- collect values for one tuple
    tuple :: [(String, SqlValue)] -> [String]
    tuple row = map (\c -> sql_val row (sql_ty c) c) headings

    -- extract typed column value from row
    sql_val :: [(String, SqlValue)] -> AType -> String -> String
    sql_val row ty col = getTypedShowVal ty 
                       $ getStringValFromColVal 
                       $ getColValFromRowMapByColNamePrefix (col ++ "_" ++ show ty) row

    -- extract column type from schema
    sql_ty :: String -> AType
    sql_ty col = case (lookup col schema) of
       Just ty -> ty

  -- headings and values, column-wise
  columns :: [[String]]
  columns = transpose (headings:tuples)
  
  -- columns plus their widths 
  wcolumns :: [(Int,[String])]
  wcolumns = zip (map (maximum . map length) columns) columns
  
  
  table :: [[String]]
  table = transpose 
        -- insert `---' just below header and
        -- pad all values to full column width
        $ map (\(w, head:tups) -> map (pad w) ([head, replicate w '-'] ++ tups)) 
        $ wcolumns
    where
    -- right-pad value with spaces 
    pad :: Int -> String -> String
    pad w val = take w (val ++ repeat ' ')


                        
checkOOPS stderr pfcmdline pfinput = do {
                            readable <- hReady stderr 
                                         
                            ; if (readable)
                                then do {    
                                    pfErrorMSG <- hGetContents stderr
                                    ; if(pfErrorMSG == "")
                                        then
                                            do
                                                return ()
                                             else
                                                let
                                                    completeErrorMS = (
                                                        "-- PF-Error ---------------------------\n" ++
                                                        pfErrorMSG ++ 
                                                        "-- PF-CommandLine ---------------------\n" ++ 
                                                        pfcmdline ++ "\n" ++ 
                                                        "-- Input Start -------------------------\n" ++
                                                        pfinput ++ 
                                                        "-- Input End ---------------------------\n") 
                                                 in
                                                    do error(completeErrorMS)
                                   }
                                else do return ()


                      }

       
                        

optimizeAlgbExp args algbExp  = let
                             xmlTree = algbExp2XML algbExp algbExpDAGMemorizer getPFUniqueName
                          in  
                             do {
                                
                                [xmlString] <- runX (
                                                root [] [xmlTree] 
                                                >>>
                                                writeDocumentToString [(a_indent,v_1)]
                                               )
                                
                               ; pfoptArgs <- getPFOPTArgs args
			                   ; pfoptPath <- getPFOPTPath args

							   ;let 
     								pfoptCommandLine = (pfoptPath ++ " " ++ pfoptArgs)
       
                                 in
                                    do {
                                        
                                        (stdin,stdout,stderr,pHandle) <- runInteractiveCommand pfoptCommandLine  
                                         
                                        
                                        ;hPutStr stdin xmlString
                                        ;hClose stdin
                                        
                                                
                                        ;optimizedXMLPlan <- hGetContents stdout
                                        
                                        
                                        ;[documentNode] <- runX (
                                                       --readDocument [(a_validate,v_0)] fileName2
                                                       readString  [(a_validate,v_0)] optimizedXMLPlan
                                                        )
                                        
                                        
                                        ;checkOOPS stderr pfoptCommandLine xmlString
                                        
                                        
                                        ;let
                                            algbExp' =  importAlgebraPlan2 documentNode
                                            
                                         in
                                            
                                            return algbExp'
                                               
                                        
                                       } 
                                       
                                }   
                              



optimizeSubs args ((c, TI (algbExp, cols, subs)):tl) = do 
                                                tl' <- optimizeSubs args tl
                                                subs' <- optimizeSubs args subs
                                                algbExp' <- optimizeAlgbExp args algbExp
                                          
                                                return ((c, TI (algbExp', cols, subs')):tl')     
                                        
optimizeSubs args [] = do
                    return [] 




optimizeTableInfos args (TI (algbExp, cols, subs)) = do
                                        algbExp' <- optimizeAlgbExp args algbExp
                                        subs' <- optimizeSubs args subs
                                        
                                        return (TI (algbExp', cols, subs'))





getInputFile :: [String] -> IO String
getInputFile args =
                    let 
                        indexMaybe = elemIndex "-f" args
                     in
                        case indexMaybe of 
                            Just i -> do return (args !! (i+1))
                            Nothing -> do return "" 
         



pathSeparator = if os == "mingw32" then "\\" else "/"
appName = "mt"
appTmpDir = do                              
                s <- getTemporaryDirectory
                return (s ++ pathSeparator ++ "." ++ appName)
                

appTmpWrkDir = do
                    appTDir <-  appTmpDir
                    return (appTDir ++ pathSeparator ++ "wrk")                           



getLogDir args =
                     let 
                        indexMaybe = elemIndex "-ld" args
                     in
                        case indexMaybe of 
                            Just i -> do return (args !! (i+1))
                            Nothing ->  do
                                            appTDir <- appTmpDir           
                                            return (appTDir ++ pathSeparator ++ "log")
 
getODBCConnectionString args =  
                     let 
                        indexMaybe = elemIndex "-odbc" args
                     in
                        case indexMaybe of 
                            Just i -> do return (args !! (i+1))
                            Nothing ->  do
                                            error("You must specify a odbc connection string via the \"-odbc\" parameter.")
 
getLogFilePath args file =
                        do {
                            logDir <- getLogDir args
                            ; let logFilePath = logDir ++ pathSeparator ++ file   
                              in return logFilePath 
                            
                        }
                                     
 
 

log args fileName content =
                                do
                                    logFilePath <- getLogFilePath args fileName
                                    writeFile logFilePath content   
 
 


getPFOPTPath args =
   		                     let 
   		                        indexMaybe = elemIndex "-pfoptpath" args
   		                     in
   		                        case indexMaybe of 
   		                            Just i -> do return (args !! (i+1))
   		                            Nothing ->  do
   		                                            return "pfopt" 


 
getPFOPTArgs args =
                     let 
                        indexMaybe = elemIndex "-pfoptargs" args
                     in
                        case indexMaybe of 
                            Just i -> do return (args !! (i+1))
                            Nothing ->  do
                                            return "" 
                

getPFSQLPath args =
   		                     let 
   		                        indexMaybe = elemIndex "-pfsqlpath" args
   		                     in
   		                        case indexMaybe of 
   		                            Just i -> do return (args !! (i+1))
   		                            Nothing ->  do
   		                                            return "pfsql"



importXML args  = 

    let
        tableInfos = do
                            inputFilePath <- (getInputFile args)
                            [documentNode] <- runX (
                                           readDocument [(a_validate,v_0)] inputFilePath 
                                            )
                            return ((importQueryPlan documentNode),RT_Table) 
        
    in
        tableInfos







getStopBit args =
                    let 
                        indexMaybe = elemIndex "-s" args
                     in
                        case indexMaybe of 
                            Just i -> (read (args !! (i+1)))::Integer 
                            Nothing -> 9 



isExit args currentStopBit =
                            let
                                stopBit = getStopBit args
                            in
                                currentStopBit == stopBit
                                    
        

checkExit args currentStopBit =
                                if isExit args currentStopBit
                                    then
                                        do
                                            exitWith ExitSuccess
                                    else
                                        return () 
                                



checkLog args item renderer fileName = if (elem "-l" args)
                                    then
                                           do
                                                  s <- (renderer args item)
                                                  Main.log args fileName s
                                                  
                                    else
                                        return ()   


checkOutput args currentStopBit item renderer = 
                                        if isExit args currentStopBit
                                            then
                                                do
                                                    s <- (renderer args item)
                                                    putStrLn s
                                                
                                            else
                                                return ()



-------------------------------------------------------
-------------------------------------------------------
type LangRenderer = [String] -> Lang.Expr -> IO String
type LangCoreRenderer = [String] -> LangCore.Expr -> IO String
type TableInfosRenderer = [String] -> TableInfos -> IO String
type TableInfosSQLCodeRenderer = TypeRTSD -> [String] ->  [(Int, (TableInfos, String,  [[(String, SqlValue)]]))] -> IO String

-------------------------------------------------------
langASTRenderer :: LangRenderer
langASTRenderer args expr = return (show expr)
 
langPPRenderer :: LangRenderer
langPPRenderer args expr = return (PrettyPrint_Lang.prettyprint expr)

-------------------------------------------------------
langCoreASTRenderer :: LangCoreRenderer
langCoreASTRenderer args expr = return (show expr)
 
langCorePPRenderer :: LangCoreRenderer
langCorePPRenderer args expr = return (PrettyPrint_LangCore.prettyprint expr)

-------------------------------------------------------
tableInfosDAGDOTRenderer :: TableInfosRenderer
tableInfosDAGDOTRenderer args tableInfos = return (ti2dot tableInfos tiPseudoDAGMemorizer algbExpDAGMemorizer)

tableInfosTreeDOTRenderer :: TableInfosRenderer
tableInfosTreeDOTRenderer args tableInfos = return (ti2dot tableInfos tiPseudoDAGMemorizer algbExpPseudoDAGMemorizer)




tableInfosDAGXMLRenderer :: TableInfosRenderer
tableInfosDAGXMLRenderer args tableInfos = do

                                    [xml] <- runX (
                                                    root [] [ti2XML tableInfos tiPseudoDAGMemorizer algbExpDAGMemorizer getPFUniqueName] 
                                                    >>>
                                                    writeDocumentToString [(a_indent,v_1)]
                                                   )
                                    return xml

tableInfosTreeXMLRenderer :: TableInfosRenderer
tableInfosTreeXMLRenderer args tableInfos = do

                                    [xml] <- runX (
                                                    root [] [ti2XML tableInfos tiPseudoDAGMemorizer algbExpPseudoDAGMemorizer getPFUniqueName] 
                                                    >>>
                                                    writeDocumentToString [(a_indent,v_1)]
                                                   )
                                    return xml




tableInfosSQLQueriesRenderer :: TableInfosSQLCodeRenderer
tableInfosSQLQueriesRenderer t args tableInfos = annotatedTableInfosDAG2SQLQueriesString args tableInfos 


tableInfosSQLQueriesResultRenderer :: TableInfosSQLCodeRenderer
tableInfosSQLQueriesResultRenderer t args tableInfos = annotatedTableInfosDAG2SQLQueriesResult args tableInfos 


 
tableInfosProgramResultRenderer :: TableInfosSQLCodeRenderer
tableInfosProgramResultRenderer t args  ((_,(TI(q, cols, subs), _, rowMaps)):tl) = do return (renderResult (getResult  (TI(q, cols, subs)) rowMaps  tl "-1" t)) 

    
  
                




myParse args = do 
            
            inputFilePath <- (getInputFile args)
            
            input <- if inputFilePath == "" then getContents else readFile inputFilePath
            
            ;parseResult <- myparse inputFilePath input 
            
            ;case parseResult of
                Left err -> 
                            let
                                msg = "parse error at " ++ (show err) 
                            in 
                                 error(msg)      
                           
                Right expr -> 
                            do
                                return expr
                        

myNormalize args expr = do return (normalize expr (elem "-groupByAsMacro" args))



myAnnotate args expr = do return (annotate expr)

myRewriteAppend args expr = let
                                
                                args' = 
                                    if ((elem "-o2" args) && not(elem "-o1" args))
                                        then
                                            args ++ ["-o1"]
                                        else
                                            args
                                
                                expr'' =
                                    if (elem "-o1" args')
                                        then 
                                            let
                                                 expr'  = CoreRewrite_Append.rewrite expr
                                                 expr''  = CoreRewrite_Append2.rewrite expr'
                                                 expr''' = CoreRewrite_AppendOptimized.rewrite expr''
                                             in
                                                expr'''
                                        else
                                            expr
                                
                                expr''' = 
                                     if (elem "-o1" args' && elem "-o2" args')
                                        then 
                                            CoreRewrite_EmptySubListsAwareAppend.rewrite expr''
                                        else
                                            expr''
                                
                                expr'''' =
                                     if (elem "-o3" args')
                                        then 
                                            CoreRewrite_EmptySubListsAwareAggregates.rewrite expr'''
                                        else
                                            expr'''       
                                
                            
                            in
                                do return expr''''

    


myCompile args expr =   let
                            t = itypeSD expr
                            tableInfos = (wrapWithSerializeOps (compile expr))
                            
                        in
                             do 
                                return (tableInfos, t)
  
  
myDAGTableInfos tableInfos =   
                            let
                                (_id, tis) = ti2DAGCustom tableInfos tiPseudoDAGMemorizer
                                tis' = (reverse (zip [0..] tis))
                            in
                                do
                                    return tis'         
        
 
myRewrite1 args ti =
            let 
                ti' = AlgbRewrites.TIRewrite2.rewrite ti
            in
                do 
                    
                    -- checkLog args ti' tableInfosDAGXMLRenderer "_lalgDAG_afterRewrite1.xml"
                    -- checkLog args ti' tableInfosDAGDOTRenderer "_lalgDAG_afterRewrite1.dot"
                                        
                    return ti'   
                     
 
       
checkRewrite2 args ti = if (elem "-o4" args)
                            then
                                let
                                    ti' = AlgbRewrites.TIRewrite1.rewrite ti
                                in     
                                    do
                                        
                                       
                                        checkLog args ti tableInfosDAGXMLRenderer "_lalgOptimizedBeforeRewritten2DAG.xml"
                                        checkLog args ti tableInfosDAGDOTRenderer "_lalgOptimizedBeforeRewritten2DAG.dot"
                                        
                                       
                                        
                                        checkLog args ti' tableInfosDAGXMLRenderer "_lalgOptimizedAfterRewritten2DAG.xml"
                                        checkLog args ti' tableInfosDAGDOTRenderer "_lalgOptimizedAfterRewritten2DAG.dot"
                                        
                                        ti'' <- optimizeTableInfos args ti'
                                        return ti'' 
                                        
                            else
                                do 
                                   
                        
                                    return ti



   



run args = do {
    (tableInfos,t)    <- 
                        if (elem "-I" args)
                            then  (importXML args)
                                
                            
                            else  do
                                expr <- myParse args
                                checkLog args expr langASTRenderer "ast.show.txt"
                                checkLog args expr langPPRenderer "ast.pp.txt"
                                checkOutput args 1 expr (if (elem "-T" args) then langASTRenderer else langPPRenderer)
                                checkExit args 1
                                        
                                expr' <- myNormalize args expr
                                checkLog args expr' langCoreASTRenderer "astCore.show.txt"
                                checkLog args expr' langCorePPRenderer "astCore.pp.txt"
                                checkOutput args 2 expr' (if (elem "-T" args) then langCoreASTRenderer else langCorePPRenderer)
                                checkExit args 2
                                        
                                expr'' <- myAnnotate args expr'
                                checkLog args expr'' langCoreASTRenderer "astCoreAnnotated.show.txt"
                                checkLog args expr'' langCorePPRenderer "astCoreAnnotated.pp.txt"
                                checkOutput args 3 expr'' (if (elem "-T" args) then langCoreASTRenderer else langCorePPRenderer)
                                checkExit args 3
                                        
                                
                                expr''' <- myRewriteAppend args expr''
                                checkLog args expr''' langCoreASTRenderer "astCoreRewritten.show.txt"
                                checkLog args expr''' langCorePPRenderer "astCoreRewritten.pp.txt"
                                checkOutput args 4 expr''' (if (elem "-T" args) then langCoreASTRenderer else langCorePPRenderer)
                                checkExit args 4
                                
                                
                                (tableInfos, t) <- myCompile args expr'''
                                
                               
                                
                                -- checkLog args tableInfos tableInfosTreeDOTRenderer "lalgTree.dot"
                                -- checkLog args tableInfos tableInfosTreeXMLRenderer "lalgTree.xml"
                                -- checkOutput args 5 tableInfos (if (elem "-E" args) then tableInfosTreeXMLRenderer else tableInfosTreeDOTRenderer)
                                -- checkExit args 5
                                
                                
                                checkLog args tableInfos tableInfosDAGDOTRenderer "lalgDAG.dot"
                                checkLog args tableInfos tableInfosDAGXMLRenderer "lalgDAG.xml"
                                checkOutput args 6 tableInfos (if (elem "-E" args) then tableInfosDAGXMLRenderer else tableInfosDAGDOTRenderer)
                                checkExit args 6
                                
                                --return tableInfos 
                                return (tableInfos, t)  
    
          
    
    ;do
                        
        ----------------------------------------------------------------------------------------------
        -- Algebra-Optimization
        ----------------------------------------------------------------------------------------------
        
        tableInfos' <- myRewrite1 args tableInfos   
                      
        tableInfos'' <- optimizeTableInfos args tableInfos' 
        --tableInfos''' <- checkRewrite2 args tableInfos'' 
        
        checkLog args tableInfos'' tableInfosDAGDOTRenderer "lalgOptimizedDAG.dot" -- FIXME, was: tableInfos'''
        checkLog args tableInfos'' tableInfosDAGXMLRenderer "lalgOptimizedDAG.xml" -- FIXME, was: tableInfos'''
        checkOutput args 7 tableInfos''  (if (elem "-X" args) then tableInfosDAGXMLRenderer else tableInfosDAGDOTRenderer)                        
        checkExit args 7
        
        ----------------------------------------------------------------------------------------------
        -- SQL-Code-Generation
        ----------------------------------------------------------------------------------------------
        tableInfosDAG <- myDAGTableInfos tableInfos''  -- FIXME, was: tableInfos'''
        
        tableInfosDAG_sqlCode <- annotateTableInfosDAGWithSQLCode args tableInfosDAG 
        checkLog args tableInfosDAG_sqlCode (tableInfosSQLQueriesRenderer t) "sqlQueries.sql"
        checkOutput args 8 tableInfosDAG_sqlCode  (tableInfosSQLQueriesRenderer t)
        checkExit args 8
        
        
       
        ----------------------------------------------------------------------------------------------
        -- SQL-Code-Execution
        ----------------------------------------------------------------------------------------------
        
        tableInfosDAG_sqlResult <- annotateTableInfosDAGWithSQLResult args tableInfosDAG_sqlCode 
        checkLog args tableInfosDAG_sqlResult (tableInfosSQLQueriesResultRenderer t) "result.sqlQueries.txt"
        checkOutput args 9 tableInfosDAG_sqlResult (tableInfosSQLQueriesResultRenderer t)
        checkExit args 9
        
        ----------------------------------------------------------------------------------------------
        -- Result Construction
        ----------------------------------------------------------------------------------------------
        
        checkLog args tableInfosDAG_sqlResult (tableInfosProgramResultRenderer t) "result.txt"
        checkOutput args 10 tableInfosDAG_sqlResult (tableInfosProgramResultRenderer t)
        checkExit args 10
                        
                
                        
                
               
}



