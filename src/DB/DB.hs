-----------------------------------------------------------------------------------------
{-| Module      : DB.DB
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module DB.DB where





import Data.List

import Char (isSpace)

import System
import System.Cmd

import System.Environment
import System.Directory
import System.Info

import System.IO
import System.Process

import Database.HDBC.ODBC (connectODBC)
import Database.HDBC





--odbcConnectionString = "DSN=SAMPLE;UID=db2admin;PWD=db2admin"

getConnection odbcConnectionString = handleSqlError $
    do 
        connection <- connectODBC odbcConnectionString
        return connection     


closeConnection connection = handleSqlError $
    do
        commit connection
        disconnect connection
          

-- strip trailing semicolon and white space (if any) from a string
stripSemi :: String -> String
stripSemi = reverse . dropWhile (\c -> c == ';' || isSpace c) . reverse

execute connection sqlQuery = handleSqlError $ 
   do
       stmt <- prepare connection (stripSemi sqlQuery)
       i <- Database.HDBC.execute stmt []        
       
       result <- fetchAllRowsAL' stmt 
       
       return result



    
    
                        
                        
getColValFromRowMapByColNamePrefix c rowMap =
            let 
                r = find (\ (colName, colVal) -> (isPrefixOf c colName) || (isInfixOf ("." ++ c) colName) ) rowMap
            in
                case r of
                    (Just (colName, colVal)) -> colVal 
                    _                        -> error("::" ++ c ++ "::" ++ (show rowMap))   
                

             
                
getStringValFromColVal colVal = case colVal of
        
        SqlString v -> v	
        
        -- never reached, as each value is generated/returned 
        -- as a string-value by the pf-sql-generator
        SqlByteString v -> show v	
        SqlWord32  v -> show v	
        SqlWord64 v  -> show v	
        SqlInt32  v -> show v	
        SqlInt64  v  -> show v	
        SqlInteger v  -> show v	
        SqlChar v   -> show v	
        SqlBool v  -> show v	
        SqlDouble v   -> show v	
        SqlRational  v -> show v	
        SqlEpochTime v  -> show v
        SqlTimeDiff  v  -> show v

        --SqlNull        -> 
        --_              -> error(show colVal)


