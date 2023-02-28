{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( get_sites,
      get_pandora_connection,
      read_sidora_credentials
    ) where

import           Data.Text             (Text)
import           Data.Word             (Word16)
import           Database.MySQL.Simple (ConnectInfo (..), Connection, Only,
                                        connect, defaultConnectInfo, query_)
import           Prelude               hiding (readFile)
import           System.Environment    (getEnv)
import           System.IO.Strict      (readFile)

read_sidora_credentials :: IO (String, Word16, String, String)
read_sidora_credentials = do
    h <- getEnv "HOME"
    [host, portStr, user, password] <- lines <$> readFile (h ++ "/.credentials")
    return (host, read portStr, user, password)

get_pandora_connection :: String -> Word16 -> String -> String -> IO Connection
get_pandora_connection host port user password = connect defaultConnectInfo {
    connectHost     = host,
    connectUser     = user,
    connectPassword = password,
    connectDatabase = "pandora",
    connectPort     = port
}

get_sites :: Connection -> IO [(Text, Text)]
get_sites conn = query_ conn "SELECT DISTINCT Full_Site_Id, Country FROM TAB_Site WHERE Projects LIKE '%MICROSCOPE%'"
