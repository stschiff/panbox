{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( renderCountries,
      renderSites,
      renderIndividuals,
      getPandoraConnection,
      readSidoraCredentials,
      Connection
    ) where

import           Eager                 (readEagerSnpCov)

import           Data.Text             (Text, unpack)
import           Data.Word             (Word16)
import           Database.MySQL.Simple (ConnectInfo (..), Connection, Only,
                                        connect, defaultConnectInfo, query_)
import           Prelude               hiding (readFile)
import           System.Environment    (getEnv)
import           System.IO.Strict      (readFile)
import           Text.Layout.Table     (asciiRoundS, column, def, expand, rowsG,
                                        tableString, titlesH)

readSidoraCredentials :: IO (String, Word16, String, String)
readSidoraCredentials = do
    h <- getEnv "HOME"
    [host, portStr, user, password] <- lines <$> readFile (h ++ "/.credentials")
    return (host, read portStr, user, password)

getPandoraConnection :: String -> Word16 -> String -> String -> IO Connection
getPandoraConnection host port user password = connect defaultConnectInfo {
    connectHost     = host,
    connectUser     = user,
    connectPassword = password,
    connectDatabase = "pandora",
    connectPort     = port
}

renderCountries :: Connection -> IO ()
renderCountries conn = do
    dat <- getCountries
    let colSpecs = replicate 2 (column def def def def)
        tableH = ["Country", "Nr_Inds"]
        tableB = map (\(s, n) -> [unpack s, show n]) dat
    putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]
  where
    getCountries :: IO [(Text, Int)]
    getCountries = query_ conn
        "SELECT S.Country, COUNT(I.Id) \
        \FROM TAB_Site AS S \
        \LEFT JOIN TAB_Individual AS I ON I.Site = S.Id \
        \WHERE I.Projects LIKE '%MICROSCOPE%' \
        \GROUP BY S.Country"

renderSites :: Connection -> IO ()
renderSites conn = do
    dat <- getSites
    let colSpecs = replicate 3 (column def def def def)
        tableH = ["Site", "Country", "Nr_Inds"]
        tableB = map (\(s, c, n) -> [unpack s, unpack c, show n]) dat
    putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]
  where
    getSites :: IO [(Text, Text, Int)]
    getSites = query_ conn
        "SELECT S.Full_Site_Id, S.Country, COUNT(I.Id) \
        \FROM TAB_Site AS S \
        \LEFT JOIN TAB_Individual AS I ON I.Site = S.Id \
        \WHERE I.Projects LIKE '%MICROSCOPE%' \
        \GROUP BY S.Full_Site_Id \
        \ORDER BY S.Country"

renderIndividuals :: Connection -> [FilePath] -> IO ()
renderIndividuals conn eagerDirs = do
    dat <- getSites
    eagerSnpCov <- readEagerSnpCov eagerDirs
    print eagerSnpCov
    let colSpecs = replicate 3 (column def def def def)
        tableH = ["Individual", "Country", "Nr_Samples"]
        tableB = map (\(s, c, n) -> [unpack s, unpack c, show n]) dat
    putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]
  where
    getSites :: IO [(Text, Text, Int)]
    getSites = query_ conn
        "SELECT I.Full_Individual_Id, S.Country, COUNT(Sa.Id) \
        \FROM TAB_Individual AS I \
        \LEFT JOIN TAB_Sample AS Sa ON I.Id = Sa.Individual \
        \LEFT JOIN TAB_Site AS S ON I.Site = S.Id \
        \WHERE Sa.Projects LIKE '%MICROSCOPE%' \
        \GROUP BY I.Full_Individual_Id \
        \ORDER BY I.Full_Individual_Id"
