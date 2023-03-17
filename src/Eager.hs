{-# LANGUAGE OverloadedStrings #-}
module Eager where

import           Utils

import           Control.Exception (throwIO)
import           Control.Monad     (forM, filterM)
import           Data.Aeson        (FromJSON (..), eitherDecodeFileStrict,
                                    withObject, (.:))
import           Data.Aeson.Key    (toString)
import           Data.Aeson.KeyMap (toList)
import           System.Directory  (listDirectory, doesDirectoryExist)
import           System.FilePath   ((</>), takeFileName)
-- import           System.FilePath            (takeBaseName, takeDirectory,
--                                              takeExtension, takeFileName, (</>))

data EagerSnpCovSingle = EagerSnpCovSingle {
    indName     :: String,
    coveredSnps :: Int,
    totalSnps   :: Int
} deriving (Show)

newtype EagerSnpCov = EagerSnpCov [EagerSnpCovSingle] deriving (Show)

instance FromJSON EagerSnpCov where
    parseJSON = withObject "EagerSnpCov" $ \v -> do
        o <- v .: "pconfig"
        o2 <- o .: "data"
        singles <- forM (toList o2) $ \(key, val) -> do
            c <- val .: "Covered_Snps"
            t <- val .: "Total_Snps"
            return $ EagerSnpCovSingle (toString key) c t
        return $ EagerSnpCov singles

findAllFiles :: [FilePath] -> FilePath -> IO [FilePath]
findAllFiles baseDirs filename = concat <$> mapM findAllFilesSingleDir baseDirs
  where
    findAllFilesSingleDir baseDir = do
        entries <- listDirectory baseDir
        let posFiles = map (baseDir </>) . filter (==filename) . map takeFileName $ entries
        subDirs <- filterM doesDirectoryExist . map (baseDir </>) $ entries
        morePosFiles <- fmap concat . mapM findAllFilesSingleDir $ subDirs
        return $ posFiles ++ morePosFiles

readEagerSnpCov :: [FilePath] -> IO EagerSnpCov
readEagerSnpCov baseDirs = do
    allFiles1 <- findAllFiles baseDirs "single_eigenstrat_coverage_mqc.json"
    allFiles2 <- findAllFiles baseDirs "double_eigenstrat_coverage_mqc.json"
    allSnpCovEntries <- forM (allFiles1 ++ allFiles2) parseEagerSnpCovJSON
    return . EagerSnpCov . concat $ [e | EagerSnpCov e <- allSnpCovEntries]

parseEagerSnpCovJSON :: FilePath -> IO EagerSnpCov
parseEagerSnpCovJSON fp = do
    eitherResult <- eitherDecodeFileStrict fp
    case eitherResult of
        Left e    -> throwIO $ EagerParsingException e
        Right obj -> return obj
