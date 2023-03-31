{-# LANGUAGE OverloadedStrings #-}
module Eager where

import           Utils

import           Control.Exception (throwIO)
import           Control.Monad     (filterM, forM, sequence)
import           Data.Aeson        (FromJSON (..), eitherDecodeFileStrict,
                                    withObject, (.:))
import           Data.Aeson.Key    (toString)
import           Data.Aeson.KeyMap (toList)
import Data.List (isSuffixOf)
import           System.Directory  (doesDirectoryExist, listDirectory)
import           System.FilePath   (takeFileName, (</>))
import           System.IO         (hPrint, stderr)
import           Text.Read         (readMaybe)
-- import           System.FilePath            (takeBaseName, takeDirectory,
--                                              takeExtension, takeFileName, (</>))

newtype EagerSnpCov = EagerSnpCov [(String, (Int, Int))] deriving (Eq, Show)

newtype EagerSexDet = EagerSexDet [(String, (Double, Double, Double, Double))] deriving (Eq, Show)

instance FromJSON EagerSnpCov where
    parseJSON = withObject "EagerSnpCov" $ \v -> do
        o <- v .: "data"
        fmap EagerSnpCov . forM (toList o) $ \(key, val) -> do
            cStr <- val .: "Covered_Snps"
            tStr <- val .: "Total_Snps"
            case (,) <$> readMaybe cStr <*> readMaybe tStr of
                Just (c, t) -> return (toString key, (c, t))
                Nothing     -> fail "could not parse Covered and Total SNPs: "

instance FromJSON EagerSexDet where
    parseJSON = withObject "EagerSexDet" $ \v -> do
        let keyValuePairs = filter (isSuffixOf ".bam" . toString . fst) . toList $ v
        fmap EagerSexDet . forM keyValuePairs $ \(key, val) -> withObject "EagerSexDetInternal" (processInternal key) val
      where
        processInternal key o = do
            rateX <- o .: "RateX"
            rateY <- o .: "RateY"
            rateXerr <- o .: "RateErrX"
            rateYerr <- o .: "RateErrY"
            return (take 6 . toString $ key, (rateX, rateY, rateXerr, rateYerr))

findAllFiles :: [FilePath] -> FilePath -> IO [FilePath]
findAllFiles baseDirs filename = concat <$> mapM findAllFilesSingleDir baseDirs
  where
    findAllFilesSingleDir baseDir = do
        entries <- listDirectory baseDir
        let posFiles = map (baseDir </>) . filter (==filename) . map takeFileName $ entries
        subDirs <- filterM doesDirectoryExist . map (baseDir </>) $ entries
        morePosFiles <- fmap concat . mapM findAllFilesSingleDir $ subDirs
        return $ posFiles ++ morePosFiles

readEagerSnpCov :: [FilePath] -> IO [(String, (Int, Int))]
readEagerSnpCov baseDirs = do
    allFiles1 <- findAllFiles baseDirs "single_eigenstrat_coverage_mqc.json"
    allFiles2 <- findAllFiles baseDirs "double_eigenstrat_coverage_mqc.json"
    concat <$> mapM parseEagerSnpCovJSON (allFiles1 ++ allFiles2)

parseEagerSnpCovJSON :: FilePath -> IO [(String, (Int, Int))]
parseEagerSnpCovJSON fp = do
    eitherResult <- eitherDecodeFileStrict fp
    case eitherResult of
        Left e    -> throwIO $ EagerParsingException e
        Right (EagerSnpCov entries) -> return entries

parseEagerSexDet :: FilePath -> IO [(String, (Double, Double, Double, Double))]
parseEagerSexDet fp = do
    eitherResult <- eitherDecodeFileStrict fp
    case eitherResult of
        Left e    -> throwIO $ EagerParsingException e
        Right (EagerSexDet entries) -> return entries

readEagerSexDet :: [FilePath] -> IO [(String, (Double, Double, Double, Double))]
readEagerSexDet baseDirs = do
    f <- findAllFiles baseDirs "sexdeterrmine.json"
    concat <$> mapM parseEagerSexDet f