{-# LANGUAGE DeriveGeneric #-}
module Config where

import           Control.Exception (throwIO)
import           Data.Aeson        (FromJSON)
import qualified Data.ByteString   as B
import Data.Word (Word16)
import           Data.Yaml         (decodeEither')
import           GHC.Generics      (Generic)
import           System.Directory  (getHomeDirectory)
import           Utils             (SimonException (..))


data SimonConfig = SimonConfig {
    pandoraHost     :: String,
    pandoraPort     :: Word16,
    pandoraUser     :: String,
    pandoraPassword :: String,
    pandoraProject  :: String,
    eagerDirs       :: [FilePath]
} deriving (Generic, Show)

instance FromJSON SimonConfig

readConfig :: IO SimonConfig
readConfig = do
    h <- getHomeDirectory
    bs <- B.readFile (h ++ "/.simon.config.yml")

    -- read yml files
    case decodeEither' bs of
        Left err     -> throwIO $ ConfigParsingException (show err)
        Right config -> return config
