module Main (main) where

import           Config
import           Lib

import           Control.Applicative (many)
import qualified Options.Applicative as OP

data PandoraEntity = PandoraCountry | PandoraSite | PandoraIndividual | PandoraSample

data Options = Options {
    _optEntity    :: PandoraEntity,
    _optEagerDirs :: [FilePath]
}

main :: IO ()
main = do
    let parserInfo = OP.info (OP.helper <*> optParser) (OP.progDesc "MPI-EVA Pandora access CLI")
    (Options entity eagerDirsCLI) <- OP.execParser parserInfo
    (SimonConfig host port user password project eagerDirsConfig) <- readConfig
    conn <- getPandoraConnection host port user password
    case entity of
        PandoraCountry    -> renderCountries conn
        PandoraSite       -> renderSites conn
        PandoraIndividual -> renderIndividuals conn (eagerDirsConfig ++ eagerDirsCLI)
        _                 -> undefined

optParser :: OP.Parser Options
optParser = Options <$> entityOptParser <*> many eagerDirParser

entityOptParser :: OP.Parser PandoraEntity
entityOptParser = OP.argument (OP.eitherReader readEntity)
    (OP.help "The level to be queried: Can be any of 'Countries', 'Sites', 'Individuals', 'Samples'" <>
     OP.metavar "ENTITY")
  where
    readEntity :: String -> Either String PandoraEntity
    readEntity s = case s of
        "countries"   -> Right PandoraCountry
        "sites"       -> Right PandoraSite
        "individuals" -> Right PandoraIndividual
        "samples"     -> Right PandoraSample
        e             -> Left $ "Unknown entity type" ++ e

eagerDirParser :: OP.Parser FilePath
eagerDirParser = OP.strOption (OP.long "eagerDir" <> OP.short 'e' <> OP.help "The directory to read eager results from. Can be given multiple times" <> OP.metavar "DIR")
