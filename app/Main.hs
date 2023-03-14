module Main (main) where

import Lib
import qualified Options.Applicative as OP

data PandoraEntity = PandoraCountry | PandoraSite | PandoraIndividual | PandoraSample

data Options = Options {
    _optEntity :: PandoraEntity
}

optParser :: OP.Parser Options
optParser = Options <$> OP.argument (OP.eitherReader readEntity)
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

main :: IO ()
main = do
    let parserInfo = OP.info (OP.helper <*> optParser) (OP.progDesc "MPI-EVA Pandora access CLI")
    (Options entity) <- OP.execParser parserInfo
    (host, port, user, password) <- readSidoraCredentials
    conn <- getPandoraConnection host port user password
    case entity of
        PandoraCountry -> renderCountries conn
        PandoraSite    -> renderSites conn
        _              -> undefined
