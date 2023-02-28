module Main (main) where

import Lib

main :: IO ()
main = do
    (host, port, user, password) <- read_sidora_credentials
    conn <- get_pandora_connection host port user password
    q <- get_sites conn
    print q
