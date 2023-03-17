module Utils where

import Control.Exception (Exception)

newtype SimonException = EagerParsingException String deriving (Show)

instance Exception SimonException
