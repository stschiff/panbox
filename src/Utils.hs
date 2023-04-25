module Utils where

import Control.Exception (Exception)

data SimonException =
      EagerParsingException String
    | ConfigParsingException String
    deriving (Show)

instance Exception SimonException
