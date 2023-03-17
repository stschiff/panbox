module Utils where

data SimonException = EagerParsingException String

instance Exception SimonException