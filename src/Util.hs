module Util where

import Text.Parsec
import Text.Parsec.String

type Solution = IO (String, String)

parseIt :: Parser a -> String -> a
parseIt p x = case parse p "" x of
                Left  err -> error (show err)
                Right res -> res
