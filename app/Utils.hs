module Utils where

import Data.Char (isDigit, isSpace)

startsWithBracket :: String -> Bool
startsWithBracket ('[' : _) = True
startsWithBracket _ = False

isTimestamp :: String -> Bool
isTimestamp line = "-->" `elem` words line

isEmptyLine :: String -> Bool
isEmptyLine = all isSpace

startsWithNumber :: String -> Bool
startsWithNumber line = not (null line) && isDigit (head line)