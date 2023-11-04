import Control.Monad (unless, when)
import Data.Char (isDigit, isSpace)
import System.Environment
import System.IO

processSrtFile :: FilePath -> IO ()
processSrtFile filePath = do
  withFile filePath ReadMode $ \handle -> do
    processLines handle

processLines :: Handle -> IO ()
processLines handle = do
  eof <- hIsEOF handle
  unless eof $ do
    line <- hGetLine handle
    when (shouldPrintLine line) (putStrLn line)
    processLines handle

shouldPrintLine :: String -> Bool
shouldPrintLine line = not (startsWithBracket line) && not (isTimestamp line) && not (isEmptyLine line) && not (startsWithNumber line)

startsWithBracket :: String -> Bool
startsWithBracket ('[' : _) = True
startsWithBracket _ = False

isTimestamp :: String -> Bool
isTimestamp line = "-->" `elem` words line

isEmptyLine :: String -> Bool
isEmptyLine = all isSpace

startsWithNumber :: String -> Bool
startsWithNumber line = not (null line) && isDigit (head line)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> processSrtFile filePath
    _ -> putStrLn "Usage: ./readSrtFile <file.srt>"
