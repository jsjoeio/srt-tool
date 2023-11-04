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
  if eof
    then return ()
    else do
      line <- hGetLine handle
      if not (startsWithBracket line) && not (isTimestamp line) && not (isEmptyLine line) && not (startsWithNumber line)
        then putStrLn line
        else return ()
      processLines handle

startsWithBracket :: String -> Bool
startsWithBracket line = case line of
  ('[' : _) -> True
  _ -> False

isTimestamp :: String -> Bool
isTimestamp line = "-->" `elem` words line

isEmptyLine :: String -> Bool
isEmptyLine line = null (filter (not . isSpace) line)

startsWithNumber :: String -> Bool
startsWithNumber line = not (null line) && isDigit (head line)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> processSrtFile filePath
    _ -> putStrLn "Usage: ./readSrtFile <file.srt>"
