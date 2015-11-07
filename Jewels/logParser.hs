{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import Data.ByteString.Char8 as B8 hiding (lines, filter, unlines, head, readFile, take, length,
                                           putStrLn, tail, map, concat, or, writeFile, intersperse,
                                           groupBy, hGetContents)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Attoparsec.Text.Lazy hiding (take)
import Control.Applicative
import Control.Monad (replicateM, mapM, forM, forM_)
import Data.Either (either)
import Data.List (intersperse, groupBy)
import System.Environment
import qualified System.IO as SIO

data TimeStamp = MkTimeStamp T.Text
               deriving Show

data LogFileInfo = BackTraceLineInfo T.Text
                 | BackTraceInfo TimeStamp T.Text T.Text [LogFileInfo]
                 | Error TimeStamp T.Text
                 | LargeError TimeStamp T.Text T.Text
                 deriving Show

data LineType = SingleLineError TimeStamp T.Text
              | DirectoryInfo T.Text
              | ErrorInfo T.Text
              | LineBackTraceInfo T.Text
              | BackTraceString T.Text
              | BackTraceLine T.Text
              deriving Show

parseTimeStamp :: Parser TimeStamp
parseTimeStamp = do
  year <- many digit
  char '-'
  month <- many digit
  char '-'
  day <- many digit
  char ' '
  hour <- many digit
  char ':'
  minute <- many digit
  char ':'
  second <- many digit
  char ' '
  (return . MkTimeStamp) $ T.pack $ year ++ "-" ++ month ++ "-" ++ day ++ " " ++ hour ++ ":" ++ minute ++ ":" ++ second

parseError :: Parser LineType
parseError = do
  string $ "ERROR - "
  timeStamp <- parseTimeStamp
  errorInfo <- parseAnyLine
  return $ SingleLineError timeStamp errorInfo

parseDirectoryInfo :: Parser LineType
parseDirectoryInfo = do
  char '/'
  directoryInfo <- parseAnyLine
  (return . DirectoryInfo) $ T.append "/" directoryInfo

parseErrorInfo :: Parser LineType
parseErrorInfo = do
  errorInfo <- parseAnyLine
  (return . ErrorInfo) errorInfo

parseBackTraceString :: Parser LineType
parseBackTraceString = do
  let backTraceStr = " Backtrace: "
  string backTraceStr
  return $ BackTraceString $ T.fromStrict backTraceStr
  
parseBacktraceLine :: Parser LineType
parseBacktraceLine = do
  char '#'
  number <- many1 digit
  backTraceInfo <- parseAnyLine
  let numberPart = '#' : number
  return $ LineBackTraceInfo $ T.append (T.pack numberPart) backTraceInfo

parseAnyLine :: Parser T.Text
parseAnyLine = do
  lineStr <- many anyChar
  return $ T.pack lineStr

-- Skips n lines for allowing other parsers to succeed
skipNLines n = replicateM n $ manyTill anyChar endOfLine

-- performParser :: Parser a -> T.Text -> BackTraceInfo
performParser = parseOnly

getEitherRight :: Either a b -> b
getEitherRight (Right b) = b

-- try no sirve con attoparsec
parseLogLine :: Parser LineType
parseLogLine = parseError
           <|> parseDirectoryInfo
           <|> parseBacktraceLine
           <|> parseBackTraceString
           <|> parseErrorInfo

main = do
  (fileName : _) <- getArgs
  h <- SIO.openFile fileName SIO.ReadMode
  SIO.hSetEncoding h SIO.latin1
  fileContents <- SIO.hGetContents h
  let titleLength           = length fileName
      titleWithoutExtension = take (titleLength - 4) fileName
      allNonEmptyLines      = map (T.pack) $ tail $ filter (/= "") $ lines fileContents -- [T.Text]
      stringList = fmap (\x -> case eitherResult (parse parseLogLine x) of
                                 Left e  -> return $ show e
                                 Right a -> return $ show a) allNonEmptyLines
  -- aquí se procesaría la lista
  h <- SIO.openFile "errorSummary.txt" SIO.WriteMode
  forM_ stringList (\x -> hPutStrLn h $ B8.pack $ unlines x)
  SIO.hClose h

-- many anyChar CAN'T be used with applicative cause it does never stop