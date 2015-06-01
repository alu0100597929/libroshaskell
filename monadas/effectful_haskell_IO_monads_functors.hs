import Data.Set (Set,singleton)
import Control.Applicative((<$>))

-- el tipo de main es una función de la cual sólo queremos los efecto laterales
main :: IO ()
main = putStrLn "Hello Wordl!"

sendSecret :: IO ()
sendSecret = writeFile "/tmp/secret" "Who is Benjamin Disraeli?"

andTheAnswerIs :: IO String
andTheAnswerIs = readFile "/tmp/secret"

-- pure function to count characters in a string
countChar :: Char -> String -> Int
countChar c = length . filter (==c)

-- use 'countChar' in IO on a file
countCharInFile :: Char -> FilePath -> IO Int
countCharInFile c filepath = do
    contents <- readFile filepath
    return $ countChar c contents

{-
*Main> :k IO
IO :: * -> *
-}

setProducingAction :: IO (Set String)
setProducingAction = return $ singleton "contrived"

-- IO String, means “run side-effects and give me a String result”
-- Por tanto, Set String indica que es un tipo Set que contiene Strings
-- Mientras que IO (Set String) indica que es un tipo IO que produce un (Set String)

returnIsEasy :: String -> IO String
returnIsEasy s = return $ s ++ " is in IOooooooo"

{-Las funciones cuyo tipo es monádico son diferentes a las funciones usuales
en vez de devolver la mónada, ellas "funcionan dentro" del contexto monádico
y los valores deben ser devueltos al contexto-}

ioNoReturn :: IO String
ioNoReturn = returnIsEasy "Elvis"

countCharInFileBind :: Char -> FilePath -> IO Int
countCharInFileBind c f = 
  readFile f >>= \cs -> return (countChar c cs)

countCharInFileEta :: Char -> FilePath -> IO Int
countCharInFileEta c f = readFile f >>= return . countChar c

countCharMsgBind :: Char -> FilePath -> IO Int
countCharMsgBind c f = 
   putStrLn ("count " ++ [c] ++ " in " ++ f) 
                >>= \_ -> readFile f    -- yuck
                >>= return . countChar c

countCharMsg :: Char -> FilePath -> IO Int
countCharMsg c f = 
   putStrLn ("count " ++ [c] ++ " in " ++ f) 
                >> readFile f           -- much better!
                >>= return . countChar c