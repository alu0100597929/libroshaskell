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
                >> readFile f           -- mucho mejor!
                >>= return . countChar c

-- readFile devuelve una IO String
countCharBroken c f =
  readFile f >>=
    \cs -> let count = countChar c cs
           in putStrLn ("Counted " ++ show count ++ " chars")
              >> return count

-- el let dentro de un bloque do no necesita 'in'
countCharLogDo :: Char -> FilePath -> IO Int
countCharLogDo c f = do
  cs <- readFile f
  let count = countChar c cs
  putStrLn $ "Counted " ++ show count ++ " chars"
  return count

countCharRevBind :: Char -> FilePath -> IO Int
countCharRevBind c f = return . countChar c =<< readFile f

countCharRevEta :: Char -> FilePath -> IO Int
countCharRevEta c = (return . countChar c =<<) . readFile

-- relación entre fmap y el bind inverso

incrementAllBy :: Int -> [Int] -> [Int]
incrementAllBy i is = fmap (+ i) is

countCharFmap :: Char -> FilePath -> IO Int
countCharFmap c f = fmap (countChar c) (readFile f)

countCharInfix :: Char -> FilePath -> IO Int
countCharInfix c f = countChar c <$> readFile f

listBindEx :: [Int] -> [Int]
listBindEx is = is >>= (\i -> replicate i i)

-- funciona como una list comprehension que simplemente concatena los casos
listBindTwo :: [Int] -> [Int] -> [Int]
listBindTwo is js = do
   i <- is
   j <- js
   [i,j]

-- Recuerda las Reglas de Transformación:

-- "a <- m
-- e" en 
-- "m >>= \a -> e"
-- y 
-- "m
-- n" en 
-- "m >> n"

listBindMia :: [Int] -> [Int] -> [Int]
listBindMia is js = is >>= 
                      \i -> js >>=
                        \j -> [i,j]

-- Por último, cambiaremos bind por su definición:
-- m >>= f = concat (fmap f m)

-- mejor hacer esto por partes, primero la más interna, obteniendo:
-- js >>= \j -> [i,j] == concat (fmap (\j -> [i,j]) js)

listBindDesplegada :: [Int] -> [Int] -> [Int]
listBindDesplegada is js = concat (fmap (\i -> concat (fmap (\j -> [i,j]) js)) is)