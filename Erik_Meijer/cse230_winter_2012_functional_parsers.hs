import Data.Char
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

newtype Parser a = P (String -> [(a, String)])

--Ahora todas las mónadas deben tener un código como este que las hace instancias de todo esto

instance Functor Parser where
  fmap = liftM

instance Applicative (Parser) where
  pure  = return
  (<*>) = ap

instance (Monad) Parser where
  (>>=)  = bindP
  return = returnP

returnP :: a -> Parser a
returnP x = P (\cs -> [(x, cs)])

bindP :: Parser t -> (t -> Parser a) -> Parser a
p1 `bindP` fp2 = P (\cs ->
  [(y, cs'') | (x, cs')  <- doParse p1 cs
             , (y, cs'') <- doParse (fp2 x) cs'])

doParse (P p) s = p s

oneChar = P (\cs -> case cs of
                 c:cs' -> [(c, cs')]
                 _     -> [])

twoChar0 = P (\cs -> case cs of
                 c1:c2:cs' -> [((c1,c2), cs')]
                 _         -> [])

--pairP ::  Parser a -> Parser b -> Parser (a,b)
--pairP p1 p2 = P (\cs -> 
--  [((x,y), cs'') | (x, cs' ) <- doParse p1 cs, 
--                   (y, cs'') <- doParse p2 cs']
--  )

twoChar9 = oneChar `pairP` oneChar

{-
twoChar :: Parser (Char, Char)
twoChar  = P (\cs -> case cs of
             c1:c2:cs' -> [((c1, c2), cs')]
             _         -> [])
-}

twoChar = pairP oneChar oneChar

pairP px py = do x <- px
                 y <- py
                 return (x, y)

failP = P $ const []

satP ::  (Char -> Bool) -> Parser Char
satP p = do c <- oneChar 
            if p c then
                     return c
                   else
                     failP

lowercaseP = satP isAsciiLower

alphaChar = satP isAlpha

digitChar = satP isDigit

digitInt = do c <- digitChar
              return ((read [c]) :: Int)

char c = satP (== c)

chooseP :: Parser a -> Parser a -> Parser a
p1 `chooseP` p2 = P (\cs -> doParse p1 cs ++ doParse p2 cs)

alphaNumChar = alphaChar `chooseP` digitChar

{-
ghci> doParse alphaNumChar "cat"
[('c',"at")]
ghci> doParse alphaNumChar "2cat"
[('2',"cat")]
ghci> doParse alphaNumChar "2at"
[('2',"at")]
-}

dosYDos = digitChar `chooseP` (char '2')

{-
parser creado por mí que ejemplifica qué ocurre si ambos parsers
tienen éxito en chooseP

*Main> doParse dosYDos "2400"
[('2',"400"),('2',"400")]
-}

{-grabn :: Int -> Parser String 
grabn n | n <= 0    = return ""
        | otherwise = do c  <- oneChar  
                         cs <- grabn (n-1)
                         return (c:cs)-}

--reducimos lo anterior a n aplicaciones de oneChar 
grabn n = sequence (replicate n oneChar)

grab2or4 = grabn 2 `chooseP` grabn 4

{-
ghci> doParse grab2or4 "mickeymouse"
[("mi","ckeymouse"),("mick","eymouse")]

ghci> doParse grab2or4 "mic"
[("mi","c")]

ghci> doParse grab2or4 "m"
[]
-}

intOp = plus `chooseP` minus `chooseP` times `chooseP` divide 
  where plus   = char '+' >> return (+)
        minus  = char '-' >> return (-)
        times  = char '*' >> return (*)
        divide = char '/' >> return div

calc = do x  <- digitInt
          op <- intOp
          y  <- digitInt 
          return $ x `op` y

