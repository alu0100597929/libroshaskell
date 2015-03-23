import Data.Char
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

newtype Parser a = P (String -> [(a,String)])

--Ahora todas las mónadas deben tener un código como este que las hace instancias de todo esto

instance Functor Parser where
  fmap = liftM

instance Applicative (Parser) where
  pure  = return
  (<*>) = ap

instance (Monad) Parser where
  (>>=)  = bindP
  return = returnP

returnP x = P (\cs -> [(x, cs)])

p1 `bindP` fp2 = P (\cs ->
  [(y, cs'') | (x, cs')  <- parse p1 cs
             , (y, cs'') <- parse (fp2 x) cs'])

item :: Parser Char
item = \inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x,xs)]

failure :: Parser a
failure = \inp -> []

return' :: a -> Parser a
return' v = \inp -> [(v,inp)]

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case p inp of
                       [] -> parse q inp
                       [(v,out)] -> [(v,out)]

-- esta función sería la identidad de los parsers
parse :: Parser a -> String -> [(a,String)]
parse p inp = p inp

{-p :: Parser (Char, Char)
p = do 
      x <- item
      item
      y <- item
      return (x,y)-}