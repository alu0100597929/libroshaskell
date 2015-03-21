-- http://olenhad.me/articles/monadic-parsers/

import qualified Control.Applicative hiding (many, many1)
import Control.Monad       (ap)
import Data.Char

data Parser a = Parser (String -> [(a, String)])

item :: Parser Char
item = Parser (\s -> case s of
                     "" -> []
                     (c:cs) -> [(c,cs)])

-- simplemente aplica un Parser
parse (Parser p) = p


bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser (\s -> concat $
                         map (\ (a, s') -> parse (f a) s')
                             $ parse p s)

unit :: a -> Parser a
unit a = Parser (\s -> [(a,s)])

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = item `bind` \c ->
              if p c then unit c else (Parser (\cs -> []))

-- data Parser a = Parser (String -> [(a, String)])
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- importante, el constructor de tipos va en las definiciones de tipos
-- por tanto, debemos sustituir f en este caso por Parser, que tiene kind * -> *
-- fmap :: (a -> b) -> Parser a -> Parser b

{-
/home/freinn/libroshaskell/tutoriales_parsec/olenhad/monadic_parsers.hs:31:31:
    Couldn't match expected type ‘String -> [(b, String)]’
                with actual type ‘b’
      ‘b’ is a rigid type variable bound by
          the type signature for fmap :: (a -> b) -> Parser a -> Parser b
          at /home/freinn/libroshaskell/tutoriales_parsec/olenhad/monadic_parsers.hs:31:3
-}

-- como siempre, se trata de hacer que los tipos encajen
instance Functor Parser where
  fmap f (Parser a) = Parser (\s -> map (\(a, s') -> (f a, s')) $ a s)

-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
instance Applicative Parser where
  pure a = Parser (\s -> [(a, s)])
  (<*>) = ap -- TODO, hacer esto sin chetos

instance Monad Parser where
  return a = Parser (\s -> [(a,s)])
  p >>= f = Parser (\s -> concat $
                       map (\ (a, s') -> parse (f a) s')
                           $ parse p s)

class Monad m => MonadPlus m where
      mzero :: m a
      mplus :: m a -> m a -> m a

-- mplus for the Parser is like an choice operator.
instance MonadPlus Parser where
         mzero = Parser (\cs -> [])
         mplus p q = Parser (\s -> (parse p) s ++ (parse q) s)

option :: Parser a -> Parser a -> Parser a
option  p q = Parser (\s -> case parse (mplus p q) s of
                                [] -> []
                                (x:xs) -> [x])

char :: Char -> Parser Char
char c = satisfies (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do { char c; string cs; return (c:cs)}

many :: Parser a -> Parser [a]
many p = many1 p `option` return []

many1 :: Parser a -> Parser [a]
many1 p = do { a <- p; as <- many p; return (a:as)}

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = (p `sepBy1` sep) `option` return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep = do a <- p
                    as <- many (do {sep; p})
                    return (a:as)

space :: Parser String
space = many (satisfies isSpace)
       where isSpace ' ' = True
             isSpace '\n' = True
             isSpace '\t' = True
             isSpace _ = False

token :: Parser a -> Parser a
token p = do { a <- p; space ; return a}

symb :: String -> Parser String
symb s = token (string s)

digit :: Parser Char
digit = satisfies (isDigit)

number :: Parser Int
number = do cs <- many1 digit
            return $ read cs

expr :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)

expr = term `chainl1` addop
term = factor `chainl1` mulop
factor = number `option` do { symb "("; n <- expr; symb ")"; return n}

addop = do {symb "+"; return (+)} `option` do {symb "-"; return (-)}
mulop = do {symb "*"; return (*)} `option` do {symb "/"; return (div)}

run :: String -> Int
run s = case parse expr s of
             [(num, _)] -> num

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) `option` return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
                 where rest a = (do f <- op
                                    b <- p
                                    rest (f a b))
                                `option` return a