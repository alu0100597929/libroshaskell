-- http://olenhad.me/articles/monadic-parsers/

data Parser a = Parser (String -> [(a, String)])

item :: Parser Char
item = Parser (\s -> case s of
                     "" -> []
                     (c:cs) -> [(c,cs)])

-- simplemente aplica un Parser
parse (Parser p) = p

{-
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser (\s -> concat $
                         map (\ (a, s') -> parse (f a) s')
                             $ parse p s)

unit :: a -> Parser a
unit a = Parser (\s -> [(a,s)])

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = item `bind` \c ->
              if p c then unit c else (Parser (\cs -> []))
-}

-- data Parser a = Parser (String -> [(a, String)])
-- fmap :: Functor f => (a -> b) -> f a -> f b

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
  fmap f (Parser a) = Parser (\ s -> map (\ (a, s') -> (f a, s') ) $ a s)

instance Monad Parser where
   return a = Parser (\s -> [(a,s)])
   p >>= f = Parser (\s -> concat $
                         map (\ (a, s') -> parse (f a) s')
                             $ parse p s)