-- ejercicioParser.hs

import Data.Char (isDigit)
import Data.List (span, null)

-- tipo general de cualquier parser que reciba un valor String:
-- String -> Maybe (String, a)

-- el primer parser del ejercicio
num :: String -> Maybe (String, Int)
num str = let (xs, ys) = span isDigit str
          in if null xs
               then Nothing
               else Just (ys, read xs)

anyChar :: String -> Maybe (String, Char)
anyChar (x:xs) = Just (xs,x)
anyChar _      = Nothing

anyChar2 :: String -> Maybe (String, (Char,Char))
anyChar2 xs = do
  (ys,x) <- anyChar xs
  (zs,y) <- anyChar ys
  return (zs,(x,y))

-- ¿Y si hacemos de esto un nuevo tipo y jugamos con él? A mi me parece una buena idea.
-- data Parser a = Parser (String -> Maybe (String, a))
-- Voilà! El tipo de los parsers de tipo a. Pero oye, este tipo a mi me suena. Si parametrizo String y Maybe...
-- data Parser s m a = Parser (s -> m (s,a))
-- ¡Y tanto que esto me suena! ¡Es el mismísimo StateT!
-- data StateT s m a = StateT (s -> m (a,s))