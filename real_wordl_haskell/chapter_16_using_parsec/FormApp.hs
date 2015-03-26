import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many,optional,(<|>))
import Numeric

a_hex :: CharParser () Char
a_hex =
     char '%' *> (hexify <$> hexDigit <*> hexDigit)
  where hexify a b = toEnum . fst .  head . readHex $ [a,b]

{-
Input: toEnum 35::Char
Output: #
-}

a_char :: CharParser () Char
a_char = oneOf urlBaseChars
     <|> (' ' <$ char '+')
     <|> a_hex

a_pair_app1 =
  liftA2 (,) (many1 a_char) (optionMaybe (char '=' *> many a_char))

a_query :: CharParser () [(String, Maybe String)]
a_query = a_pair_app1 `sepBy` char '&'

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

testFA = parseTest a_query "foo=bar&a%21=b+c"