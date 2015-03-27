-- file: JSONParsec.hs
import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many,optional,(<|>))
import Numeric

-- la clase JSONClass del capítulo 6
newtype JAry a = JAry { fromJAry :: [a] } deriving (Eq, Ord, Show)

newtype JObj a = JObj { fromJObj :: [(String, a)] } deriving (Eq, Ord, Show)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)
            | JArray (JAry JValue)
            deriving (Eq, Ord, Show)

p_text :: CharParser () JValue
p_text = spaces *> text
     <?> "JSON text" -- recuerda, <?> lo que hace es informar del error si se produce
    where text = JObject <$> p_object -- <$> es sinónimo de fmap
             <|> JArray <$> p_array

-- between parsea el carácter de apertura, luego el parser, después el de cierre
-- se queda con lo que parser haya parseado
p_series :: Char -> CharParser () a -> Char -> CharParser () [a]
p_series left parser right =
    between (char left <* spaces) (char right) $
            (parser <* spaces) `sepBy` (char ',' <* spaces)

p_array :: CharParser () (JAry JValue)
p_array = JAry <$> p_series '[' p_value ']'

p_object :: CharParser () (JObj JValue)
p_object = JObj <$> p_series '{' p_field '}'
    where p_field = (,) <$> (p_string <* char ':' <* spaces) <*> p_value

p_value :: CharParser () JValue
p_value = value <* spaces
  where value = JString <$> p_string
            <|> JNumber <$> p_number
            <|> JObject <$> p_object
            <|> JArray <$> p_array
            <|> JBool <$> p_bool
            <|> JNull <$ string "null"
            <?> "JSON value"

p_bool :: CharParser () Bool
p_bool = True <$ string "true"
     <|> False <$ string "false"

p_value_choice = value <* spaces
  where value = choice [ JString <$> p_string
                       , JNumber <$> p_number
                       , JObject <$> p_object
                       , JArray <$> p_array
                       , JBool <$> p_bool
                       , JNull <$ string "null"
                       ]
                <?> "JSON value"

p_number :: CharParser () Double
p_number = do s <- getInput
              case readSigned readFloat s of
                [(n, s')] -> n <$ setInput s'
                _         -> empty

p_string :: CharParser () String
p_string = between (char '\"') (char '\"') (many jchar)
    where jchar = char '\\' *> (p_escape <|> p_unicode)
              <|> satisfy (`notElem` "\"\\")

p_escape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
    where decode c r = r <$ char c

p_unicode :: CharParser () Char
p_unicode = char 'u' *> (decode <$> count 4 hexDigit)
    where decode x = toEnum code
              where ((code,_):_) = readHex x