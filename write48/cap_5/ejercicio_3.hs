{-# LANGUAGE ExistentialQuantification #-} -- faltaba, para el forall

module Main where

import Data.Char (toLower)
import Control.Monad (liftM)
import Data.Array (Array (..), listArray)
import Data.Ratio (Rational (..), (%))
import Data.Complex (Complex (..))
import Numeric (readOct, readHex)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)


-----------------Parte nueva-----------------

-- nuevo, Control.Monad.Error está deprecated
import Control.Monad.Except -- cabal install mtl
import Data.List
import Debug.Trace
import Data.Either.Unwrap
--import Data.String.Utils (replace) --para las Strings

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
 
instance Show LispError where show = showError

{-
La línea de código está currificada, se podría escribir así también:
type ThrowsError b = Either LispError b
-}
type ThrowsError = Either LispError

main :: IO ()
main = do
    args <- getArgs
    let procesada = {-show $-} unlines $ lines (args !! 0)
    print procesada
    evaled <- return $ liftM show $ readExpr procesada >>= eval
    putStrLn $ extractValue $ trapError evaled
    --exp <- return $ readExpr (args !! 0)
    --putStrLn $ showVal $ extractValue exp

{-catchError: recibe un valor Either (una acción) y si es Right, lo devuelve, si es Left,
le aplica la función que recibe (en este caso está hardcoded, y lo que hace es
pasar del Left a un valor normal de LispVal). El sentido de todo esto es que
el Either resultado siempre tenga un valor Right:.-}
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- throwError takes an Error value and lifts it into the Left (error) constructor of an Either
-- es decir, pasa de (Error) a (Left LispError)
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> throwError $ Parser err
                   Right val -> return val

-- ejercicio 3, case, molaría forzar que el primer LispVal fuera List
-- primer LispVal, expr a evaluar
-- luego, "lista claves" "resultado"
-- | CasePair' CasePair
-- | CaseExpr LispVal [CasePair]

-- nuevo helper que busca un elemento en una lista, probada.
findList :: LispVal -> LispVal -> ThrowsError LispVal
findList el (List [])     = Right (Bool False)
findList el (List (x:xs)) = case eqv [el,x] of
                              Right (Bool True) -> Right (Bool True)
                              _ -> findList el (List xs)

-- nuevo: ayudante de eval que busca coincidencias en expresiones case
-- recibe una clave y la busca en cada lista, si está, devuelve el resultado
findLispVal :: LispVal -> [CasePair] -> Maybe LispVal
findLispVal clave []     = trace ("findLispVal vacía") Nothing
-- trace ("findLispVal " ++ showVal clave ++ " " ++ showVal (fst x))
findLispVal clave (x:xs) = case findList clave (fst x) of
                                  Right (Bool True) -> Just (snd x)
                                  _ -> findLispVal clave xs

--
-- Evaluador
--

{-
*Main> eval $ fromRight $ parse parseExpr "jaja" "(case (+ 2 2) ((4 9 2 1 2) 'd64\n((1 2) 'pepito\n((1) 'jorgito)"
4 (4 9 2 1 2)
Right "d64"
*Main> eval $ fromRight $ parse parseExpr "jaja" "(case (+ 1 1) ((4 9 1) 'd64\n((1 2) 'pepito\n((1) 'jorgito)"
2 (4 9 1)
2 (1 2)
Right "pepito"
*Main> eval $ fromRight $ parse parseExpr "jaja" "(case (+ 5 5) ((4 9 1) 'd64\n((1 2) 'pepito\n((10) 'jorgito)"
10 (4 9 1)
10 (1 2)
10 (10)
Right "jorgito"
-}

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
-- nuevo
eval (CaseExpr expr lista_pares) = do
    result <- eval expr --TRACE, en la línea siguiente: trace (showVal result) 
    case findLispVal result lista_pares of
      Nothing -> return (String "undefined")
      Just x -> return x
eval (List [Atom "if", pred, conseq, alt]) = 
     do result <- eval pred
        case result of
             Bool False -> eval alt
             Bool True  -> eval conseq
             _          -> throwError $ TypeMismatch "boolean predicate" pred
-- de lecciones anteriores
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

--ejercicio 3: nuevo parser para expresiones case

parseCaseResult :: Parser LispVal
parseCaseResult = do
    char '\''
    first <- (letter <|> symbol) --de momento será una String
    rest <- many (digit <|> letter <|> symbol)
    return $ String (first:rest)

parseCasePair :: Parser CasePair
parseCasePair = do
    list <- lexeme (char '(') >> (lexeme (char '(')) *> parseList <* (lexeme $ char ')')
    result <- lexeme $ parseCaseResult <* char ')'
    return (list, result)

parseCaseExpr :: Parser LispVal
parseCaseExpr = do
    lexeme $ char '('
    lexeme $ string "case"
    conditional_expr <- lexeme (char '(') *> parseList <* lexeme (char ')')
    -- armada en las nuevas líneas, solucionado con un parser más trabajado
    lista <- sepBy parseCasePair newline -- $ try (string "\\\\\n") <|> try (string "\n") <|> string "\r"
    return $ CaseExpr conditional_expr lista

-- parte nueva

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && 
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- parte de lecciones anteriores

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

--
-- Primitive functions lookup table
--
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+))
             ,("-", numericBinop (-))
             ,("*", numericBinop (*))
             ,("/", numericBinop div)
             ,("mod", numericBinop mod)
             ,("quotient", numericBinop quot)
             ,("remainder", numericBinop rem)
             ,("not", unaryOp not')
             ,("boolean?", unaryOp boolP)
             ,("list?", unaryOp listP)
             ,("symbol?", unaryOp symbolP)
             ,("char?", unaryOp charP)
             ,("string?", unaryOp stringP)
             ,("number?", unaryOp numberP)
             ,("vector?", unaryOp vectorP)
             ,("symbol->string", unaryOp symbol2string)
             ,("string->symbol", unaryOp string2symbol)
             -- parte nueva
             ,("=", numBoolBinop (==))
             ,("<", numBoolBinop (<))
             ,(">", numBoolBinop (>))
             ,("/=", numBoolBinop (/=))
             ,(">=", numBoolBinop (>=))
             ,("<=", numBoolBinop (<=))
             ,("&&", boolBoolBinop (&&))
             ,("||", boolBoolBinop (||))
             ,("string=?", strBoolBinop (==))
             ,("string<?", strBoolBinop (<))
             ,("string>?", strBoolBinop (>))
             ,("string<=?", strBoolBinop (<=))
             ,("string>=?", strBoolBinop (>=))
             ,("car", car)
             ,("cdr", cdr)
             ,("cons", cons)
             ,("eq?", eqv)
             ,("eqv?", eqv)
             ,("equal?", equal)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

-- parte de las lecciones anteriores

-- no nombrada en la web, pero había que hacer estos cambios
unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp func [arg] = return $ func arg

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

-----------------Parte nueva-----------------

--newtype ListaLispVal = ListaLispVal [LispVal]

type CasePair = (LispVal, LispVal)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Ratio Rational
             | Float Double
             | Complex (Complex Double)
             | String String
             | Char Char
             | Bool Bool
             | Vector (Array Int LispVal)
             | Nil () -- usarlo cuando convenga
             -- ejercicio 3, case, molaría forzar que el primer LispVal fuera List
             | CasePair' CasePair
             -- primer LispVal, expr a evaluar
             | CaseExpr LispVal [CasePair]

instance Show LispVal where show = showVal

--
-- LispVal Parsers
--

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseChar
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRatio
        <|> try parseNumber
        <|> try parseCaseExpr
        <|> parseBool
        <|> parseQuoted
        <|> parseQuasiquote
        <|> try parseUnquoteSplicing
        <|> parseUnquote
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               (return . Atom) (first:rest)

parseList :: Parser LispVal
parseList = fmap List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseNumber :: Parser LispVal
parseNumber = parsePlainNumber <|> parseRadixNumber

parsePlainNumber :: Parser LispVal
parsePlainNumber = many1 digit >>= return . Number . read

parseRadixNumber :: Parser LispVal
parseRadixNumber = char '#' >> 
                   (
                        parseDecimal 
                        <|> parseBinary
                        <|> parseOctal
                        <|> parseHex
                   )

parseDecimal :: Parser LispVal
parseDecimal = do char 'd'
                  n <- many1 digit
                  (return . Number . read) n

parseBinary :: Parser LispVal
parseBinary = do char 'b'
                 n <- many $ oneOf "01"
                 (return . Number . bin2int) n

parseOctal :: Parser LispVal
parseOctal = do char 'o'
                n <- many $ oneOf "01234567"
                (return . Number . (readWith readOct)) n

parseHex :: Parser LispVal
parseHex = do char 'x'
              n <- many $ oneOf "0123456789abcdefABCDEF"
              (return . Number . (readWith readHex)) n

parseRatio :: Parser LispVal
parseRatio = do num <- fmap read $ many1 digit
                char '/'
                denom <- fmap read $ many1 digit
                (return . Ratio) (num % denom)

parseFloat :: Parser LispVal
parseFloat = do whole <- many1 digit
                char '.'
                decimal <- many1 digit
                return $ Float (read (whole++"."++decimal))

parseComplex :: Parser LispVal
parseComplex = do r <- fmap toDouble (try parseFloat <|> parsePlainNumber)
                  char '+'
                  i <- fmap toDouble (try parseFloat <|> parsePlainNumber)
                  char 'i'
                  (return . Complex) (r :+ i)
               where toDouble (Float x) = x
                     toDouble (Number x) = fromIntegral x

parseString :: Parser LispVal
parseString = do char '"'
                 s <- many (escapedChars <|> (noneOf ['\\', '"']))
                 char '"'
                 (return . String) s

parseChar :: Parser LispVal
parseChar = do string "#\\"
               s <- many1 letter
               return $ case (map toLower s) of
                      "space" -> Char ' '
                      "newline" -> Char '\n'
                      [x] -> Char x

parseBool :: Parser LispVal
parseBool = do char '#'
               c <- oneOf "tf"
               return $ case c of
                      't' -> Bool True
                      'f' -> Bool False

parseQuasiquote :: Parser LispVal
parseQuasiquote = do char '`'
                     expr <- parseExpr
                     return $ List [Atom "quasiquote", expr]

-- Bug: this allows the unquote to appear outside of a quasiquoted list
parseUnquote :: Parser LispVal
parseUnquote = do char ','
                  expr <- parseExpr
                  return $ List [Atom "unquote", expr]

-- Bug: this allows unquote-splicing to appear outside of a quasiquoted list
parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do string ",@"
                          expr <- parseExpr
                          return $ List [Atom "unquote-splicing", expr]

parseVector :: Parser LispVal
parseVector = do string "#("
                 elems <- sepBy parseExpr spaces
                 char ')'
                 return $ Vector (listArray (0, (length elems)-1) elems)

--
-- Show functions
--

-- | CasePair' CasePair
-- | CaseExpr LispVal [CasePair]

showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Atom name) = name
showVal (Char c) = show c -- faltaba
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List xs) = "(" ++ unwordsList xs ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (CasePair' (a,b)) = "(" ++ show a ++ ", " ++ show b ++ ")"
showVal (CaseExpr expr lista_pares) = "(" ++ showVal expr ++ " "
                                      ++ unwords (map casePair2Str lista_pares) ++ ")"

casePair2Str :: CasePair -> String
casePair2Str (a,b) = showVal a ++ ", " ++ showVal b

--
-- Unary primitive defs all have type
-- LispVal -> LispVal

not' (Bool x) = (Bool . not) x
not' _ = Bool False

boolP (Bool _) = Bool True
boolP _ = Bool False

listP (List _) = Bool True
listP (DottedList _ _) = Bool True
listP _ = Bool False

symbolP (Atom _) = Bool True
symbolP _ = Bool False

charP (Char _) = Bool True
charP _ = Bool False

stringP (String _) = Bool True
stringP _ = Bool False

numberP (Number _) = Bool True
numberP _ = Bool False

vectorP (Vector _) = Bool True
vectorP _ = Bool False

symbol2string (Atom s) = String s
symbol2string _ = error "Expecting an Atom"

string2symbol (String s) = Atom s
string2symbol _ = error "Expecting a String"

--
-- Helpers
--

-- nuevo helper, lexeme
-- TODO: gran armada...lexeme se come cosas que luego usamos como separador...
ws :: Parser String
--ws = many (oneOf " \t\n")
ws = many (oneOf " \t")

-- mi propio combinador lexeme
lexeme :: Parser a -> Parser a
lexeme p = p <* ws -- whitespace

escapedChars :: Parser Char
escapedChars = do
             char '\\'
             c <- oneOf ['\\','"', 'n', 'r', 't']
             return $ case c of
                    '\\' -> c
                    '"'  -> c
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

bin2int :: String -> Integer
bin2int s = sum $ map (\(i,x) -> i*(2^x)) $ zip [0..] $ map p (reverse s)
          where p '0' = 0
                p '1' = 1

readWith :: (t -> [(a, b)]) -> t -> a
readWith f s = fst $ f s !! 0

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal