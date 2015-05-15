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
import Control.Monad.Error -- está deprecated
import Control.Monad.Except -- cabal install mtl, deberíamos usar esta
import Data.List
import Debug.Trace
import System.IO

-----------------Parte nueva-----------------
import Data.IORef
-- import System.Console.Readline

{-
Ejemplos de uso: tener en cuenta que no van las comillas en el modo intérprete!!!

Lisp>>> (case (+ 5 5) ((4 9 1) 'd64)\n((1 2) 'pepito)\n((10) 'jorgito))
jorgito
Lisp>>> (cond ((> 3 2) 'greater)\n((< 3 2) 'less))
greater
Lisp>>> (cond ((> 3 3) 'greater)\n((< 3 3) 'less)\n(else 'equal))
equal
-}

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

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

{-
La línea de código está currificada, se podría escribir así también:
type ThrowsError b = Either LispError b
-}
type ThrowsError = Either LispError

-- helper que cambia las dobles backslashes por una sola 
foo :: String -> String
foo s = read $ "\"" ++ s ++ "\""

-- nuevos helpers
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

{-
readPrompt :: String -> IO String
readPrompt prompt = do
    may <- readline prompt
    case may of
      Just str -> return str
      Nothing -> return ""
-}

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

-- código nuevo

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  = if var /= "else" && var /= "cond" && var /= "case"
                       then do env <- liftIO $ readIORef envRef
                               maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                                     (liftIO . readIORef)
                                     (lookup var env)
                       else return $ Bool True

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else case value of
               Atom "else" -> return $ Bool True
               _ -> liftIO $ do
                    valueRef <- newIORef value
                    env <- readIORef envRef
                    writeIORef envRef ((var, valueRef) : env)
                    return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn
 
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr (foo expr)) >>= eval env

-- Nueva implementación
runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)] 
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) 
        >>= hPutStrLn stderr
 
runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args

-- código viejo

{-catchError: recibe un valor Either (una acción) y si es Right, lo devuelve, si es Left,
le aplica la función que recibe (en este caso está hardcoded, y lo que hace es
pasar del Left a un valor normal de LispVal). El sentido de todo esto es que
el Either resultado siempre tenga un valor Right:.-}
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- throwError takes an Error value and lifts it into the Left (error) constructor of an Either
-- es decir, pasa de (Error) a (Left LispError)

-- ejercicio 3, case, molaría forzar que el primer LispVal fuera List
-- primer LispVal, expr a evaluar
-- luego, "lista claves" "resultado"
-- | CondCasePair' CondCasePair
-- | CaseExpr LispVal [CondCasePair]

-- nuevo helper que busca un elemento en una lista, probada.
findList :: LispVal -> LispVal -> ThrowsError LispVal
findList el (List [])     = Right (Bool False)
findList el (List (x:xs)) = case eqv [el,x] of
                              Right (Bool True) -> Right (Bool True)
                              _ -> findList el (List xs)

-- nuevo: ayudante de eval que busca coincidencias en expresiones case
-- recibe una clave y la busca en cada lista, si está, o si es un else,
-- devuelve el resultado
findLispVal :: LispVal -> [CasePair] -> Maybe LispVal
findLispVal clave []     = Nothing
findLispVal clave [x]    = case fst x of
                             List [Atom "else"] -> Just (snd x)
                             _ -> case findList clave (fst x) of
                                  Right (Bool True) -> Just (snd x)
                                  _ -> Nothing
findLispVal clave (x:xs) = case findList clave (fst x) of
                                  Right (Bool True) -> Just (snd x)
                                  _ -> findLispVal clave xs

-- nuevo helper que verifica si una condición es verdadera
checkCondition :: Env -> CasePair -> IOThrowsError LispVal
checkCondition env cond_expr = do
    result <- eval env (fst cond_expr)
    return $ proccessResult cond_expr result
    
-- TODO: añadir mensajes de error
proccessResult cond_expr result = case result of
                                    Bool True -> snd cond_expr
                                    _ -> Bool False

checkConds :: Env -> [CasePair] -> IOThrowsError LispVal
checkConds env []     = return $ String "undefined"
checkConds env (x:xs) = do result <- checkCondition env x
                           case result of
                             Bool False -> checkConds env xs
                             y -> return y

-- type IOThrowsError = ErrorT LispError IO

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

-- se quita la cabecera porque si no, falla el compilador
readExpr = readOrThrow parseExpr

readExprList = readOrThrow (endBy parseExpr spaces)

--
-- Evaluador, actualizado
--

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
-- nueva ecuación
eval env (List [Atom "load", String filename]) = 
     load filename >>= liftM last . mapM (eval env)
eval env (List [Atom "quote", val]) = return val
eval env (CaseExpr expr lista_pares) = do
    result <- eval env expr
    case findLispVal result lista_pares of
      Nothing -> return (String "undefined")
      Just x -> return x
eval env (List [Atom "if", pred, conseq, alt]) =
    do result <- eval env pred
       case result of
         Bool False -> eval env alt
         otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
-- nuevo
-- Esta ecuación faltaba, es parecido a set! pero definiendo variable
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

parseCaseResult :: Parser LispVal
parseCaseResult = do
    char '\''
    result <- parseExpr
    return result

parseCondElse :: Parser CasePair
parseCondElse = do
    atom_else <- lexeme (char '(') *> lexeme (parseAtom)
    result <- parseCaseResult <* lexeme (char ')')
    return (atom_else, result)

parseCasePair :: Parser CasePair
parseCasePair = do
    list <- lexeme (char '(') >> (lexeme (char '(')) *> parseList <* (lexeme $ char ')')
    result <- parseCaseResult <* char ')' -- no puede ser lexeme
    return (list, result)

parseCondExpr :: Parser LispVal
parseCondExpr = do
    lexeme $ char '('
    lexeme $ string "cond"
    lista <- sepBy (try parseCasePair <|> parseCondElse) (char ' ') -- (char '\\' >> char 'n')
    lexeme $ char ')'
    return $ CondExpr lista

-- las posibles acciones de un case se separan por líneas obligatoriamente, luego hubo
-- que arreglar el error de parseo derivado de que |n se lee como \\n, es decir,
-- una barra escapada y luego una n, esto se hizo con la función foo, encima
-- de main
parseCaseExpr :: Parser LispVal
parseCaseExpr = do
    lexeme $ char '('
    lexeme $ string "case"
    conditional_expr <- lexeme (char '(') *> parseList <* lexeme (char ')')
    lista <- sepBy parseCasePair (char ' ') -- (char '\\' >> char 'n')
    char ')'
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

-- helper para comparar listas
eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && 
                                                    (all eqvPair $ zip arg1 arg2)
  where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                             Left err -> False
                             Right (Bool val) -> val

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [l1@(List arg1), l2@(List arg2)]       = eqvList eqv [l1, l2]
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
equal [l1@(List arg1), l2@(List arg2)] = eqvList equal [l1, l2]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                       [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- nueva definición de apply

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
      if num params /= num args && varargs == Nothing
         then throwError $ NumArgs (num params) args
         else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = liftM last $ mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env
-- nueva ecuación
apply (IOFunc func) args = func args

-- nuevas funciones

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ liftM foo $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                               ++ map (makeFunc PrimitiveFunc) primitives)
     where makeFunc constructor (var, func) = (var, constructor func)

-- código de lecciones anteriores

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

-- nueva función
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

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
             -- primer lispVal, expr booleana a evaluar
             | CondExpr [CasePair]
             -- primer LispVal, expr a evaluar, segundo, lista de pares de casos
             | CaseExpr LispVal [CasePair]
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }
             -- nuevos constructores de datos
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

instance Show LispVal where show = showVal

--
-- LispVal Parsers
--

parseExpr :: Parser LispVal
parseExpr = (many spaces) >> (parseAtom
        <|> parseString
        <|> try parseChar
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRatio
        <|> try parseNumber
        <|> try parseCondExpr
        <|> try parseCaseExpr
        <|> parseBool
        <|> parseQuoted
        <|> parseQuasiquote
        <|> try parseUnquoteSplicing
        <|> parseUnquote
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x)

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

-- | CondCasePair' CondCasePair
-- | CaseExpr LispVal [CondCasePair]

showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Atom name) = name
showVal (Char c) = show c -- faltaba
showVal (Number n) = show n
showVal (Float f) = show f -- faltaba
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List xs) = "(" ++ unwordsList xs ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (CasePair' (a,b)) = "(" ++ show a ++ ", " ++ show b ++ ")"
showVal (CondExpr lista_conds) = "(" ++ unwords (map casePair2Str lista_conds) ++ ")"
showVal (CaseExpr expr lista_pares) = "(" ++ showVal expr ++ " "
                                      ++ unwords (map casePair2Str lista_pares) ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"
--nuevas ecuaciones
showVal (Port _)   = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

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