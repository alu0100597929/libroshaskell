****** Estructura del programa de Write Yourself a Scheme in 48 hours ******

Estructura general del programa:

## Lección 1:

El tipo IO es instancia de la clase de tipos Monad, mónada es un concepto, decir que un valor pertenece a la clase de tipos Monad es decir:

1) Hay (un cierto tipo de) información oculta adjunta a este valor.
2) La mayoría de funciones no se tienen que preocupar de esa información.

En este caso:

La información extra son acciones IO que se harán usando los valores que se van pasando de una a otra; mientras que el valor básico (el cual tiene información adjunta) es void, la tupla vacía o unidad, ().

IO [String] e IO () pertenecen al mismo tipo, el de lamónada IO, pero tienen distintos tipos base. Actúan sobre (y se pasan unos a otros) valores de distintos tipos, [String] y ().

Los "valores con información oculta adjunta" son llamados "valores monádicos". 

Los "valores monádicos" se suele llamar "acciones", porque la manera más fácil de pensar en el uso de la mónada IO es pensar en una secuencia de acciones afectando al mundo exterior. Cada acción de la mencionada secuencia de acciones podría actuar sobre valores básicos (no monádicos).

## Lección 2:

Parsec (en realidad, genParser) es otro ejemplo de mónada: en este caso, la "información extra" que se encuentra "oculta" es toda aquella relativa a la posición en la cadena de entrada, registro de backtracking, conjuntos first y follow...etc.

la función parse devuelve un Either, que tendremos que manejar según construya un Left (ParseError) o Right (valor correcto). parse :: (Stream s Identity t) => Parsec s () a -> SourceName -> s -> Either ParseError a

readExpr: recibe una String (la cadena de entrada) y devuelve otra String con información de lo que haya parseado.

readExpr utiliza la función parse, que devuelve un Either, que readExpr maneja según construya un Left (error) o Right (valor correcto).

Luego se trata de ir parseando los diferentes tokens de Scheme y luego construir, mediante un constructor de valor para el tipo LispVal, un valor determinado.

Para ello se aplican parsers de Parsec y se extrae su información oculta de aquello que han parseado mediante el constructo <-, o se usa liftM funcion valor_monadico (explicado en notas_parser.txt).

Parsers recursivos:

En un lenguaje funcional la recursividad es uno de los métodos más interesantes. En un lenguaje como Scheme y muchos otros, encontramos estructuras de datos que pueden contener a otras, por ejemplo, una lista puede contener:

* otras listas (sean normales o dotted)
* cualquier otra expresión.

Por tanto, en el intérprete se llama recursivamente al parser principal, parseExpr :: Parser LispVal, para ir parseando lo que hay dentro de cada expresión.

Por ejemplo, en parseList y parseDottedList se usan, respectivamente:

sepBy parseExpr spaces
endBy parseExpr spaces

Los cuales van a devolver una [LispVal], justo el argumento que necesita el constructor de tipo List y el primero que necesita DottedList.

Por tanto, vemos que mediante el uso de sepBy y endBy estamos haciendo llamadas recursivas a readExpr y por ello nuestro parser es recursivo.

## Lección 3: evaluación, parte 1

Lo primero de todo es hacer LispVal instancia de Show para poder imprimir por pantalla los valores de tipo LispVal.

Para ello se crea la función showVal :: LispVal -> String y se iguala a show (instance Show LispVal where show = showVal). Para los dos tipos de listas, List y DottedList, se usa la función unwordsList:

    unwordsList :: [LispVal] -> String
    unwordsList = unwords . map showVal

Ahora empezamos con el evaluador propiamente dicho:

    eval :: LispVal -> LispVal
    eval val@(String _) = val
    eval val@(Number _) = val
    eval val@(Bool _) = val
    eval (List [Atom "quote", val]) = val

LispVal se puede ver como una expresión, y cambiaremos LispVal para que devuelva una expresión en vez de su valor de cadena de carácteres.

    readExpr :: String -> LispVal
    readExpr input = case parse parseExpr "lisp" input of
        Left err -> String $ "No match: " ++ show err
        Right val -> val

Cambiamos el código de la función main para que evalúe las expresiones en vez de sólo imprimirlas por pantalla:

    main :: IO ()
    main = getArgs >>= print . eval . readExpr . head

Añadimos a la función eval una ecuación que nos permitirá aplicar funciones a sus argumentos (para aplicar funciones se debe poner una lista cuyo primer elemento es el nombre de la función y luego los demás elementos serán los argumentos de dicha función):

eval (List (Atom func : args)) = apply func $ map eval args

Como vemos, tenemos hecha una ecuación que es recursiva, mapea eval sobre los argumentos, con lo cual ya tenemos resuelto el problema de evaluar listas anidadas de manera "gratuita".

    apply :: String -> [LispVal] -> LispVal
    apply func args = maybe (Bool False) ($ args) $ lookup func primitives

La función maybe recibe un valor por defecto, una función, y un valor de tipo Maybe. Si el valor de tipo Maybe es Nothing, la función devuelve el valor por defecto. Si no, aplica la función al valor dentro del Just y devuelve el resultado.

    primitives :: [(String, [LispVal] -> LispVal)]
    primitives = [("+", numericBinop (+)),
                  ("-", numericBinop (-)),
                  ("*", numericBinop (*)),
                  ("/", numericBinop div),
                  ("mod", numericBinop mod),
                  ("quotient", numericBinop quot),
                  ("remainder", numericBinop rem)]

    numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
    numericBinop op params = Number $ foldl1 op $ map unpackNum params
     
    unpackNum :: LispVal -> Integer
    unpackNum (Number n) = n
    unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
                               if null parsed 
                                  then 0
                                  else fst $ parsed !! 0
    unpackNum (List [n]) = unpackNum n
    unpackNum _ = 0

## Lección 4: Manejo de errores

Crearemos una mónada llamada ThrowsError, que en realidad se comporta como la mónada Either:

    type ThrowsError = Either LispError

la línea de arriba está currificada, se podría escribir así también:

    type ThrowsError b = Either LispError b

ThrowsError es, por tanto, una mónada que puede contener LispError (en el caso de Left) o un tipo b, que en nuestro programa es LispVal (en el caso de Right). Por tanto, cuando accedemos a su interior, encontraremos un LispError o un LispVal. Es decir, estamos definiendo un tipo que puede ser del tipo b, o bien dar error.

    readExpr :: String -> ThrowsError LispVal
    eval :: LispVal -> ThrowsError LispVal
    showVal :: LispVal -> String

`Either` es una mónada en la cual bind para su ejecución cuando encuentra un `Left`, devolviendo ese `Left` y ahorrando mucho tiempo de computación.

La mónada `Either` también provee otras dos funciones a parte de las monádicas estándar:

`throwsError`: recibe un valor de tipo Error y lo eleva al constructor Left (error) de un Either.

¿Por qué se usa throwsError? Porque en realidad, no existe el constructor de valor LispError, sino que es un constructor de tipo. Por ello, mediante throwsError, creamos un Left (el LispError concreto que sea), lo cual es un resultado de tipo ThrowsError LispVal, el tipo retorno de readExpr.

`catchError`: recibe un valor Either (una acción) y si es Right, lo devuelve, si es Left,
le aplica la función que recibe (en este caso está hardcoded, y lo que hace es pasar del Left a un valor normal de LispVal). El sentido de todo esto es que
el Either resultado siempre tenga un valor Right:

    trapError action = catchError action (return . show)

De este modo lo que hacemos es transformar los errores (Left) en su representación String metidos en la mónada Either.

Ahora que tenemos asegurado que todos los valores van a ser Right, hagamos un accessor
efectivo:

    extractValue :: ThrowsError a -> a
    extractValue (Right val) = val

La función parse devuelve un Either, que tendremos que manejar según construya un Left (ParseError) o Right (valor correcto). parse :: (Stream s Identity t) => Parsec s () a -> SourceName -> s -> Either ParseError a

Ahora `eval` va a devolver un valor monádico, con lo cual, en vez de map debemos usar mapM, y usar return para encapsular en valores monádicos los resultados de `eval`.

    mapM :: Monad m => (a -> m b) -> [a] -> m [b]:

`mapM mf xs` recibe una función monádica (con tipo Monad m => (a -> m b)) y la aplica a cada elemento en la lista xs; el resultado es una lista (con elementos del tipo b, en este caso) dentro de una mónada. Por tanto `mapM eval args` da como resultado `[LispVal]`.

    eval :: LispVal -> ThrowsError LispVal
    eval val@(String _) = return val
    eval val@(Number _) = return val
    eval val@(Bool _) = return val
    eval (List [Atom "quote", val]) = return val
    eval (List (Atom func : args)) = mapM eval args >>= apply func
    eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

    apply :: String -> [LispVal] -> ThrowsError LispVal
    apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                            ($ args)
                            (lookup func primitives)

    primitives :: [(String, [LispVal] -> ThrowsError LispVal)]

    numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
    numericBinop op           []  = throwError $ NumArgs 2 []
    numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
    numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

    unpackNum :: LispVal -> ThrowsError Integer
    unpackNum (Number n) = return n
    unpackNum (String n) = let parsed = reads n in 
                               if null parsed 
                                 then throwError $ TypeMismatch "number" $ String n
                                 else return $ fst $ parsed !! 0
    unpackNum (List [n]) = unpackNum n
    unpackNum notNum     = throwError $ TypeMismatch "number" notNum

Aquí lo más complicado es saber el tipo de evaled, así que vayamos por partes:

1) readExpr (args !! 0) >>= eval: readExpr da un ThrowsError LispVal, luego bind lo que hace es pasar el LispVal a eval, y acaba dando otro ThrowsError LispVal.

2) liftM show sobre la mónada ThrowsError LispVal da una ThrowsError String.

3) hacer `return` sobre una `ThrowsError String` nos devuelve un IO (Either ThrowsError String), y esto se hace para que al operar con el constructo `<-` sigamos teniendo el `Either ThrowsError String`, que es lo que recibe `trapError`.

* Recuerda, si estamos trabajando en un `do` de una mónada tipo IO, el `return` va a envolver el dato en una mónada IO, y esto es válido para cualquier mónada.

4) `trapError` nos devuelve un `Either` del tipo `Either String`, porque recordemos, `Left` era `LispError`, `Right` era `String`, y `catchError` siempre devuelve Right.

5) `extrackValue` nos devuelve un `String`

    main :: IO ()
    main = do
         args <- getArgs
         evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
         putStrLn $ extractValue $ trapError evaled

## Lección 5

Todas las funciones cuyo primer argumento es `LispVal` deben usar pattern matching para saber qué constructor de valor se ha usado para crear el mencionado `LispVal`.

`car` de Scheme es como `head` de Haskell. `cdr` de Scheme es  como `tail` de Haskell.

`cons` es el operador `(:)` de Haskell, es decir, el que sirve para concatenar un elemento a una lista del tipo de ese elemento. Si aplicamos `cons` a una lista que no contenga al menos una lista, obtendremos una `DottedList`.

Ahora definimos un cuantificador existencial (sí, aunque se llame `forall`, no es universal). Esto lo que hace es crear un constructor de valor `AnyUnpacker` que recibe funciones de `Lispval` a `ThrowsError a`, para todo tipo `a` que sea instancia de la clase de tipos `Eq`:

    data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

    unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
    unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
                 do unpacked1 <- unpacker arg1
                    unpacked2 <- unpacker arg2
                    return $ unpacked1 == unpacked2
            `catchError` (const $ return False)

Aquí lo que hacemos es crear un bloque `do` en el cual desempaquetamos los dos argumentos `arg1` y `arg2`, ligándolos a las variables `unpacked1` y `unpacked2`. Luego comprobamos su igualdad (serán casi seguro `LispVal`s) y Los volvemos a meter en la mónada `ThrowsError`.

Ahora entra en juego `catchError`, que, recordemos, lo que hacía es recibir un `Either` y si es `Right`, devolver ese mismo `Either`, si es `Left`, aplicarle la función de la derecha.

¿Y lo de `const`? 

    const            :: a -> b -> a
    const x _        =  x

Veamos los tipos de cada una de las partes para entenderlo mejor:

Prelude Control.Monad.Except> :t (const $ return False)
(const $ return False) :: Monad m => b -> m Bool
Prelude Control.Monad.Except> :t (return False)
(return False) :: Monad m => m Bool

Luego lo que está haciendo `const` es permitir una currificación que, da igual lo que reciba esa función (en este caso recibe un `Left` conteniendo el error), devolverá siempre lo primero que recibió, en este caso el resultado de `return False`, que no es otra cosa que un `ThrowsError Bool`.

    equal :: [LispVal] -> ThrowsError LispVal
    equal [arg1, arg2] = do
          primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                             [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
          eqvEquals <- eqv [arg1, arg2]
          return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
    equal badArgList = throwError $ NumArgs 2 badArgList

El tipo de `unpackEquals arg1 arg2` es `Unpacker -> ThrowsError Bool`, por tanto, `mapM (unpackEquals arg1 arg2)` sobre la lista `[AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]` dará una mónada `ThrowsError` conteniendo una lista de `Bool`, es decir, `ThrowsError [Bool]`. A dicha lista le aplicaremos la función `or` mediante `liftM`.