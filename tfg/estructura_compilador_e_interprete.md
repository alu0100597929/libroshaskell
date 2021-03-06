****** Estructura del programa de Write Yourself a Scheme in 48 hours ******

Estructura general del programa:

## Lección 1:

El tipo `IO` es instancia de la clase de tipos `Monad`, mónada es un concepto, decir que un valor pertenece a la clase de tipos `Monad` es decir:

1) Hay (un cierto tipo de) información oculta adjunta a este valor.
2) La mayoría de funciones no se tienen que preocupar de esa información.

En este caso:

La información extra son acciones IO que se harán usando los valores que se van pasando de una a otra; mientras que el valor básico (el cual tiene información adjunta) es void, la tupla vacía o unidad, ().

IO [String] e IO () pertenecen al mismo tipo, el de la mónada IO, pero tienen distintos tipos base. Actúan sobre (y se pasan unos a otros) valores de distintos tipos, [String] y ().

Los "valores con información oculta adjunta" son llamados "valores monádicos". 

Los "valores monádicos" se suele llamar "acciones", porque la manera más fácil de pensar en el uso de la mónada IO es pensar en una secuencia de acciones afectando al mundo exterior. Cada acción de la mencionada secuencia de acciones podría actuar sobre valores básicos (no monádicos). Por tanto:

* `m a` es una acción

* `(a -> m ())` es una función que devuelve una acción que contiene la tupla vacía o unidad `()`.

En los bloques do no se pueden mezclar acciones de mónadas diferentes.

Hay dos maneras de crear una acción IO:

* Elevar un valor ordinario en la mónada IO, usando la función return.

* Combinar dos acciones existentes.

Para combinar estas acciones, usamos un bloque do. Un bloque do consiste en una serie de líneas (las cuales tienen que tener la misma indentación). Cada línea puede tener una de estas dos formas:

* nombre <- acción1

* acción2

La primera forma liga el resultado de acción1 a nombre, para que esté disponible en las siguientes acciones. Por ejemplo, si el tipo de acción1 es IO [String], entonces el nombre estará ligado en todas las acciones y lo podremos usar en acciones posteriores, y esto se consigue mediante el operador bind (>>=).

En la segunda opción, simplemente ejecutamos la acción (por ejemplo, imprimir algo por pantalla) pero no ligamos nada a ningún nombre, ya que consideramos que no es necesario. Esto se consigue mediante el operador (>>).

Se suele usar el nombre de mónadas para denominar a la clase de tipos que describe como mínimo bind y return, pero cada mónada es diferente y sus funciones conseguirán distintos resultados. En el caso de la mónada IO, se ejecutan las acciones de modo secuencial, produciendo los efectos laterales externos que resulten de acciones.

## Lección 2:

Parsec (en realidad, genParser) es otro ejemplo de mónada: en este caso, la "información extra" que se encuentra "oculta" es toda aquella relativa a la posición en la cadena de entrada, registro de backtracking, conjuntos first y follow...etc.

la función parse devuelve un Either, que tendremos que manejar según construya un Left (ParseError) o Right (valor correcto). parse :: (Stream s Identity t) => Parsec s () a -> SourceName -> s -> Either ParseError a

readExpr: recibe una String (la cadena de entrada) y devuelve otra String con información de lo que haya parseado.

readExpr utiliza la función parse, que devuelve un Either, que readExpr maneja según construya un Left (error) o Right (valor correcto).

Luego se trata de ir parseando los diferentes tokens de Scheme y luego construir, mediante un constructor de valor para el tipo `LispVal`, un valor determinado.

Para ello se aplican parsers de Parsec y se extrae su información oculta de aquello que han parseado mediante el constructo `<-`, o se usa `liftM función valor\_monadico` (explicado en notas\_parser.txt).

Parsers recursivos:

En un lenguaje funcional la recursividad es uno de los métodos más interesantes. En un lenguaje como Scheme y muchos otros, encontramos estructuras de datos que pueden contener a otras, por ejemplo, una lista puede contener:

* otras listas (sean normales o dotted)

* cualquier otra expresión.

Por tanto, en el intérprete se llama recursivamente al parser principal, parseExpr :: Parser LispVal, para ir parseando lo que hay dentro de cada expresión.

Por ejemplo, en `parseList` y `parseDottedList` se usan, respectivamente:

    sepBy parseExpr spaces
    endBy parseExpr spaces

Los cuales van a devolver una `[LispVal]`, justo el argumento que necesita el constructor de tipo `List` y el primero que necesita `DottedList`.

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

Todas las funciones cuyo primer argumento es `LispVal` deben usar pattern matching para saber qué constructor de valor se ha usado para crear el mencionado `LispVal`.

## Lección 5: Evaluación, parte 2

`boolBinop` es una función definida para ser currificada, y por ello es muy versátil:

    boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
    boolBinop unpacker op args = if length args /= 2 
                                 then throwError $ NumArgs 2 args
                                 else do left <- unpacker $ args !! 0
                                          right <- unpacker $ args !! 1
                                          return $ Bool $ left `op` right

Gracias a ella podemos usar una función arbitraria que pase de LispVal a (ThrowsError a) y con ello crear una plantilla para una operación binaria entre los tipos que queramos:

    numBoolBinop  = boolBinop unpackNum
    strBoolBinop  = boolBinop unpackStr
    boolBoolBinop = boolBinop unpackBool

Esto nos permite ampliar nuestra lista de primitivas más cómodamente:

    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),

Ahora empieza lo interesante, implementemos condicionales sencillos, aún sin else:

    eval (List [Atom "if", pred, conseq, alt]) = 
         do result <- eval pred
            case result of
                 Bool False -> eval alt
                 otherwise  -> eval conseq

`car` de Scheme es como `head` de Haskell, y su implementación se basa en el reconocimiento de patrones:

    car :: [LispVal] -> ThrowsError LispVal
    car [List (x : xs)]         = return x
    car [DottedList (x : xs) _] = return x
    car [badArg]                = throwError $ TypeMismatch "pair" badArg
    car badArgList              = throwError $ NumArgs 1 badArgList

`cdr` de Scheme es  como `tail` de Haskell:

    cdr :: [LispVal] -> ThrowsError LispVal
    cdr [List (x : xs)]         = return $ List xs
    cdr [DottedList [_] x]      = return x
    cdr [DottedList (_ : xs) x] = return $ DottedList xs x
    cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
    cdr badArgList              = throwError $ NumArgs 1 badArgList

`cons` es el operador `(:)` de Haskell, es decir, el que sirve para concatenar un elemento a una lista del tipo de ese elemento. Si aplicamos `cons` a una lista que sólo contenga elementos, obtendremos una `DottedList`:

    cons :: [LispVal] -> ThrowsError LispVal
    cons [x1, List []] = return $ List [x1]
    cons [x, List xs] = return $ List $ x : xs
    cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
    cons [x1, x2] = return $ DottedList [x1] x2
    cons badArgList = throwError $ NumArgs 2 badArgList

Lo siguiente es definir una función que establezca un criterio de igualdad entre valores. Esto se hace recibiendo una lista con dos elementos, los valores a comparar (los cuales también pueden ser listas). Lo primero es comprobar que los dos valores son del mismo tipo, ya que estamos implementando una comparación fuerte. Si esto no ocurre, se devuelve falso. Si los dos valores son del mismo tipo, se les aplica la función `(==)` de Haskell:

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

Ahora definimos un cuantificador existencial (sí, aunque se llame `forall`, no es universal). Esto lo que hace es crear un constructor de valor `AnyUnpacker` que recibe funciones de `Lispval` a `ThrowsError a`, para todo tipo `a` que sea instancia de la clase de tipos `Eq`. Para usarlo debemos añadir a la cabecera de nuestro programa el pragma `{-# LANGUAGE ExistentialQuantification #-}`:

    data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

Aunque parezca muy extraño y novedoso, indagando un poco en por qué se le llama existencial nos daremos cuenta de que todo cobra sentido; esto es un constructor de tipos como otro cuaquiera, pero estamos obligando a que el tipo `a` sea instancia de la clase de tipos `Eq`. Luego, si y sólo si existe la instancia para `Eq` del tipo que queramos instanciar como tipo `Unpacker`, el compilador nos permitirá instanciarlo. Estamos ordenando nuestro código y evitando errores en tiempo de ejecución a costa de generar potenciales errores en tiempo de compilación, puro Haskell.

Nuestra intención es tener también un modo "débil" de comparar, y para ello lo que haremos será probar uno a uno todos nuestros unpackers, y desde el momento en que uno de ellos devuelva `True`, eso mismo devolveremos, y si en cambio todos devuelven `False`, entonces devolveremos `False`.

Lo primero que implementaremos será un helper que determinará si dos `LispVal` son iguales, usando un `Unpacker`, es decir, un desempaquetador arbitrario. Aquí lo que hacemos es crear un bloque `do` en el cual desempaquetamos los dos argumentos `arg1` y `arg2`, ligándolos a las variables `unpacked1` y `unpacked2`. Luego comprobamos su igualdad (serán casi seguro `LispVal`s) y Los volvemos a meter en la mónada `ThrowsError`.

    unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
    unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
                 do unpacked1 <- unpacker arg1
                    unpacked2 <- unpacker arg2
                    return $ unpacked1 == unpacked2
            `catchError` (const $ return False)


Ahora entra en juego `catchError`, que, recordemos, lo que hacía es recibir un `Either` y si es `Right`, devolver ese mismo `Either`, si es `Left`, aplicarle la función de la derecha.

¿Y lo de `const`? 

    const            :: a -> b -> a
    const x _        =  x

Veamos los tipos de cada una de las partes para entenderlo mejor:

    (const $ return False) :: Monad m => b -> m Bool
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

Menuda muerte en las expresiones case, pero ya funcionan. Para cambiar en una string los carácteres doblemente escapados por los simplemente escapados, debemos usar la función:

    foo :: String -> String
    foo s = read $ "\"" ++ s ++ "\""

Hack del else, cada vez que `eval` se encuentra un else, devuelve un `Bool True` para que siempre se ejecute esa expresión.