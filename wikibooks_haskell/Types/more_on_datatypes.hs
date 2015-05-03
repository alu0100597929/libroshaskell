-- enumeración, todos los constructores son sin argumentos
data Month = January | February | March | April | May | June | July
           | August | September | October | November | December

-- no enumeración, un constructor recibe argumentos
data Colour = Black | Red | Green | Blue | Cyan
            | Yellow | Magenta | White | RGB Int Int Int

-- Bool es una enumeración
--data Bool = False | True
--    deriving (Bounded, Enum, Eq, Ord, Read, Show)

{-
Creación de un tipo de dato de manera normal, y luego accessors
creados manualmente

data Configuration = Configuration
    String   -- User name
    String   -- Local host
    String   -- Remote host
    Bool     -- Is guest?
    Bool     -- Is superuser?
    String   -- Current directory
    String   -- Home directory
    Integer  -- Time connected
  deriving (Eq, Show)

getUserName (Configuration un _ _ _ _ _ _ _) = un
getLocalHost (Configuration _ lh _ _ _ _ _ _) = lh
getRemoteHost (Configuration _ _ rh _ _ _ _ _) = rh
getIsGuest (Configuration _ _ _ ig _ _ _ _) = ig
-- And so on... 
-}

data Configuration = Configuration
    { username      :: String
    , localHost     :: String
    , remoteHost    :: String
    , isGuest       :: Bool
    , isSuperuser   :: Bool
    , currentDir    :: String
    , homeDir       :: String
    , timeConnected :: Integer
    }

{-
Esto genera automáticamente los siguientes accessors:

username :: Configuration -> String
localHost :: Configuration -> String
-- etc.

ejemplo de funciones que cambian y acceden respectivamente a un valor
Configuration

changeDir :: Configuration -> String -> Configuration
changeDir cfg newDir =
    if directoryExists newDir -- make sure the directory exists
        then cfg { currentDir = newDir }
        else error "Directory does not exist"
 
postWorkingDir :: Configuration -> String
postWorkingDir cfg = currentDir cfg

So, in general, to update the field x in a datatype y to z, you write y { x = z }.
You can change more than one; each should be separated by commas, for instance,
y {x = z, a = b, c = d }.

Esto NO altera el valor del "objeto" al que accedemos, sino que crea un
nuevo objeto con los valores que hayamos modificado cambiados. El objeto
original permanece tal y como estaba
-}

{-
pattern matching:

getHostData (Configuration { localHost = lh, remoteHost = rh }) = (lh, rh)

Esto lo que hace es enlazar a la variable lh el valor de localHost y a rh el valor
de remoteHost
-}

-- ejemplos de creación de valores, uno más corto y otro más claro

initCFG = Configuration "nobody" "nowhere" "nowhere" False False "/" "/" 0
 
initCFG' = Configuration
    { username      = "nobody"
    , localHost     = "nowhere"
    , remoteHost    = "nowhere"
    , isguest       = False
    , issuperuser   = False
    , currentdir    = "/"
    , homedir       = "/"
    , timeConnected = 0
    }

-- esto compila, pero si intentamos acceder a algún campo sin especificar,
-- obtendremos un error en tiempo de ejecución

cfgFoo = Configuration { username = "Foo" }
cfgBar = Configuraton { localHost = "Bar", remoteHost = "Baz" }
cfgUndef = Configuration {}

-- Muy buen ejemplo de Either
-- data Either a b = Left a | Right b

pairOff :: Int -> Either String Int
pairOff people
    | people < 0  = Left "Can't pair off negative number of people."
    | people > 30 = Left "Too many people for this activity."
    | even people = Right (people `div` 2)
    | otherwise   = Left "Can't pair off an odd number of people."
 
groupPeople :: Int -> String
groupPeople people =
    case pairOff people of
        Right groups -> "We have " ++ show groups ++ " group(s)."
        Left problem -> "Problem! " ++ problem