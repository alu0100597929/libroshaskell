import Data.Char
import Control.Exception --catch

--hoy en día no se puede usar catch directamente, debemos definir esta función
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

miAccion :: IO()
miAccion = do putStr "Introduce texto: "
              xs <- getLine
              putStr "En mayúsculas es "
              putStr (map toUpper xs ++ "\n")

accs :: [IO()]
accs = [putStr "Dame tu nombre: ", responder]
  where
    responder = do xs <- getLine
                   putStr ("hola " ++ xs ++ "\n")

{- *Main> sequence_ accs          --sequence_ es una función predefinida que secuencia acciones de una lista
   Dame tu nombre: freinn
   hola freinn
-}

muestraFichero :: IO()
muestraFichero = do putStr "Introduce nombre de fichero: "
                    nombre <- getLine
                    contenido <- readFile nombre
                    putStr (contenido ++ "\n")

--muestraFichero' :: (Exception e) => IO()
muestraFichero' = muestraFichero `catchAny` manejador
  where
    manejador err = 
      do putStr ("Se produjo el error " ++ show err ++ "\n")
         muestraFichero'

--readLn (predefinida) puede ser usada para leer valores del teclado para cualquier tipo perteneciente
--a la clase Read

leerEntero :: IO Integer
leerEntero = readLn `catchAny` manejador
  where
    manejador err = do putStr "Error. Prueba de nuevo: "
                       leerEntero

demo :: IO()
demo = do putStr "Introduce un entero: "
          x <- leerEntero
          putStr ("El entero introducido es: " ++ show x ++ "\n")

type Tabulado = Int
data Doc = Vacio
         | Doc :<> Doc
         | Nl
         | Texto String
         | Tab Tabulado Doc

vacio :: Doc
vacio = Vacio

texto :: String -> Doc
texto s = Texto s

infixl 8 <>, :<>
(<>)     :: Doc -> Doc -> Doc
d1 <> d2 = d1 :<> d2

nl :: Doc
nl = Nl

tab :: Tabulado -> Doc -> Doc
tab i d = Tab i d

--implementación ineficiente ya que concatena a la izquierda
{-docAString :: Doc -> String
docAString d = docAString' 0 d

docAString' :: Tabulado -> Doc -> String
docAString' i Vacio = ""
docAString' i (d1 :<> d2) = docAString' i d1 ++ docAString' i d2
docAString' i Nl = "\n" ++ (espacios i)
docAString' i (Texto str) = str
docAString' i (Tab j d) = docAString' (i + j) d-}

espacios :: Int -> String
espacios n = replicate n ' '

--instanciamos el tipo Doc para la clase Show
instance Show Doc where
  show = docAString

--implementación eficiente concatenando a la derecha
docOpt :: Tabulado -> Doc -> String -> String
docOpt i Vacio s       = s
docOpt i (d1 :<> d2) s = docOpt i d1 (docOpt i d2 s)
docOpt i Nl s          = "\n" ++ (espacios i ++ s)
docOpt i (Texto str) s = str ++ s
docOpt i (Tab j d) s   = docOpt (i + j) d s

docAString :: Doc -> String
docAString d = docOpt 0 d ""