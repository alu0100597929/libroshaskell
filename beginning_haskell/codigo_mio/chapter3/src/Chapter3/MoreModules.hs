module Chapter3.MoreModules where

-- import Data.List(permutations, subsequence) -- sólo importa las funciones permutations y subsequence
-- import Data.List hiding (tail) -- importa todo el módulo menos la función tail

-- error del libro, la siguiente línea da error
--import qualified Data.List (permutations, subsequences) as L

--solución del error del libro:
import Data.List as L(filter, null, permutations)

--import Chapter3.ParamPoly (Client()) -- sólo el tipo, no los constructores
--import Chapter3.ParamPoly (Client(GovOrg, Individual)) -- un subconjunto de constructores
--import Chapter3.ParamPoly (Client(..)) -- importa todos los constructores

-- permutationsStartingWith :: Char -> String -> [String]
-- permutationsStartingWith letter = filter (\p -> if not (null p) then head p == letter else False ) . permutations

permutationsStartingWith :: Char -> String -> [String]
permutationsStartingWith letter = L.filter (\p -> if not (L.null p) then head p == letter else False ) . L.permutations