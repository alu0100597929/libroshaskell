import qualified Data.Foldable as F
import Data.Monoid

newtype Pair b a = Pair { getPair :: (a, b) }

-- cuando creamos un nuevo tipo, hay que definir fmap y todas sus funciones
instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

newtype CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

-- esto es simplemente un sinónimo de tipo, es decir, podemos usar estas dos maneras para
-- referirnos a lo mismo
type IntList = [Int]

-- usamos record syntax, con lo cual obtenemos una función que nos permite pasar del nuevo
-- tipo, al tipo original (que es envuelto por el nuevo)
newtype CharList = CharList { getCharList :: [Char] }

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

instance F.Foldable Tree where
  foldMap f EmptyTree = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend`
                           f x `mappend`
                           F.foldMap f r