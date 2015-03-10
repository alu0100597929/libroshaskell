import Data.Complex
import Data.List

square x = x*x

{-main = do
  print $ square 2
  print $ square 2.1
  print $ square (2 :+ 1) --notación de los números complejos (clase Complex)-}

{-type Name   = String --type simplemente cambia el nombre de los tipos, serán lo mismo para el compilador
type Color  = String

showInfos :: Name ->  Color -> String
showInfos name color =  "Name: " ++ name
                        ++ ", Color: " ++ color
name :: Name
name = "Robin"
color :: Color
color = "Blue"-}

--main = putStrLn $ showInfos name color

--main = putStrLn $ showInfos color name

{-data Name   = NameConstr String --con la palabra reservada data creamos un tipo de verdad
data Color  = ColorConstr String--el compilador se quejará si cambiamos el orden de la llamada a main

showInfos :: Name ->  Color -> String
showInfos (NameConstr name) (ColorConstr color) =
      "Name: " ++ name ++ ", Color: " ++ color

name  = NameConstr "Robin"
color = ColorConstr "Blue"
main = putStrLn $ showInfos name color-}

--No funciona
{-data Complex = Num a => Complex { real :: a, img :: a}
c = Complex 1.0 2.0
z = Complex { real = 3, img = 4 }-}

{-main = print $ real c
	   print $ img c-}

{-data Knight = Knight { name :: String, quest :: String, favoriteColor :: String }

galaad = Knight { name = "Galaad, the pure"
                , quest = "To seek the Holy Grail"
                , favoriteColor = "The blue... No the red! AAAAAAHHHHHHH!!!!" }

showCharacter :: Knight -> String
showCharacter knight = "What is your name?\n"
    ++ "My name is " ++ name knight
    ++ "\nWhat is your quest?\n"
    ++ quest knight
    ++ "\nWhat is your favorite color?\n"
    ++ favoriteColor knight

main = do
  putStrLn $ showCharacter galaad-}

data Name = Name String
data Quest = Quest String
data Color = Color String

data Knight = Knight { name :: Name
                     , quest :: Quest
                     , favoriteColor :: Color }

showNameQuestion :: Name -> String
showNameQuestion (Name someName) = "What is your name? My name is " ++ someName

showQuestQuestion :: Quest -> String
showQuestQuestion (Quest someQuest) = "What is your quest? " ++ someQuest

showColorQuestion :: Color -> String
showColorQuestion (Color someColor) = "What is your favorite color? " ++ someColor
    
showCharacter :: Knight -> String
{-
--Esta versión no compila, debido a los tipos
showCharacter knight = showNameQuestion (favoriteColor knight) ++ "\n"
                       ++ showQuestQuestion (name knight ) ++ "\n"
                       ++ showColorQuestion (quest knight)
-}

showCharacter knight = showNameQuestion (name knight) ++ "\n"
                       ++ showQuestQuestion (quest knight ) ++ "\n"
                       ++ showColorQuestion (favoriteColor knight)

galaad = Knight { name = Name "Galaad, the pure"
                , quest = Quest "To seek the Holy Grail"
                , favoriteColor = Color "The blue... No the red! AAAAAAHHHHHHH!!!!" }

{-main = do
  putStrLn $ showCharacter galaad-}

infixr 5 :::
data List a = Nil | a ::: (List a)
              deriving (Show,Read,Eq,Ord)

convertList [] = Nil
convertList (x:xs) = x ::: convertList xs

data BinTree a = Empty
                 | Node a (BinTree a) (BinTree a)
                  deriving (Eq,Ord)

treeFromList :: (Ord a) => [a] -> BinTree a
treeFromList [] = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
                             (treeFromList (filter (>x) xs))

-- declare BinTree a to be an instance of Show
instance (Show a) => Show (BinTree a) where
  -- will start by a '<' before the root
  -- and put a : a begining of line
  show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
    where
    -- treeshow pref Tree
    --   shows a tree and starts each line with pref
    -- We don't display the Empty tree
    treeshow pref Empty = ""
    -- Leaf
    treeshow pref (Node x Empty Empty) =
                  (pshow pref x)

    -- Right branch is empty
    treeshow pref (Node x left Empty) =
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "`--" "   " left)

    -- Left branch is empty
    treeshow pref (Node x Empty right) =
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "`--" "   " right)

    -- Tree with left and right children non empty
    treeshow pref (Node x left right) =
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "|--" "|  " left) ++ "\n" ++
                  (showSon pref "`--" "   " right)

    -- shows a tree using some prefixes to make it nice
    showSon pref before next t =
                  pref ++ before ++ treeshow (pref ++ next) t

    -- pshow replaces "\n" by "\n"++pref
    pshow pref x = replace '\n' ("\n"++pref) (show x)

    -- replaces one char by another string
    replace c new string =
      concatMap (change c new) string
      where
          change c new x
              | x == c = new
              | otherwise = x:[] -- "x"

{-main = do
  putStrLn "Int binary tree:"
  print $ treeFromList [7,2,4,8,1,3,6,21,12,23]
-}
main = do
  putStrLn "\nBinary tree of Char binary trees:"
  print ( treeFromList
           (map treeFromList ["baz","zara","bar"]))

