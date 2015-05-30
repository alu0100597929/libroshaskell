module Chapter3.ParamPoly where

maybeString (Just _) = "Just"
maybeString Nothing  = "Nothing"

data Client i = GovOrg { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                        , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving Show
 
data Person = Person { firstName :: String, lastName :: String }
            deriving Show

{-
*Chapter3.ParamPoly> :t Individual { clientId = "pepito", person = Person { firstName = "josito", lastName = "yoksetioxdxd" } }
Individual { clientId = "pepito", person = Person { firstName = "josito", lastName = "yoksetioxdxd" } }
  :: Client [Char]
*Chapter3.ParamPoly> :t GovOrg 'n' "NTTF"
GovOrg 'n' "NTTF" :: Client Char
-}

client1 = GovOrg 'n' "NTTF"
client2 = Individual { clientId = "pepito", person = Person { firstName = "josito", lastName = "yoksetioxdxd" } }

data Triple a b c = Triple a b c

data SamePair a = SamePair a a

-- exercise 3-1

--swapTriple :: (a,b,c) -> (b,c,a)
swapTriple (x,y,z) = (y,z,x)

-- duplicate :: a -> (a,a)
duplicate x = (x,x)

-- nothing :: a -> Maybe b
nothing _ = Nothing

-- index :: [a] -> [(b,a)]
index [] = []
index [x] = [(0,x)]
index (x:xs) = let indexed@((n,_):_) = index xs
               in (n+1,x):indexed

-- maybeA :: [a] -> Char
maybeA [] = 'a'