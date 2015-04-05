-- https://github.com/leonidas/codeblog/blob/master/2011/2011-12-18-haskell-nfa.md

import Control.Monad

data NFA q s = NFA
    { intialState :: q
    , isAccepting :: q -> Bool
    , transition  :: q -> s -> [q]
    }

testNFA :: NFA q s -> [s] -> Bool
testNFA (NFA i a t) = any a . foldM t i

data State  = Q1 | Q2 | Q3 | Q4 | Q5 deriving (Eq, Show)
data Symbol = A | B | C | D deriving (Eq, Show)

-- initial state
i = Q1

-- accept criteria
a = (`elem` [Q4,Q5])

-- state transitions
t Q1 A = [Q2]
t Q2 A = [Q3,Q4]
t Q2 B = [Q1,Q2]
t Q2 C = [Q3,Q4]
t Q3 D = [Q5]
t Q4 A = [Q2,Q4]
t _  _ = []

nfa = NFA i a t

{-
foldl :: (a -> b -> a) -> a -> [b] -> a

foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a

foldM _ a []     = return a
foldM f a (x:xs) = f a x >>= \fax -> foldM f fax xs

[Q1] >>= \q -> f q A

[Q1,Q2] >>= \q -> f q A
-}