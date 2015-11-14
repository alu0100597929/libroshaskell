import Control.Monad

{-
After reading the documentation and tweaking a bit with foldM in GHCi,
I think I can explain what happened in your example. Let's reexamine
the type signature of foldM:

foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
From this type signature, we can conclude that foldM takes a 
function (a -> b -> m a) and applies it to each element of a list ([b]).
The second parameter is the initial value passed to the function in
the "first call". Subsequent calls use the resulting value of applying
the function (the one "extracted" from m a).

Thus, when you do:

ghci> let f = (\xs x -> [x] : [xs])
ghci> foldM f [] [1,2,3]
[[3],[2],[3],[1],[3],[2],[3],[]]
It is equivalent to:

ghci> ([] `f` 1) >>= (`f` 2) >>= (`f` 3)
ghci> [[3],[2],[3],[1],[3],[2],[3],[]]
If we break the line above into the following subexpressions we can see
more clearly what's going on:

ghci> ([] `f` 1)
ghci> [[1],[]]
ghci> ([] `f` 1) >>= (`f` 2)
ghci> [[2],[1],[2],[]]
...
The function f takes a list and a value as arguments, creates a singleton
list (putting the value in its own list) and adds it to a list of lists.
Initially, when we have an empty list, the result is obvious:
[[1],[]] (which is our "m a" in the type signature). Now, as I said before,
in order to call f again it's necessary to take the new "a" value from that
result. This time, we call f passing the extracted value and the next value
in the supplied list (i.e. 2 from [1,2,3]). The question is, considering
our "m a" is [[1],[]], which list should we pass as the first argument to f:
[1] or []? And the answer relies on the behaviour of the >>= operator for
lists, which can be viewed as non-deterministic computations, that applies
the given function to each element in the given list and combines the results.
For this specific step in the example, f will be called twice for two
different first parameters: f [1] 2 and f [] 2.

I tried to answer the question based on the example given by the author,
but the monadic chain used to explicit the behaviour of foldM in this
particular case can be used to reason about any Monad.
-}

f = (\xs x -> [x] : [xs])

foldMonad acc xs = foldM f acc xs

-- ejemplo para [1,2,3]
foldMonad' = [] `f` 1 >>= (`f` 2) >>= (`f` 3)

foldMonad'' = do
                x <- [] `f` 1
                y <- x `f` 2
                y `f` 3