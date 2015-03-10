--Tony Morris - Explain List Folds to Yourself - BFPG 2013-04-23
--http://vimeo.com/64673035

{-
Prelude> ((1:2:3:[]):(4:5:6:[]):[])
[[1,2,3],[4,5,6]]

Prelude> let append list1 list2 = foldr (:) list2 list1
Prelude> append [1,2,3] [4,5,6]
[1,2,3,4,5,6]
Prelude> let conc x = foldr append [] x
Prelude> conc 
Prelude> conc [[1,2],[3,4]]
[1,2,3,4]
Prelude> conc [[1,2],[3,4],[5,6]]
[1,2,3,4,5,6]

Prelude> let ($$) f a = f a
-}