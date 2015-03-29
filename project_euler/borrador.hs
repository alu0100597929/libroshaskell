--borrador con ideas para los problemas de project euler

import qualified Data.Sequence as S

ayudante :: S.Seq Int -> S.Seq (S.Seq Int)
ayudante [] = []
ayudante [x] = []
ayudante [x,y] = []
ayudante xs = if length inicio == 2
                then [inicio]
                else inicio : tail inicio : ayudante inicio
  where
    inicio = init xs 

colas :: S.Seq Int -> S.Seq (S.Seq Int)
colas [] = []
colas [x,y] = [[x,y]]
colas entera@(x:xs) = entera : colas xs

listasConsecutivas xs = colas xs ++ ayudante xs