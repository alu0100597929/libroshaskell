type Person = String

-- (Hijo/a, Madre, Padre)
personList :: [(Person,(Person, Person))]
personList = [("Juan",("Ana","Airam"))
             ,("Pedro",("Marta","Lola"))
             ,("Ana",("Lorena","Piter"))
             ,("Airam",("Miriam","Miguel"))]

father :: Person -> Maybe Person
father p = case lookup p personList of
             Nothing -> Nothing
             Just (x,y) -> Just y

mother :: Person -> Maybe Person
mother p = case lookup p personList of
             Nothing -> Nothing
             Just (x,y) -> Just x

maternalGrandfather :: Person -> Maybe Person
maternalGrandfather p =
    case mother p of
        Nothing -> Nothing
        Just mom -> father mom

bothGrandfathers :: Person -> Maybe (Person, Person)
bothGrandfathers p =
    case father p of
        Nothing -> Nothing
        Just dad ->
            case father dad of
                Nothing -> Nothing
                Just gf1 ->                          -- found first grandfather
                    case mother p of
                        Nothing -> Nothing
                        Just mom ->
                            case father mom of
                                Nothing -> Nothing
                                Just gf2 ->          -- found second grandfather
                                    Just (gf1, gf2)

maternalGrandfather' p = mother p >>= father

bothGrandfathers' p = father p >>= \f1 ->
                        father f1 >>= \gf1 ->
                          mother p >>= \m1 ->
                            mother m1 >>= \gf2 ->
                              Just (gf1, gf2)

maternalGrandfather'' p = do
                             m <- mother p
                             father m

bothGrandfathers'' p = do
                          f <- father p
                          gf1 <- father f
                          m <- mother p
                          gf2 <- mother m
                          return (gf1, gf2)

{-
*Main> maternalGrandfather' "Juan"
Just "Piter"
*Main> bothGrandfathers' "Juan"
Just ("Miguel","Lorena")
-}