data Date = Date Int Int Int -- year, month, day

data Anniversary = Birthday String Date       -- name, date
                 | Wedding String String Date -- spouse name 1, spouse name 2, date

johnSmith :: Anniversary
johnSmith = Birthday "John Smith" (Date 1968 7 3)

smithWedding :: Anniversary
smithWedding = Wedding "John Smith" "Jane Smith" (Date 1987 3 4)

anniversariesOfJohnSmith :: [Anniversary]
anniversariesOfJohnSmith = [johnSmith, smithWedding]

--anniversariesOfJohnSmith = [Birthday "John Smith" 1968 7 3, Wedding "John Smith" "Jane Smith" 1987 3 4]

{-
showDate :: Int -> Int -> Int -> String
showDate y m d = show y ++ "-" ++ show m ++ "-" ++ show d
 
showAnniversary :: Anniversary -> String
showAnniversary (Birthday name year month day) =
   name ++ " born " ++ showDate year month day
showAnniversary (Wedding name1 name2 year month day) =
   name1 ++ " married " ++ name2 ++ " on " ++ showDate year month day
-}

-- Exercises:

showDate :: Date -> String
showDate (Date year month day) = show year ++ "-" ++ show month ++ "-" ++ show day

showAnniversary :: Anniversary -> String
showAnniversary (Birthday name date) = name ++ " born " ++ showDate date

showAnniversary (Wedding name1 name2 date) = name1 ++ " married " ++ name2
                                             ++ " on " ++ showDate date