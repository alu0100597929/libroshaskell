-- preliminar

--main = do
--    putStrLn "Please enter your birth year"
--    year <- getLine
--    putStrLn $ "In 2020, you will be: " ++ show (2020 - read year)

-- versión 1

import Safe (readMay)

--main = do
--    -- We use explicit types to tell the compiler how to try and parse the
--    -- string.
--    print (readMay "1980" :: Maybe Integer)
--    print (readMay "hello" :: Maybe Integer)
--    print (readMay "2000" :: Maybe Integer)
--    print (readMay "two-thousand" :: Maybe Integer)

--main = do
--    putStrLn "Please enter your birth year"
--    yearString <- getLine
--    case readMay yearString of
--        Nothing -> putStrLn "You provided an invalid year"
--        Just year -> putStrLn $ "In 2020, you will be: " ++ show (2020 - year)

--import Safe (readMay)

--displayAge maybeAge =
--    case maybeAge of
--        Nothing -> putStrLn "You provided an invalid year"
--        Just age -> putStrLn $ "In 2020, you will be: " ++ show age

--yearToAge year = 2020 - year

--main = do
--    putStrLn "Please enter your birth year"
--    yearString <- getLine
--    let maybeAge =
--            case readMay yearString of
--                Nothing -> Nothing
--                Just year -> Just (yearToAge year)
--    displayAge maybeAge

--import Safe (readMay)

--displayAge maybeAge =
--    case maybeAge of
--        Nothing -> putStrLn "You provided an invalid year"
--        Just age -> putStrLn $ "In 2020, you will be: " ++ show age

--yearToAge year = 2020 - year

--main = do
--    putStrLn "Please enter your birth year"
--    yearString <- getLine
--    let maybeAge = fmap yearToAge (readMay yearString)
--    displayAge maybeAge

--import Safe (readMay)

--displayAge maybeAge =
--    case maybeAge of
--        Nothing -> putStrLn "You provided an invalid year"
--        Just age -> putStrLn $ "In 2020, you will be: " ++ show age

--yearToAge year = 2020 - year

--main = do
--    putStrLn "Please enter your birth year"
--    yearString <- getLine
--    let maybeAge = do
--            yearInteger <- readMay yearString
--            return $ yearToAge yearInteger
--    displayAge maybeAge

-- mónadas

--import Safe (readMay)

--displayAge maybeAge =
--    case maybeAge of
--        Nothing -> putStrLn "You provided invalid input"
--        Just age -> putStrLn $ "In that year, you will be: " ++ show age

--main = do
--    putStrLn "Please enter your birth year"
--    birthYearString <- getLine
--    putStrLn "Please enter some year in the future"
--    futureYearString <- getLine
--    let maybeAge =
--            case readMay birthYearString of
--                Nothing -> Nothing
--                Just birthYear ->
--                    case readMay futureYearString of
--                        Nothing -> Nothing
--                        Just futureYear -> Just (futureYear - birthYear)
--    displayAge maybeAge

import Safe (readMay)

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided invalid input"
        Just age -> putStrLn $ "In that year, you will be: " ++ show age

yearDiff futureYear birthYear = futureYear - birthYear

main = do
    putStrLn "Please enter your birth year"
    birthYearString <- getLine
    putStrLn "Please enter some year in the future"
    futureYearString <- getLine
    let maybeAge = do
            birthYear <- readMay birthYearString
            futureYear <- readMay futureYearString
            return $ yearDiff futureYear birthYear
    displayAge maybeAge