import Prelude hiding ((<*>))

type Params = [String]
data User = User String String

getParam :: String -> Params -> Maybe String
getParam = undefined

userFromParams' :: Params -> Maybe User
userFromParams' params =
  case getParam "name" params of
    Just name -> case getParam "email" params of
                   Just email -> Just (User name email)
                   Nothing -> Nothing
    Nothing -> Nothing

-- userFromValues :: User
-- userFromValues = User aName anEmail

-- userFromMaybeValues :: Maybe User
-- userFromMaybeValues = User <$> aMaybeName <*> aMaybeEmail

-- fmap :: (a -> b) -> f a -> f b

--              a -> b
-- User :: String -> (String -> User)
--              f a          -> f b
-- fmap User :: Maybe String -> Maybe (String -> User)

-- getParam "name" params             :: Maybe String
-- fmap User                          :: Maybe String -> Maybe (String -> User)
-- fmap User (getParam "name" params) :: Maybe (String -> User)

-- (<$>) es sinónimo de fmap y permite escribir las cosas de manera más pro
-- User <$> getParam "name" params :: Maybe (String -> User)

-- fmapUser :: Maybe (String -> User)
-- fmapUser = User <$> getParam "name" params

-- aMaybeEmail :: Maybe String
-- aMaybeEmail = getParam "email" params

-- userFromParams :: Params -> Maybe User
-- userFromParams params = fmapUser <*> aMaybeEmail

-- (<*>) :: Maybe (String -> User) -> Maybe String -> Maybe User

(<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
(Just f) <*> (Just value) = Just $ f value 
_ <*> _ = Nothing

-- (<*>) :: f (a -> b) -> f a -> f b

userFromParams :: Params -> Maybe User
userFromParams params = User <$> getParam "name" params <*> getParam "email" params