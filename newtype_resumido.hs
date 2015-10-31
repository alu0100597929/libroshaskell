-- Como vemos en el siguiente ejemplo, la función generada por newtype
-- recibe el tipo completo del constructor de tipos y devuelve lo que pone en la
-- declaración de la función generada por newtype

-- runMaybeT :: MaybeT m a -> m (Maybe a)
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
--      |________| ->                      |_________|

-- MaybeT :: m (Maybe a) -> MaybeT m a