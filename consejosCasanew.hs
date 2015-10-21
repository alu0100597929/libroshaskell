-- cambiar las cosas estilo x <- return y por let x = y
-- catMaybes para evitar los fromMaybes!!!
-- no pasar como argumento el mismo que hemos recibido

Si tienes funtor y monad:

pure = return
af <*> ax = af >>= \f -> fmap f ax

para cualquier tipo