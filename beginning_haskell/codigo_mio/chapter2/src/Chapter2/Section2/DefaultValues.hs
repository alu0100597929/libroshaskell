module Chapter2.Section2.DefaultValues where

data ConnType = TCP | UDP
data UseProxy = NoProxy | Proxy String
data TimeOut = NoTimeOut | TimeOut Integer
 
--data Connection = undefined -- Definition omitted
 
--connect :: String -> ConnType -> Integer -> UseProxy -> Bool -> Bool -> TimeOut -> Connection
--connect = undefined

--connectUrl :: String -> Connection
--connectUrl u = connect u TCP 0 NoProxy False False NoTimeOut

data ConnOptions = ConnOptions { connType :: ConnType
                               , connSpeed :: Integer
                               , connProxy :: UseProxy
                               , connCaching :: Bool
                               , connKeepAlive :: Bool
                               , connTimeOut :: TimeOut
                               }

--connect' :: String -> ConnOptions -> Connection
--connect' url options = undefined

connDefault :: ConnOptions
connDefault = ConnOptions TCP 0 NoProxy False False NoTimeOut