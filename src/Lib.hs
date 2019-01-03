module Lib
    ( someFunc
    ) where

import qualified Data.Configurator as C
import qualified Data.Text as T

getToken :: IO String
getToken = do 
    conf <- C.load [C.Required "/home/olga/code/haskell-echo-bot/conf/local.conf"]
    let key = T.pack "telegram.botToken"
    token <- C.lookup conf key :: IO (Maybe String)
    return (liftToken token)

liftToken :: (Maybe String) -> String
liftToken (Just token) = token
liftToken Nothing = error "Can't find Telegram botToken in conf"

someFunc :: IO ()
someFunc = do 
    token <- getToken
    putStrLn token
    
