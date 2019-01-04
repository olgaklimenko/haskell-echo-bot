module Lib
    ( someFunc
    ) where

import qualified Data.Configurator as C
import qualified Data.Text as T
import Data.Text (Text, pack)
import Telegram

getToken :: IO Text
getToken = do 
    conf <- C.load [C.Required "/home/olga/code/haskell-echo-bot/conf/local.conf"]
    let key = T.pack "telegram.botToken"
    token <- C.lookup conf key :: IO (Maybe String)
    return (liftToken token)

liftToken :: (Maybe String) -> Text
liftToken (Just token) = pack token
liftToken Nothing = error "Can't find Telegram botToken in conf"

someFunc :: IO ()
someFunc = do 
    token <- getToken
    Telegram.getUpdates token
