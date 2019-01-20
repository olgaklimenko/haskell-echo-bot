{-# LANGUAGE OverloadedStrings #-}

module Messengers.Telegram.Helpers where
import qualified Data.Configurator as C
import qualified Data.Text as T
import Data.Monoid ((<>))

getToken :: IO T.Text
getToken = do 
    conf <- C.load [C.Required "conf/local.conf"]
    token <- C.lookup conf "telegram.botToken"
    pure $ liftToken token

liftToken :: (Maybe T.Text) -> T.Text
liftToken (Just token) = "bot" <> token
liftToken Nothing = error "Can't find Telegram botToken in conf"