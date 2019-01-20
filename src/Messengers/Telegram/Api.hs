{-# LANGUAGE OverloadedStrings #-}

module Messengers.Telegram.Api where

import Control.Monad
import Data.Aeson
import Data.Maybe (fromJust, maybe)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB
import Network.HTTP.Req
import Messengers.Telegram.Parsers
import Messengers.Telegram.Helpers
import Ext.Data.List 
import Requests

getMessages :: Int -> IO Int
getMessages offset = do
    rsp <- getUpdates offset
    either left right rsp
  where
    left errorMsg = print errorMsg >> getMessages offset
    right updateResponse = mapM_ handleUpdate updates
        >> getMessages (getNextOffset $ updates)
        where 
            updates = updateResponseResult updateResponse

getUpdates :: Int -> IO (Either String (Either String UpdateResponse))
getUpdates offset = do
    token <- getToken
    let url = (https "api.telegram.org" /: token /: "getUpdates")
    let options = "offset" =: offset
    rsp <- get url options
    pure $ either left right rsp
    where  
        left errorMsg = Left $ show errorMsg
        right = Right . eitherDecode . responseBody

startPolling :: IO ()
startPolling =  getMessages 0 >> pure ()

getNextOffset :: [Update] -> Int
getNextOffset xs = list 0 ((+ 1) . last) $ fmap updateUpdateId xs

handleUpdate :: Update -> IO ()
handleUpdate upd = print "Handle update: " >> print upd
