{-# LANGUAGE OverloadedStrings #-}

module Messengers.Slack.Api where

import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (fromJust, fromMaybe, maybe)
import qualified Data.Text as T
import Ext.Data.List
import Network.HTTP.Req

import Messengers.Slack.Serializers
import Requests (get, post)
import Users

token = "" :: T.Text

channel = "" :: T.Text

startPolling :: IO ()
startPolling = getMessages "" >> pure ()

getMessages :: T.Text -> IO T.Text
getMessages timestamp = do
  print "Request with timestamp: " >> print timestamp
  rsp <- getUpdates timestamp
  either left right rsp
  where
    left errorMsg = getMessages timestamp
    right updateResponse =
      mapM_ handleMessage messages >>
      getMessages (getNextTimestamp timestamp messages)
      where
        messages = surMessages updateResponse

getNextTimestamp :: T.Text -> [SlackMessage] -> T.Text
getNextTimestamp lastTimestamp [] = lastTimestamp
getNextTimestamp _ (x:xs) = smTimestamp x

handleMessage :: SlackMessage -> IO LB.ByteString
handleMessage msg = do
  print "Handle message: " >> print msg
  pure undefined

getUpdates :: T.Text -> IO (Either String SlackUpdateResponse)
getUpdates timestamp = do
  let url = https "slack.com" /: "api" /: "conversations.history"
  let options =
        "token" =: token <> ("channel" =: channel) <> ("oldest" =: timestamp)
  rsp <- get url options
  let r = either left right rsp
  pure r
  where
    left errorMsg = Left $ show errorMsg
    right = eitherDecode . responseBody


sendMessage :: T.Text -> T.Text -> IO (Either String LB.ByteString)
sendMessage chatId text = do
  let url = https "slack.com" /: "api" /: "chat.postMessage"
  pure undefined
