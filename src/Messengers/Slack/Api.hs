{-# LANGUAGE OverloadedStrings #-}

module Messengers.Slack.Api where

import BotMonad
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Maybe (fromJust, fromMaybe, maybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Exceptions (BotConfigException(..))
import Ext.Data.List
import Logger (logDebug)
import Messengers.Slack.Serializers
import Network.HTTP.Req
import Requests (get, post)
import Users

startPolling :: BotMonad IO ()
startPolling = getMessages "" >> pure ()

getMessages :: T.Text -> BotMonad IO T.Text
getMessages timestamp = do
  liftIO $ logDebug $ "Request with timestamp: " <> timestamp
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

handleMessage :: SlackMessage -> BotMonad IO LB.ByteString
handleMessage msg = do
  liftIO $ logDebug $ "Handle message: " <> smText msg
  pure undefined

getUpdates :: T.Text -> BotMonad IO (Either String SlackUpdateResponse)
getUpdates timestamp = do
  conf <- asks heConfig
  token <- liftIO $ getSlackToken conf
  channel <- liftIO $ getSlackChannel conf
  let url = https "slack.com" /: "api" /: "conversations.history"
      options =
        "token" =: token <> ("channel" =: channel) <> ("oldest" =: timestamp)
  rsp <- liftIO $ get url options
  let r = either left right rsp
  pure r
  where
    left errorMsg = Left $ show errorMsg
    right = eitherDecode . responseBody

sendMessage :: T.Text -> T.Text -> IO LB.ByteString
sendMessage chatId text = do
  let url = https "slack.com" /: "api" /: "chat.postMessage"
  let headers =
        header "Authorization" "Bearer " <>
        header "Content-type" "application/json" -- TODO: use token form config
      body = SlackSendMessageData chatId text
  rsp <- post url body headers
  pure $ responseBody rsp

getSlackToken :: C.Config -> IO T.Text
getSlackToken conf = do
  token <- C.lookup conf "slack.botUserToken"
  case token of
    Nothing -> throw $ NoTokenException "Slack"
    Just token -> pure token

getSlackChannel :: C.Config -> IO T.Text
getSlackChannel conf = do
  channel <- C.lookup conf "slack.channel"
  case channel of
    Nothing -> throw $ NoChannelException "Slack"
    Just channel -> pure channel
