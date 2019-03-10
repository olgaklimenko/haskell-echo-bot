{-# LANGUAGE OverloadedStrings #-}

module Messengers.Telegram.Api where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (fromJust, maybe)
import qualified Data.Text as T
import Ext.Data.List
import Messengers.Telegram.Helpers
import Messengers.Telegram.Serializers
import Network.HTTP.Req
import Requests (get, post)

startPolling :: IO ()
startPolling = getMessages 0 >> pure ()

getMessages :: Int -> IO Int
getMessages offset = do
  rsp <- getUpdates offset
  either left right rsp
  where
    left errorMsg = print errorMsg >> getMessages offset
    right updateResponse =
      mapM_ handleUpdate updates >> getMessages (getNextOffset $ updates)
      where
        updates = updateResponseResult updateResponse

getUpdates :: Int -> IO (Either String UpdateResponse)
getUpdates offset = do
  token <- getToken
  let url = https "api.telegram.org" /: token /: "getUpdates"
  let options = "offset" =: offset
  rsp <- get url options
  pure $ either left right rsp
  where
    left errorMsg = Left $ show errorMsg
    right = eitherDecode . responseBody

getNextOffset :: [Update] -> Int
getNextOffset xs = list 0 ((+ 1) . last) $ fmap updateUpdateId xs

handleUpdate :: Update -> IO ()
handleUpdate upd = print "Handle update: " >> print upd

sendMessage :: Int -> T.Text -> Maybe InlineKeyboard -> IO LB.ByteString
sendMessage chatId text keyboard = do
  token <- getToken
  let url = https "api.telegram.org" /: token /: "sendMessage"
  let reqBody = SendMessageData chatId text
  rsp <-
    case keyboard of
      Nothing -> post url (SendMessageData chatId text)
      (Just k) ->
        post
          url
          (SendMessageWithInlineKeyboardData "Choose echo msg repeat times" k)
  pure $ responseBody rsp

helpHandler :: Message -> IO LB.ByteString
helpHandler (Message chat msg) =
  sendMessage (chatId chat) "Try /repeat command." Nothing -- TODO: вынести текст в конфиг

messageHandler :: Message -> IO LB.ByteString
messageHandler (Message chat msg) =
  sendMessage (chatId chat) (msgForSend msg) Nothing -- TODO: Смотреть в стейте юзера сколько раз слать
  where
    msgForSend Nothing = "Received empty message"
    msgForSend (Just m) = m

repeatHandler :: Message -> IO LB.ByteString
repeatHandler (Message chat msg) = sendMessage (chatId chat) msgForSend (Just keyboard)
  where
    msgForSend = "Choose echo msg repeat times"
    keyboard =
      InlineKeyboard
        [ [ InlineKeyboardButton "1" "repeat1"
          , InlineKeyboardButton "2" "repeat2"
          ]
        , [ InlineKeyboardButton "3" "repeat3"
          , InlineKeyboardButton "4" "repeat4"
          ]
        , [InlineKeyboardButton "5" "repeat5"]
        ]

-- callbackQueryHandler :: CallbackQuery -> IO LB.ByteString
-- callbackQueryHandler (CallbackQuery chat msg)