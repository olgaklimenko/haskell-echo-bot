{-# LANGUAGE OverloadedStrings #-}

module Messengers.Telegram.Api where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (fromJust, fromMaybe, maybe)
import qualified Data.Text as T
import Debug.Trace
import Ext.Data.List
import Helpers (intToText)
import Messengers.Telegram.Helpers
import Messengers.Telegram.Serializers
import Network.HTTP.Req
import Requests (get, post)
import Users

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

commands = [("/help", helpHandler), ("/repeat", repeatHandler)]

callbackQueries =
  [ ("repeat1", callbackQueryHandler 1)
  , ("repeat2", callbackQueryHandler 2)
  , ("repeat3", callbackQueryHandler 3)
  , ("repeat4", callbackQueryHandler 4)
  , ("repeat5", callbackQueryHandler 5)
  ]

type Handler = Message -> IO LB.ByteString

routeMessage :: Message -> Maybe Handler
routeMessage msg =
  case messageText msg of
    Just text -> Just (fromMaybe messageHandler (lookup text commands))
    Nothing -> Nothing

routeCallbackQuery :: CallbackQuery -> Maybe Handler
routeCallbackQuery cQ =
  case callbackQueryData cQ of
    Nothing -> Nothing
    Just cqData ->
      Just (fromMaybe (callbackQueryHandler 1) (lookup cqData callbackQueries))

handleUpdate :: Update -> IO LB.ByteString
handleUpdate upd = do
  print "Handle update: " >> print upd
  case updateMessage upd of
    Just m -> (fromJust $ routeMessage m) m -- TODO: handle exception
    Nothing ->
      let cQuery = updateCallbackQuery upd
       in case cQuery of
            Nothing -> undefined -- TODO: No handler error
            Just cQ ->
              let handler = fromJust $ routeCallbackQuery cQ -- TODO: handle exception
                  msg = fromJust $ callbackQueryMessage cQ
               in handler msg

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
          (SendMessageWithInlineKeyboardData
             chatId
             "Choose echo msg repeat times"
             k)
  pure $ responseBody rsp

helpHandler :: Handler
helpHandler (Message chat msg) =
  sendMessage (chatId chat) "Try /repeat command." Nothing -- TODO: вынести текст в конфиг

messageHandler :: Handler
messageHandler (Message chat msg) =
  sendMessage (chatId chat) (fromMaybe "Received empty message" msg) Nothing -- TODO: Смотреть в стейте юзера сколько раз слать

repeatHandler :: Handler
repeatHandler (Message chat msg) =
  sendMessage (chatId chat) msgForSend (Just keyboard)
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

callbackQueryHandler :: Int -> Handler
callbackQueryHandler n (Message chat msg) =
  sendMessage (chatId chat) ("repeats: " <> intToText n) Nothing

