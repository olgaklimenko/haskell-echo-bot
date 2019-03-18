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
import Logger

startPolling :: UsersMonad IO ()
startPolling = getMessages 0 >> pure ()

getMessages :: Int -> UsersMonad IO Int
getMessages offset = do
  rsp <- liftIO $ getUpdates offset
  either left right rsp
  where
    left errorMsg = getMessages offset
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

type Handler = Message -> IO LB.ByteString

type HandlerMonad = Message -> UsersMonad IO LB.ByteString

routeMessage :: Message -> Maybe HandlerMonad
routeMessage msg =
  case messageText msg of
    Just text -> Just (fromMaybe messageHandler (lookup text commands))
    Nothing -> Nothing

routeCallbackQuery :: CallbackQuery -> Maybe HandlerMonad
routeCallbackQuery cQ =
  case callbackQueryData cQ of
    Nothing -> Nothing
    Just cqData ->
      Just (fromMaybe (callbackQueryHandler 1) (lookup cqData callbackQueries))

handleUpdate :: Update -> UsersMonad IO LB.ByteString
handleUpdate upd = do
  liftIO $ print "Handle update: " >> print upd
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

helpHandler :: HandlerMonad
helpHandler (Message chat msg) = do
  let cId = chatId chat
  getOrCreateUser cId
  liftIO $ sendMessage cId "Try /repeat command." Nothing -- TODO: вынести текст в конфиг

messageHandler :: HandlerMonad
messageHandler (Message chat msg) = do
  let cId = chatId chat
  mUser <- getOrCreateUser cId
  let r = repeats (fromJust mUser) -- TODO : exception
  liftIO $
    sendMsgNtimes r cId (fromMaybe "Received empty message" msg)

sendMsgNtimes :: Int -> Int -> T.Text -> IO LB.ByteString
sendMsgNtimes 1 cId msg = sendMessage cId msg Nothing
sendMsgNtimes n cId msg = do
  sendMessage cId msg Nothing
  sendMsgNtimes (n - 1) cId msg

repeatHandler :: HandlerMonad
repeatHandler (Message chat msg) = do
  let cId = chatId chat
  getOrCreateUser cId
  liftIO $ sendMessage cId msgForSend (Just keyboard)
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

callbackQueryHandler :: Int -> HandlerMonad
callbackQueryHandler n (Message chat msg) = do
  let cId = chatId chat
  getOrCreateUser cId
  changeRepeats cId n
  liftIO $ sendMessage cId ("repeats: " <> intToText n) Nothing

commands = [("/help", helpHandler), ("/repeat", repeatHandler)]

callbackQueries =
  [ ("repeat1", callbackQueryHandler 1)
  , ("repeat2", callbackQueryHandler 2)
  , ("repeat3", callbackQueryHandler 3)
  , ("repeat4", callbackQueryHandler 4)
  , ("repeat5", callbackQueryHandler 5)
  ]
