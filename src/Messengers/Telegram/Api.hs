{-# LANGUAGE OverloadedStrings #-}

module Messengers.Telegram.Api where

import BotMonad
import Control.Exception (throw)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Maybe (fromJust, fromMaybe, maybe)
import qualified Data.Text as T
import Exceptions (BotConfigException(..))
import Ext.Data.List (list)
import Helpers (intToText)
import Logger (logDebug)
import Messengers.Telegram.Serializers
import Network.HTTP.Req
import qualified Requests as R
import Users
import Config (loadConfig, getConf)

startPolling :: BotMonad IO ()
startPolling = getMessages 0 >> pure ()

getMessages :: Int -> BotMonad IO Int
getMessages offset = do
  rsp <- getUpdates offset
  either left right rsp
  where
    left errorMsg = getMessages offset
    right updateResponse =
      mapM_ handleUpdate updates >> getMessages (getNextOffset updates)
      where
        updates = tuResponseResult updateResponse

getUpdates :: Int -> BotMonad IO (Either String TelegramUpdateResponse)
getUpdates offset = do
  token <- asks beToken
  let url = https "api.telegram.org" /: token /: "getUpdates"
  let options = "offset" =: offset
  rsp <- liftIO $ R.get url options
  pure $ either left right rsp
  where
    left errorMsg = Left $ show errorMsg
    right = eitherDecode . responseBody

getNextOffset :: [TelegramUpdate] -> Int
getNextOffset xs = list 0 ((+ 1) . last) $ fmap tuUpdateId xs

type Handler = TelegramMessage -> IO LB.ByteString

type HandlerMonad = TelegramMessage -> BotMonad IO LB.ByteString

routeMessage :: TelegramMessage -> Maybe HandlerMonad
routeMessage msg =
  case tmText msg of
    Just text -> Just (fromMaybe messageHandler (lookup text commands))
    Nothing -> Nothing

routeCallbackQuery :: TelegramCallbackQuery -> Maybe HandlerMonad
routeCallbackQuery cQ =
  case tcqData cQ of
    Nothing -> Nothing
    Just cqData ->
      Just (fromMaybe (callbackQueryHandler 1) (lookup cqData callbackQueries))

handleUpdate :: TelegramUpdate -> BotMonad IO LB.ByteString
handleUpdate upd = do
  liftIO $ print "Handle update: " >> print upd
  case tuMessage upd of
    Just m -> (fromJust $ routeMessage m) m -- TODO: handle exception
    Nothing ->
      let cQuery = tuCallbackQuery upd
       in case cQuery of
            Nothing -> undefined -- TODO: No handler error
            Just cQ ->
              let handler = fromJust $ routeCallbackQuery cQ -- TODO: handle exception
                  msg = fromJust $ tcqMessage cQ
               in handler msg

sendMessage ::
     Int -> T.Text -> Maybe TelegramInlineKeyboard -> BotMonad IO LB.ByteString
sendMessage chatId text keyboard = do
  token <- asks beToken
  let url = https "api.telegram.org" /: token /: "sendMessage"
  let reqBody = TelegramSendMessageData chatId text
  rsp <-
    case keyboard of
      Nothing ->
        liftIO $ R.post url (TelegramSendMessageData chatId text) mempty
      (Just k) ->
        liftIO $
        R.post
          url
          (TelegramMessageInlineKeyboardData
             chatId
             "Choose echo msg repeat times"
             k)
          mempty
  pure $ responseBody rsp

helpHandler :: HandlerMonad -- (ReaderT BotEnv) (UsersMonad  IO) LB.ByteString
helpHandler (TelegramMessage chat msg) = do
  let cId = tcId chat
  getOrCreateUser cId
  sendMessage cId "Try /repeat command." Nothing -- TODO: вынести текст в конфиг

messageHandler :: HandlerMonad
messageHandler (TelegramMessage chat msg) = do
  let cId = tcId chat
  mUser <- getOrCreateUser cId
  let r = repeats (fromJust mUser) -- TODO : exception
  sendMsgNtimes r cId (fromMaybe "Received empty message" msg)

sendMsgNtimes :: Int -> Int -> T.Text -> BotMonad IO LB.ByteString
sendMsgNtimes 1 cId msg = sendMessage cId msg Nothing
sendMsgNtimes n cId msg = do
  sendMessage cId msg Nothing
  sendMsgNtimes (n - 1) cId msg

repeatHandler :: HandlerMonad
repeatHandler (TelegramMessage chat msg) = do
  let cId = tcId chat
  getOrCreateUser cId
  sendMessage cId msgForSend (Just keyboard)
  where
    msgForSend = "Choose echo msg repeat times"
    keyboard =
      TelegramInlineKeyboard
        [ [ TelegramInlineKeyboardButton "1" "repeat1"
          , TelegramInlineKeyboardButton "2" "repeat2"
          ]
        , [ TelegramInlineKeyboardButton "3" "repeat3"
          , TelegramInlineKeyboardButton "4" "repeat4"
          ]
        , [TelegramInlineKeyboardButton "5" "repeat5"]
        ]

callbackQueryHandler :: Int -> HandlerMonad
callbackQueryHandler n (TelegramMessage chat msg) = do
  let cId = tcId chat
  getOrCreateUser cId
  changeRepeats cId n
  sendMessage cId ("repeats: " <> intToText n) Nothing

commands = [("/help", helpHandler), ("/repeat", repeatHandler)]

callbackQueries =
  [ ("repeat1", callbackQueryHandler 1)
  , ("repeat2", callbackQueryHandler 2)
  , ("repeat3", callbackQueryHandler 3)
  , ("repeat4", callbackQueryHandler 4)
  , ("repeat5", callbackQueryHandler 5)
  ]

telegramEnv :: IO BotEnv
telegramEnv = do
    conf <- loadConfig
    tgToken <- getConf conf "telegram.token"
    pure BotEnv {beToken = tgToken, beChannel = Nothing}
