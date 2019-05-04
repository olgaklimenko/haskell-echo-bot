{-# LANGUAGE OverloadedStrings #-}
module Main where

import BotMonad
import Config (loadConfig, getConf)
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Configurator as C
import qualified Data.Text as T
import Exceptions
import qualified Messengers.Slack.Api as SlackApi
import qualified Messengers.Telegram.Api as TelegramApi
import Users (UsersMonad(..))
import Logger

main :: IO ()
main = do
  conf <- loadConfig
  messenger <- C.lookup conf "common.messenger" :: IO (Maybe T.Text)
  case messenger of
    Nothing -> throw MessengerNotSetException
    Just "Slack" -> do
      env <- SlackApi.slackEnv
      runIOBotMonad SlackApi.startPolling env [] >> pure ()
    Just "Telegram" -> do
      env <- TelegramApi.telegramEnv
      runIOBotMonad TelegramApi.startPolling env [] >> pure ()
    Just m -> throw $ UnknownMessengerException m

