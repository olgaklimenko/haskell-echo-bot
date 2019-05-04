{-# LANGUAGE OverloadedStrings #-}

module Main where

import BotMonad
import Config (loadConfig)
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Configurator as C
import qualified Data.Text as T
import Exceptions
import qualified Messengers.Slack.Api as SlackApi
import qualified Messengers.Telegram.Api as TelegramApi
import Users (UsersMonad(..))

main :: IO ()
main = do
  conf <- loadConfig
  let env = BotEnv {heConfig = conf}
  messenger <- C.lookup conf "common.messenger" :: IO (Maybe T.Text)
  case messenger of
    Nothing -> throw MessengerNotSetException
    Just m -> start m env >> pure ()
  where
    start "Slack" env = runIOBotMonad SlackApi.startPolling env []
    start "Telegram" env = runIOBotMonad TelegramApi.startPolling env []
    start m env = throw $ UnknownMessengerException m
