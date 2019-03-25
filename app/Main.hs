
module Main where
import qualified Messengers.Telegram.Api as TelegramApi
import qualified Messengers.Slack.Api as SlackApi

import Control.Monad.State
import Users (UsersMonad(..))

main :: IO ()
-- main = runStateT TelegramApi.startPolling [] >> return ()
main = SlackApi.startPolling >> return ()
