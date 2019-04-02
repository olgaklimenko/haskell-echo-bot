
module Main where
import qualified Messengers.Telegram.Api as TelegramApi
import qualified Messengers.Slack.Api as SlackApi
import BotMonad
import Control.Monad.State
import Control.Monad.Reader
import Users (UsersMonad(..))
import Config (loadConfig)

main :: IO ()
-- main = runStateT TelegramApi.startPolling [] >> return ()
main = SlackApi.startPolling >> return ()
-- main = runReaderT conf $ runStateT TelegramApi.startPolling [] >> return ()
--     where conf = pure loadConfig -- TODO: use it
-- main = runBotMonad 