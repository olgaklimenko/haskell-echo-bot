
module Main where
import qualified Messengers.Telegram.Api as TelegramApi
import Control.Monad.State
import Users (UsersMonad(..))

main :: IO ()
main = runStateT TelegramApi.startPolling [] >> return ()
