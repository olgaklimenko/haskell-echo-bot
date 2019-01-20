
module Main where
import qualified Messengers.Telegram.Api as TelegramApi

main :: IO ()
main = TelegramApi.startPolling
