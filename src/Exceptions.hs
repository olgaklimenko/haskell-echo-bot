module Exceptions where

import Control.Exception
import qualified Data.Text as T

data BotConfigException
  = MessengerNotSetException
  | UnknownMessengerException T.Text
  | NoTokenException T.Text
  | NoChannelException T.Text
  deriving (Show)

instance Exception BotConfigException
