{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Messengers.Slack.Serializers where

import Data.Aeson
import qualified Data.Text as T

data SlackMessage = SlackMessage
  { smType :: T.Text
  , smUser :: T.Text
  , smText :: T.Text
  , smTimestamp :: T.Text 
  } deriving (Show)

instance FromJSON SlackMessage where
  parseJSON (Object v) =
    SlackMessage <$> v .: "type" <*> v .: "user" <*> v .: "text" <*> v .: "ts"

data SlackUpdateResponse = UpdateResponseSlack
  { surOk :: Bool
  , surMessages :: [SlackMessage]
  } deriving (Show)

instance FromJSON SlackUpdateResponse where
  parseJSON (Object v) = UpdateResponseSlack <$> v .: "ok" <*> v .: "messages"

data SlackSendMessageData = SlackSendMessageData {
  ssmdChannel :: T.Text,
  ssmdText :: T.Text
}

instance ToJSON SlackSendMessageData where
  toJSON SlackSendMessageData {..} =
    object ["channel" .= ssmdChannel, "text" .= ssmdText]

--   {
--             "type": "message",
--             "user": "U012AB3CDE",
--             "text": "I find you punny and would like to smell your nose letter",
--             "ts": "1512085950.000216"
--         },
