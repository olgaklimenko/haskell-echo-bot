{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Messengers.Telegram.Parsers where

import           Data.Aeson
import qualified Data.Text                     as T
import           GHC.Generics

data Chat = Chat {
    chatId :: Int,
    chatFirstName :: Maybe T.Text,
    chatLastName :: Maybe T.Text,
    chatUsername :: Maybe T.Text,
    chatType :: T.Text
} deriving (Show, Eq, Generic)

instance FromJSON Chat where
    parseJSON (Object v) = Chat
        <$> v .: "id"
        <*> v .: "first_name"
        <*> v .: "last_name"
        <*> v .: "username"
        <*> v .: "type"

data Message = Message {
    messageChat :: Chat,
    messageText :: Maybe T.Text
} deriving (Show, Eq, Generic)

instance FromJSON Message where
    parseJSON (Object v) = Message
        <$> v .: "chat"
        <*> v .: "text"

data Update = Update {
    updateUpdateId :: Int,
    updateMessage :: Maybe Message
} deriving (Show, Eq, Generic)

instance FromJSON Update where
    parseJSON (Object v) = Update
        <$> v .: "update_id"
        <*> v .: "message"

data UpdateResponse = UpdateResponse {
    updateResponseOk :: Bool,
    updateResponseResult :: [Update]
} deriving (Show, Eq, Generic)

instance FromJSON UpdateResponse where
    parseJSON (Object v) = UpdateResponse
        <$> v .: "ok"
        <*> v .: "result"

data SendMessage = SendMessage {
    sendMessageChatId :: Int,
    sendMessageText :: T.Text
    } deriving (Show, Eq, Generic)

-- instance FromJSON SendMessage where
--     parseJSON (Object v) = SendMessage
--         <$> v .: "chat_id"
--         <*> v .: "text"

instance ToJSON SendMessage where
toJSON (SendMessage chatId text) = object ["chat_id" .= chatId, "text" .= text]