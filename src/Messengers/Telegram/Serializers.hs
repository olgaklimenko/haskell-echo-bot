{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Messengers.Telegram.Serializers where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data Chat = Chat
  { chatId :: Int
  , chatFirstName :: Maybe T.Text
  , chatLastName :: Maybe T.Text
  , chatUsername :: Maybe T.Text
  , chatType :: T.Text
  } deriving (Show, Eq, Generic)

instance FromJSON Chat where
  parseJSON (Object v) =
    Chat <$> v .: "id" <*> v .:? "first_name" <*> v .:? "last_name" <*>
    v .:? "username" <*>
    v .: "type"

data Message = Message
  { messageChat :: Chat
  , messageText :: Maybe T.Text
  } deriving (Show, Eq, Generic)

instance FromJSON Message where
  parseJSON (Object v) = Message <$> v .: "chat" <*> v .:? "text"

data CallbackQuery = CallbackQuery
  { callbackQueryId :: String
  , callbackQueryData :: Maybe String
  , callbackQueryMessage :: Maybe Message
  } deriving (Show, Eq, Generic)

instance FromJSON CallbackQuery where
  parseJSON (Object v) =
    CallbackQuery <$> v .: "id" <*> v .:? "data" <*> v .:? "message"

data Update = Update
  { updateUpdateId :: Int
  , updateMessage :: Maybe Message
  } deriving (Show, Eq, Generic)

instance FromJSON Update where
  parseJSON (Object v) = Update <$> v .: "update_id" <*> v .:? "message"

data UpdateResponse = UpdateResponse
  { updateResponseOk :: Bool
  , updateResponseResult :: [Update]
  } deriving (Show, Eq, Generic)

instance FromJSON UpdateResponse where
  parseJSON (Object v) = UpdateResponse <$> v .: "ok" <*> v .: "result"

data SendMessageData = SendMessageData
  { sendMessageChatId :: Int
  , sendMessageText :: T.Text
  } deriving (Show, Eq, Generic)

instance ToJSON SendMessageData

toJSON SendMessageData {..} =
  object ["chat_id" .= sendMessageChatId, "text" .= sendMessageText]

data InlineKeyboardButton = InlineKeyboardButton
  { inlineKeyboardButtonText :: String
  , inlineKeyboardButtonCallbackData :: String
  } deriving (Show, Eq, Generic)

instance ToJSON InlineKeyboardButton where
  toJSON (InlineKeyboardButton btnText btnData) =
    object ["text" .= btnText, "callback_data" .= btnData]

newtype InlineKeyboard = InlineKeyboard
  { inlineKeyboardButtons :: [[InlineKeyboardButton]]
  }

instance ToJSON InlineKeyboard where
  toJSON (InlineKeyboard btns) = object ["inline_keyboard" .= btns]

data SendMessageWithInlineKeyboardData = SendMessageWithInlineKeyboardData
  { sendMessageWithInlineKeyboardDataText :: String
  , sendMessageWithInlineKeyboardDataReplyMarkup :: InlineKeyboard
  }

instance ToJSON SendMessageWithInlineKeyboardData where
  toJSON SendMessageWithInlineKeyboardData {..} =
    object
      [ "text" .= sendMessageWithInlineKeyboardDataText
      , "reply_markup" .= sendMessageWithInlineKeyboardDataReplyMarkup
      ]
