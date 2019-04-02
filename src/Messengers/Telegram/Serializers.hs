{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Messengers.Telegram.Serializers where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data TelegramChat = TelegramChat
  { tcId :: Int
  , tcFirstName :: Maybe T.Text
  , tcLastName :: Maybe T.Text
  , tcUsername :: Maybe T.Text
  , tcType :: T.Text
  } deriving (Show, Eq, Generic)

instance FromJSON TelegramChat where
  parseJSON (Object v) =
    TelegramChat <$> v .: "id" <*> v .:? "first_name" <*> v .:? "last_name" <*>
    v .:? "username" <*>
    v .: "type"

data TelegramMessage = TelegramMessage
  { tmChat :: TelegramChat
  , tmText :: Maybe T.Text
  } deriving (Show, Eq, Generic)

instance FromJSON TelegramMessage where
  parseJSON (Object v) = TelegramMessage <$> v .: "chat" <*> v .:? "text"

data TelegramCallbackQuery = TelegramCallbackQuery
  { tcqId :: String
  , tcqData :: Maybe String
  , tcqMessage :: Maybe TelegramMessage
  , tcqInlineMessageId :: Maybe String
  } deriving (Show, Eq, Generic)

instance FromJSON TelegramCallbackQuery where
  parseJSON (Object v) =
    TelegramCallbackQuery <$> v .: "id" <*> v .:? "data" <*> v .:? "message" <*> v.:? "inline_message_id"

data TelegramUpdate = TelegramUpdate
  { tuUpdateId :: Int
  , tuMessage :: Maybe TelegramMessage
  , tuCallbackQuery :: Maybe TelegramCallbackQuery
  } deriving (Show, Eq, Generic)

instance FromJSON TelegramUpdate where
  parseJSON (Object v) = TelegramUpdate <$> v .: "update_id" <*> v .:? "message" <*> v .:? "callback_query"

data TelegramUpdateResponse = TelegramUpdateResponse
  { tuResponseOk :: Bool
  , tuResponseResult :: [TelegramUpdate]
  } deriving (Show, Eq, Generic)

instance FromJSON TelegramUpdateResponse where
  parseJSON (Object v) = TelegramUpdateResponse <$> v .: "ok" <*> v .: "result"

data TelegramSendMessageData = TelegramSendMessageData
  { tsmdChatId :: Int
  , tsmdText :: T.Text
  } deriving (Show, Eq, Generic)

instance ToJSON TelegramSendMessageData where
  toJSON TelegramSendMessageData {..} =
    object ["chat_id" .= tsmdChatId, "text" .= tsmdText]

data TelegramInlineKeyboardButton = TelegramInlineKeyboardButton
  { tikbText :: String
  , tikbCallbackData :: String
  } deriving (Show, Eq, Generic)

instance ToJSON TelegramInlineKeyboardButton where
  toJSON (TelegramInlineKeyboardButton btnText btnData) =
    object ["text" .= btnText, "callback_data" .= btnData]

newtype TelegramInlineKeyboard = TelegramInlineKeyboard
  { inlineKeyboardButtons :: [[TelegramInlineKeyboardButton]]
  }

instance ToJSON TelegramInlineKeyboard where
  toJSON (TelegramInlineKeyboard btns) = object ["inline_keyboard" .= btns]

data TelegramMessageInlineKeyboardData = TelegramMessageInlineKeyboardData
  { tmikdChatId :: Int
  , tmikdDataText :: String
  , tmikdDataReplyMarkup :: TelegramInlineKeyboard
  }

instance ToJSON TelegramMessageInlineKeyboardData where
  toJSON TelegramMessageInlineKeyboardData {..} =
    object
      [ "chat_id" .= tmikdChatId
      ,  "text" .= tmikdDataText
      , "reply_markup" .= tmikdDataReplyMarkup
      ]
