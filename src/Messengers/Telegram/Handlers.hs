{-# LANGUAGE DeriveGeneric #-}

module Handlers where

import GHC.Generics

data Chat = Chat {
    id :: Int
    , first_name :: String
    , last_name :: String
    , username :: String
} deriving (Generic, Show) 

data Message = Message {
    message_id :: Int
    , chat :: Chat
    , date :: Int
    , text :: String
} deriving (Generic, Show) 

data Update = Update {
    update_id  :: Int
    , message :: Message
    } deriving (Generic, Show)  

data Updates = Updates {
    ok :: Bool
    , result :: [Update]
} deriving (Generic, Show) 


