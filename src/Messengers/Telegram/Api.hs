{-# LANGUAGE OverloadedStrings #-}

module Messengers.Telegram.Api 
    (getUpdates
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Default.Class
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B

getUpdates :: Text -> IO ()
-- add offset and timeout parameters (offset is id gte then last update_id)
-- timeout for long polling
getUpdates token = runReq def $ do
    response <- req GET (https "api.telegram.org" /: token /: "getUpdates") NoReqBody jsonResponse mempty
    liftIO $ print (responseBody response :: Value)
