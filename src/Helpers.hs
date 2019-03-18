module Helpers where

import qualified Data.Text as T

intToText :: Int -> T.Text
intToText = T.pack . show

