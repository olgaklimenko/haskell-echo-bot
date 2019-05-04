{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BotMonad where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Class
import qualified Data.ByteString.Lazy as LB
import qualified Data.Configurator.Types as C
import qualified Data.Text as T
import Users

data BotEnv = BotEnv
  { beToken :: T.Text
  , beChannel :: Maybe T.Text
  }

newtype BotMonad m a = BotMonad
  { runBotMonad :: (ReaderT BotEnv) (UsersMonad m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader BotEnv
             , MonadState Users
             )

type Handler = BotMonad IO LB.ByteString

runIOBotMonad :: BotMonad IO a -> BotEnv -> Users -> IO (a, Users)
runIOBotMonad (BotMonad m) env state = runStateT (runReaderT m env) []
