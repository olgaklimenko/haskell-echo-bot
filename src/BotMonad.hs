{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BotMonad where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString.Lazy as LB
import qualified Data.Configurator.Types as C
import Users (User, Users, UsersMonad)

newtype BotEnv = BotEnv
  { heConfig :: C.Config
  }

newtype BotMonad a = BotMonad
  { runBotMonad :: (ReaderT BotEnv) (UsersMonad IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader BotEnv
             , MonadState Users
             )

type Handler = BotMonad LB.ByteString
