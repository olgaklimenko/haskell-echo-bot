{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BotMonad where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Class
import qualified Data.ByteString.Lazy as LB
import qualified Data.Configurator.Types as C
import Users (User, Users, UsersMonad)

newtype BotEnv = BotEnv
  { heConfig :: C.Config
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

-- instance MonadTrans BotMonad where
  --lift :: Monad m => m a -> t m a 
  -- ReaderT BotEnv (UsersMonad m) (ReaderT r0 m0 a0)
  -- Monad m => m a -> BotMonad m a
  -- lift =  (ReaderT BotEnv) 

type Handler = BotMonad IO LB.ByteString
