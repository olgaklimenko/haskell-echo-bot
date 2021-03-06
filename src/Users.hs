{-# LANGUAGE FlexibleContexts #-}

module Users where

import Control.Monad.State
import Data.List
import Data.Maybe (fromJust)

data User = User
  { uChatId :: Int
  , repeats :: Int
  } deriving (Show)

instance Eq User where
  u1 == u2 = uChatId u1 == uChatId u2

type Users = [(Int, User)]

type UsersMonad m = StateT Users m

createUser :: (Monad m, MonadState Users m) => Int -> m (Maybe User)
createUser chatId = do
  users <- get
  put ((chatId, User chatId 1) : users)
  gets $ lookup chatId

getUser :: (Monad m, MonadState Users m) => Int -> m (Maybe User)
getUser chatId = gets $ lookup chatId

changeRepeats :: (Monad m, MonadState Users m) => Int -> Int -> m ()
changeRepeats chatId repeats = do
  users <- get
  let user = lookup chatId users
  case user of
    Nothing -> put users
    Just u ->
      let l1 = takeWhile (/= (chatId, u)) users
          l2 = tail $ dropWhile (/= (chatId, u)) users
          newState = l1 ++ l2
       in put ((chatId, User chatId repeats) : newState)

getOrCreateUser :: (Monad m, MonadState Users m) => Int -> m (Maybe User)
getOrCreateUser chatId = do
  mUser <- getUser chatId
  case mUser of
    Just u -> pure mUser
    Nothing -> createUser chatId
{--
проверка
import Users
import Control.Monad.State
(val, s) = runState (addUser "123") []
(val1, s1) = runState (addUser "1234") s

(val2, s2) = runState (changeRepeats "1234" 3) s1

evalState (getUser "1234") s2
--}
