module Users where

import Control.Monad.State
import Data.List

data User = User
  { chatId :: String
  , repeats :: Integer
  } deriving Show

instance Eq User where
  u1 == u2 = chatId u1 == chatId u2

type UsersMonad = State [(String, User)]

addUser :: String -> UsersMonad ()
addUser chatId = do
  users <- get
  put ((chatId, User chatId 1) : users)

getUser :: String -> UsersMonad (Maybe User)
getUser chatId = gets $ lookup chatId
  
changeRepeats :: String -> Integer -> UsersMonad ()
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


{--
проверка
import Users
import Control.Monad.State
(val, s) = runState (addUser "123") []
(val1, s1) = runState (addUser "1234") s

(val2, s2) = runState (changeRepeats "1234" 3) s1

evalState (getUser "1234") s2
--}