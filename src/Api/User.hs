{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Control.Monad.IO.Class
import Database.Persist
import Database.Persist.Sql
import Data.Time.Calendar
import Data.Time.Clock
import Servant
import Servant.API
import qualified Data.Text as T
import Db

import qualified Types as S

-- | Users
type UserAPI =
      Summary "Get User"
      :> "user" 
      :> Capture "userId" S.UserId 
      :> Get '[JSON] (S.User)
 :<|> Summary "Create User"
      :> "user" 
      :> QueryParam "name" T.Text 
      :> QueryParam "username" T.Text
      :> Post '[JSON] (S.User)


userServer :: Server UserAPI
userServer = getUser :<|> createUser

-- | Get user by ID
getUser :: S.UserId -> Handler (S.User)
getUser i = do
  maybeEntityUser <- runDb (getEntity $ toSqlKey i)
  case maybeEntityUser of
    Just entityUser -> return $ toUser entityUser
    Nothing         -> throwError err404

-- | Create a new user
createUser :: Maybe T.Text -> Maybe T.Text -> Handler (S.User)
createUser (Just name) (Just username) = do
  now <- liftIO getCurrentTime
  entityUser <- runDb $ insertEntity (User name username now)
  return $ toUser entityUser
createUser Nothing _ = throwError err400
createUser _ Nothing = throwError err400

-- | Transform the user database type to the expected API type
toUser :: Entity User -> S.User
toUser (Entity ukey u) = 
  S.User uid name username createdAt
  where
    uid = fromSqlKey ukey
    name = userName u
    username = userUsername u
    createdAt = userCreated_at u