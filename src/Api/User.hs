{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Servant
import Servant.API
import qualified Data.Text as T

import Types

-- | Users
type UserAPI =
      Summary "Get User"
      :> "user" 
      :> Capture "userId" UserId 
      :> Get '[JSON] User
 :<|> Summary "Create User"
      :> "user" 
      :> QueryParam "name" T.Text 
      :> QueryParam "username" T.Text
      :> Post '[JSON] User
