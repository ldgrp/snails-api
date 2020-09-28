{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.News where

import Servant
import Servant.API
import Types

import qualified Data.Text as T


type NewsAPI =
      Summary "Get News item"
         :> "news"
         :> Capture "newsId" NewsId 
         :> Get '[JSON] News
 :<|> Summary "Get list of News items"
         :> Description "Results are ordered from newest to oldest."
         :> "news" 
         :> QueryParam "count" Integer 
         :> QueryParam "since" T.Text
         :> QueryParam "until" T.Text
         :> Get '[JSON] [News]
