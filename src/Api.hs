{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api 
(
  UserAPI,
  EntryAPI,
  WeatherAPI,
  NewsAPI,
  MessageAPI,
  TransportAPI,
  MapAPI,
  API
)where

import Servant.API

import Api.User
import Api.Entry
import Api.News
import Api.Message
import Api.Transport
import Api.Map
import Api.Weather

import qualified Data.Text as T

import Types
-- | General information: weather, etc

type API = UserAPI 
      :<|> EntryAPI
      :<|> WeatherAPI 
      :<|> NewsAPI 
      :<|> MessageAPI 
      :<|> TransportAPI 
      :<|> MapAPI 
