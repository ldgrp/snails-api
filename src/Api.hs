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
  API,
  app
)where

import Api.User
import Api.Entry
import Api.News
import Api.Message
import Api.Transport
import Api.Map
import Api.Weather

import Servant


import Types

type API = UserAPI 
      :<|> EntryAPI
      :<|> MessageAPI 
      :<|> TransportAPI 
      :<|> NewsAPI 
      :<|> WeatherAPI 
      :<|> MapAPI 

app :: Application
app = serve (Proxy :: Proxy API)
    (  userServer 
  :<|> entryServer 
  :<|> messageServer 
  :<|> transportServer 
  :<|> newsServer
  :<|> weatherServer
  :<|> mapServer
    )