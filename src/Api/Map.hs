{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Map where

import Servant
import Servant.API
import Types

import qualified Data.Text as T

type MapAPI = 
       Summary "Get map link"
        :> "maps" 
        :> QueryParam' '[Required, Description "latitude"] "lat" Double 
        :> QueryParam' '[Required, Description "longitude"] "lng" Double 
        :> Get '[JSON] Map
  :<|> Summary "Get map link for a board"
        :> "maps"
        :> "board"
        :> QueryParam' '[Required, Description "board id"] "id" T.Text
        :> Get '[JSON] Map
