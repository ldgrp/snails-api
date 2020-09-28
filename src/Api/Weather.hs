{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Weather where

import Servant
import Servant.API
import Types

import qualified Data.Text as T

type WeatherAPI = 
        -- | Get current weather information
        Summary "Get current weather information" :> "weather" :> Get '[JSON] Weather