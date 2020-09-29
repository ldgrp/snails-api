{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Weather where

import Control.Monad.IO.Class
import Data.Time.Clock
import Servant
import Types

import qualified Data.Text as T

type WeatherAPI = 
        -- | Get current weather information
        Summary "Get current weather information" :> "weather" :> Get '[JSON] Weather

weatherServer :: Server WeatherAPI
weatherServer = getWeather

getWeather :: Handler Weather
getWeather = do
  now <- liftIO getCurrentTime
  return $ Weather now (Just 25) (Just 14) (Just 28) (Just 0.8) (Just 0.2) (Just Sunny) (Just 7)