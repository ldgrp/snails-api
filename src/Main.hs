{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Proxy
import Data.Swagger
import Servant.Swagger

import qualified Data.ByteString.Lazy.Char8 as BLC

import API
import Types

main = do 
    let path = "swagger.json"
    putStrLn $ "swagger docs generated at " <> path
    BLC.writeFile path (encodePretty snailsSwagger)

snailsSwagger :: Swagger
snailsSwagger = toSwagger (Proxy :: Proxy API)
    & info.title .~ "Snails API"
    & info.version .~ "0.1"
    & info.description ?~ "Description."
    & applyTagsFor (subOperations (Proxy :: Proxy UserAPI) (Proxy :: Proxy API)) ["User"]
    & applyTagsFor (subOperations (Proxy :: Proxy EntryAPI) (Proxy :: Proxy API)) ["Entry"]
    & applyTagsFor (subOperations (Proxy :: Proxy WeatherAPI) (Proxy :: Proxy API)) ["Weather"]
    & applyTagsFor (subOperations (Proxy :: Proxy NewsAPI) (Proxy :: Proxy API)) ["News"]
    & applyTagsFor (subOperations (Proxy :: Proxy TransportAPI) (Proxy :: Proxy API)) ["Transport"]
    & applyTagsFor (subOperations (Proxy :: Proxy MessageAPI) (Proxy :: Proxy API)) ["Message"]
    & applyTagsFor (subOperations (Proxy :: Proxy MapAPI) (Proxy :: Proxy API)) ["Map"]
