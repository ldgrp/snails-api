{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Lens
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Time.Clock
import Data.Proxy
import Data.Swagger
import Database.Persist
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Swagger

import qualified Data.Text as T

import qualified Data.ByteString.Lazy.Char8 as BLC

import Api
import Api.User
import Api.Map
import Api.Entry
import Api.Message
import Api.News
import Api.Transport
import Api.Weather
import qualified Db as D

import Database.Persist.Sql

main = do 
    let path = "swagger.json"
    BLC.writeFile path (encodePretty snailsSwagger)
    putStrLn $ "swagger docs generated at " <> path
    D.doMigrations
    populateDb
    run 8081 app

populateDb :: IO()
populateDb = do
  now <- getCurrentTime
  runHandler $ do
    createUser (Just "Hooty") (Just "alexhirsch")
    createUser (Just "Amity Blight") (Just "shutup")
    createUser (Just "Eda Clawthorne") (Just "theowllady")
    createUser (Just "King") (Just "billcipher")
    createUser (Just "Gus") (Just "itsgus")
    createEntry (Just "1") (Just "Hello world!") (Nothing)
    createEntry (Just "2") (Just "Hello world2!") (Just 1)
    createEntry (Just "3") (Just "Hello world3!") (Just 2)
    createEntry (Just "4") (Just "Hello world4!") (Just 1)
    createEntry (Just "4") (Just "Hello world5!") (Just 1)
    createEntry (Just "4") (Just "Hello world6!") (Just 1)
    createEntry (Just "4") (Just "Hello world7!") (Just 1)
    createEntry (Just "4") (Just "Hello world8!") (Just 1)
    createEntry (Just "4") (Just "Hello world9!") (Just 1)
    createEntry (Just "4") (Just "Hello world10!") (Just 1)
    createEntry (Just "4") (Just "Hello world11!") (Just 1)
    createEntry (Just "4") (Just "Hello world12!") (Just 1)
    createEntry (Just "4") (Just "Hello world13!") (Just 1)
    createEntry (Just "4") (Just "Hello world14!") (Just 1)
    createEntry (Just "4") (Just "Hello world15!") (Just 1)
    createEntry (Just "4") (Just "Hello world16!") (Just 1)
    deleteEntry (Just "2") (2)
    likeEntry (Just "1") (1)
    likeEntry (Just "2") (1)
    likeEntry (Just "3") (1)
    unlikeEntry (Just "3") (1)
    sendMessage (Just "1") 2 (Just "Foo 1-2 alpha")
    sendMessage (Just "2") 1 (Just "Bar 2-1 beta")
    sendMessage (Just "1") 2 (Just "Foo 1-2 kappa")
    sendMessage (Just "3") 2 (Just "Foo 3-2")
  return ()

snailsSwagger :: Swagger
snailsSwagger = toSwagger (Proxy :: Proxy API)
    & info.title .~ "Snails API"
    & info.version .~ "0.2"
    & info.description ?~ "Description."
    & applyTagsFor (subOperations (Proxy :: Proxy UserAPI) (Proxy :: Proxy API)) ["User"]
    & applyTagsFor (subOperations (Proxy :: Proxy EntryAPI) (Proxy :: Proxy API)) ["Entry"]
    & applyTagsFor (subOperations (Proxy :: Proxy WeatherAPI) (Proxy :: Proxy API)) ["Weather"]
    & applyTagsFor (subOperations (Proxy :: Proxy NewsAPI) (Proxy :: Proxy API)) ["News"]
    & applyTagsFor (subOperations (Proxy :: Proxy TransportAPI) (Proxy :: Proxy API)) ["Transport"]
    & applyTagsFor (subOperations (Proxy :: Proxy MessageAPI) (Proxy :: Proxy API)) ["Message"]
    & applyTagsFor (subOperations (Proxy :: Proxy MapAPI) (Proxy :: Proxy API)) ["Map"]