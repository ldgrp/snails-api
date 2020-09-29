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


app :: Application
app = serve (Proxy :: Proxy (UserAPI :<|> EntryAPI)) (userServer :<|> entryServer)

populateDb :: IO()
populateDb = do
  now <- getCurrentTime
  runHandler $ createUser (Just "Hooty") (Just "alexhirsch")
  runHandler $ createUser (Just "Amity Blight") (Just "shutup")
  runHandler $ createUser (Just "Eda Clawthorne") (Just "theowllady")
  runHandler $ createUser (Just "King") (Just "billcipher")
  runHandler $ createUser (Just "Gus") (Just "itsgus")
  runHandler $ createEntry (Just "1") (Just "Hello world!") (Nothing)
  runHandler $ createEntry (Just "2") (Just "Hello world2!") (Just 1)
  runHandler $ createEntry (Just "3") (Just "Hello world3!") (Just 2)
  runHandler $ createEntry (Just "4") (Just "Hello world4!") (Just 1)
  runHandler $ createEntry (Just "4") (Just "Hello world5!") (Just 1)
  runHandler $ createEntry (Just "4") (Just "Hello world6!") (Just 1)
  runHandler $ createEntry (Just "4") (Just "Hello world7!") (Just 1)
  runHandler $ createEntry (Just "4") (Just "Hello world8!") (Just 1)
  runHandler $ createEntry (Just "4") (Just "Hello world9!") (Just 1)
  runHandler $ createEntry (Just "4") (Just "Hello world10!") (Just 1)
  runHandler $ createEntry (Just "4") (Just "Hello world11!") (Just 1)
  runHandler $ createEntry (Just "4") (Just "Hello world12!") (Just 1)
  runHandler $ createEntry (Just "4") (Just "Hello world13!") (Just 1)
  runHandler $ createEntry (Just "4") (Just "Hello world14!") (Just 1)
  runHandler $ createEntry (Just "4") (Just "Hello world15!") (Just 1)
  runHandler $ createEntry (Just "4") (Just "Hello world16!") (Just 1)
  runHandler $ deleteEntry (Just "2") (2)
  runHandler $ likeEntry (Just "1") (1)
  runHandler $ likeEntry (Just "2") (1)
  runHandler $ likeEntry (Just "3") (1)
  runHandler $ unlikeEntry (Just "3") (1)
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