{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Servant.API

import qualified Data.Text as T

import Types

type EntryAPI =
        Summary "Get entry"
            :> "entry" 
            :> QueryParam "postId" T.Text 
            :> Get '[JSON] Entry
   :<|> Summary "Get entry replies"
            :> "entry" 
            :> "replies" 
            :> QueryParam "entryId[id]" EntryId 
            :> Get '[JSON] [Entry]
   :<|> Summary "Create entry"
            :> "entry" 
            :> Header "Authorization" Token 
            :> QueryParam "content" T.Text 
            :> QueryParam "replyTo" EntryId
            :> Post '[JSON] Entry
   :<|> Summary "Delete entry"
            :> "entry" 
            :> Header "Authorization" Token
            :> Capture "entryId" EntryId 
            :> Delete '[JSON] Entry
        -- | Like entry
   :<|> Summary "Like entry"
            :> "entry" 
            :> "like" 
            :> Header "Authorization" Token
            :> Capture "entryId" EntryId 
            :> Post '[JSON] ()
        -- | Unlike entry
   :<|> Summary "Unlike entry"
            :> "entry" 
            :> "unlike" 
            :> Header "Authorization" Token
            :> Capture "entryId" EntryId 
            :> Post '[JSON] ()

-- | Private Messaging
type MessageAPI =
      Summary "Send message" 
         :> "message" 
         :> Header "Authorization" Token
         :> Capture "userId" UserId
         :> QueryParam "content" T.Text
         :> Post '[JSON] Message
 :<|> Summary "List messages" 
         :> "messages" 
         :> Header "Authorization" Token
         :> Capture "userId" UserId
         :> QueryParam "count" Integer
         :> QueryParam "since" T.Text
         :> QueryParam "until" T.Text
         :> Get '[JSON] [Message]
 :<|> Summary "Get message"
         :> "message" 
         :> Header "Authorization" Token
         :> Capture "messageId" MessageId
         :> Get '[JSON] Message

type TransportAPI =
      Summary "Get vehicles" 
         :> "transport" 
         :> QueryParams "code" T.Text
         :> QueryParam "count" Integer
         :> Get '[JSON] [Vehicle]

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

-- | General information: weather, etc
type WeatherAPI = 
        -- | Get current weather information
        Summary "Get current weather information" :> "weather" :> Get '[JSON] Weather

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

type API = UserAPI 
      :<|> EntryAPI
      :<|> WeatherAPI 
      :<|> NewsAPI 
      :<|> MessageAPI 
      :<|> TransportAPI 
      :<|> MapAPI 

