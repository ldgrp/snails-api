{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Entry where

import Servant
import Servant.API
import Types

import qualified Data.Text as T

type EntryAPI =
        Summary "Get entry"
            :> "entry" 
            :> Capture "entryId" EntryId
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
   :<|> Summary "Like entry"
            :> "entry" 
            :> "like" 
            :> Header "Authorization" Token
            :> Capture "entryId" EntryId 
            :> Post '[JSON] ()
   :<|> Summary "Unlike entry"
            :> "entry" 
            :> "unlike" 
            :> Header "Authorization" Token
            :> Capture "entryId" EntryId 
            :> Post '[JSON] ()
   :<|> Summary "Get entries"
            :> "entries"
            :> QueryParam "count" Int
            :> QueryParam "before[id]" EntryId
            :> QueryParam "after[id]" EntryId
            :> Get '[JSON] [Entry]

