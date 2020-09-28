{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Message where

import Servant
import Servant.API
import Types

import qualified Data.Text as T


-- | Private Messaging
type MessageAPI =
      Summary "Send message" 
         :> "message" 
         :> "user"
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
