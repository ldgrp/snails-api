{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Message where

import Api.User
import Control.Monad.IO.Class
import Database.Persist
import Database.Persist.Sql
import Data.Time.Clock
import Data.Maybe
import Data.Int (Int64)
import Db
import Servant

import qualified Data.Text as T
import qualified Types as S


-- | Private Messaging
type MessageAPI =
      Summary "Send message" 
         :> "message" 
         :> "user"
         :> Header "Authorization" S.Token
         :> Capture "userId" S.UserId
         :> QueryParam "content" T.Text
         :> Post '[JSON] S.Message
 :<|> Summary "List messages" 
         :> "messages" 
         :> Header "Authorization" S.Token
         :> Capture "userId" S.UserId
         :> QueryParam "count" Int
         :> QueryParam "before" S.MessageId
         :> QueryParam "after" S.MessageId
         :> Get '[JSON] [S.Message]
 :<|> Summary "Get message"
         :> "message" 
         :> Header "Authorization" S.Token
         :> Capture "messageId" S.MessageId
         :> Get '[JSON] S.Message

messageServer :: Server MessageAPI
messageServer =
       sendMessage
  :<|> getMessages
  :<|> getMessage

-- | Send a message to a recipient
-- | Requests must have a valid token and must contain a message
sendMessage :: Maybe S.Token -> S.UserId -> Maybe T.Text -> Handler S.Message
sendMessage (Just token) to (Just content) = do
  now <- liftIO getCurrentTime
  let sender = toSqlKey $ (read $ T.unpack token :: Int64)
  let recipient = toSqlKey to
  let message = Message content sender recipient now
  entityMessage <- runDb $ insertEntity message
  toMessage entityMessage
sendMessage Nothing _ _ = throwError err401
sendMessage _ _ _ = throwError err400

-- | Get `count` messages appearing before and/or after specific messages.
-- | Requests must have a valid token
getMessages :: Maybe S.Token 
            -> S.UserId
            -> Maybe Int
            -> Maybe S.MessageId 
            -> Maybe S.MessageId 
            -> Handler [S.Message]
getMessages (Just token) to (maybeCount) (maybeBeforeId) (maybeAfterId) = do
  let sender = toSqlKey $ (read $ T.unpack token :: Int64)
  let recipient = toSqlKey to
  beforeFilter <- before (toSqlKey <$> maybeBeforeId)
  afterFilter <- after (toSqlKey <$> maybeAfterId)
  liftIO $ print maybeBeforeId
  liftIO $ print maybeAfterId
  messages <- runDb $ selectList (
    (
      [MessageRecipient ==. recipient, MessageSender ==. sender] ||.
      [MessageRecipient ==. sender, MessageSender ==. recipient]
    ) ++ catMaybes [beforeFilter, afterFilter]) 
    [Desc MessageCreated_at, LimitTo count]
  mapM toMessage messages
  where 
    count :: Int
    count = maybe 5 id maybeCount
    before :: Maybe MessageId -> Handler (Maybe (Filter Message))
    before (Just mid) = fmap (fmap (MessageCreated_at <.)) (getTime mid)
    before Nothing = return Nothing
    after :: Maybe MessageId -> Handler (Maybe (Filter Message))
    after (Just mid) = fmap (fmap (MessageCreated_at >.)) (getTime mid)
    after Nothing = return Nothing
    getTime :: MessageId -> Handler (Maybe UTCTime)
    getTime mid = runDb $ fmap (fmap messageCreated_at) (get mid)
getMessages Nothing _ _ _ _ = throwError err401

-- | Get message by Id
getMessage :: Maybe S.Token -> S.MessageId -> Handler S.Message
getMessage (Just token) i = do
  let user = toSqlKey $ (read $ T.unpack token :: Int64)
  let filter = ([MessageRecipient ==. user] ||. [MessageSender ==. user]) ++ [MessageId ==. toSqlKey i]
  maybeMessage <- runDb $ selectFirst filter []
  case maybeMessage of
    Just message -> toMessage message
    Nothing -> throwError err404

-- | Transform the message database type to the expected API type
toMessage :: Entity Message -> Handler S.Message
toMessage (Entity mkey m) = runDb $ do
  from <- getJustEntity (messageSender m)
  to <- getJustEntity (messageRecipient m)
  return $ S.Message (toUser from) (toUser to) content createdAt mid
  where
    mid = fromSqlKey mkey
    createdAt = messageCreated_at m
    content = messageContent m