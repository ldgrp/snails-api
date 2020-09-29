{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Entry where

import Control.Monad.IO.Class
import Database.Persist
import Database.Persist.Sql
import Data.Int (Int64)
import Data.Maybe
import Data.Time.Clock
import qualified Data.Map.Strict as M
import Servant
import Servant.API

import Db

import qualified Data.Text as T
import qualified Types as S
import Api.User

type Token = T.Text

type EntryAPI =
        Summary "Get entry"
            :> "entry" 
            :> Capture "entryId" S.EntryId
            :> Get '[JSON] (S.Entry)
   :<|> Summary "Get entry replies"
            :> "entry" 
            :> "replies" 
            :> QueryParam "entryId[id]" S.EntryId 
            :> Get '[JSON] [S.Entry]
   :<|> Summary "Create entry"
            :> "entry" 
            :> Header "Authorization" S.Token 
            :> QueryParam "content" T.Text 
            :> QueryParam "replyTo" S.EntryId
            :> Post '[JSON] (S.Entry)
   :<|> Summary "Delete entry"
            :> "entry" 
            :> Header "Authorization" S.Token
            :> Capture "entryId" S.EntryId 
            :> Delete '[JSON] ()
   :<|> Summary "Like entry"
            :> "entry" 
            :> "like" 
            :> Header "Authorization" S.Token
            :> Capture "entryId" S.EntryId 
            :> Post '[JSON] (S.Entry)
   :<|> Summary "Unlike entry"
            :> "entry" 
            :> "unlike" 
            :> Header "Authorization" S.Token
            :> Capture "entryId" S.EntryId 
            :> Post '[JSON] (S.Entry)
   :<|> Summary "Get entries"
            :> "entries"
            :> QueryParam "count" Int
            :> QueryParam "before[id]" S.EntryId
            :> QueryParam "after[id]" S.EntryId
            :> Get '[JSON] [S.Entry]

entryServer :: Server EntryAPI
entryServer = 
       getEntry 
  :<|> getEntryReplies
  :<|> createEntry
  :<|> deleteEntry
  :<|> likeEntry
  :<|> unlikeEntry
  :<|> getEntries

-- | Get entry by ID
getEntry :: S.EntryId -> Handler (S.Entry)
getEntry i = do
  maybeEntry <- runDb (getEntity $ toSqlKey i)
  case maybeEntry of
    Just entry -> toEntry entry
    Nothing    -> throwError err404

-- | Get a list of entries which are replies to an entry
getEntryReplies :: Maybe S.EntryId -> Handler [S.Entry]
getEntryReplies (Just i) = do
  entries <- runDb $ selectList [EntryReply_to ==. Just (toSqlKey i)] []
  mapM toEntry entries

-- | Create a new entry
createEntry :: Maybe S.Token -> Maybe T.Text -> Maybe S.EntryId -> Handler (S.Entry)
createEntry (Just token) (Just content) (replyTo) = do
  now <- liftIO getCurrentTime
  let author = toSqlKey $ (read $ T.unpack token :: Int64)
  let entry = Entry content author now (toSqlKey <$> replyTo) False
  entityEntry <- runDb $ insertEntity entry
  toEntry entityEntry
createEntry Nothing _ _ = throwError err401
createEntry _ Nothing _ = throwError err404

-- | Delete an entry
deleteEntry :: Maybe S.Token -> S.EntryId -> Handler ()
deleteEntry (Just token) eid = do
  let author = toSqlKey $ (read $ T.unpack token :: Int64)
  runDb $ updateWhere [EntryId ==. toSqlKey eid, EntryAuthor ==. author] [EntryContent =. "Deleted", EntryIs_deleted =. True]
deleteEntry Nothing _ = throwError err401

-- | Like an entry
likeEntry :: Maybe S.Token -> S.EntryId -> Handler (S.Entry)
likeEntry (Just token) eid = do
  let author = toSqlKey $ (read $ T.unpack token :: Int64)
  maybeEntityEntry <- runDb $ do
    insert (EntryLikes author (toSqlKey eid))
    getEntity (toSqlKey eid)
  case maybeEntityEntry of
    Just entityEntry  -> toEntry entityEntry
    Nothing -> throwError err503
likeEntry Nothing _ = throwError err401

-- | Unlike an entry
unlikeEntry :: Maybe S.Token -> S.EntryId -> Handler (S.Entry)
unlikeEntry (Just token) eid = do
  let author = toSqlKey $ (read $ T.unpack token :: Int64)
  maybeEntityEntry <- runDb $ do
    deleteWhere [EntryLikesEntryId ==. toSqlKey eid, EntryLikesUserId ==. author]
    insert (EntryLikes author (toSqlKey eid))
    getEntity (toSqlKey eid)
  case maybeEntityEntry of
    Just entityEntry  -> toEntry entityEntry
    Nothing -> throwError err503
unlikeEntry Nothing _ = throwError err401

-- | Get `count` entries appearing before and/or after specific entries.
getEntries :: Maybe Int -> Maybe S.EntryId -> Maybe S.EntryId -> Handler [S.Entry]
getEntries maybeCount maybeBeforeId maybeAfterId = do
  beforeFilter <- before (toSqlKey <$> maybeBeforeId)
  afterFilter <- after (toSqlKey <$> maybeAfterId)
  entries <- runDb $ selectList (catMaybes [beforeFilter, afterFilter]) [LimitTo count]
  mapM toEntry entries
  where 
    count :: Int
    count = maybe 3 id maybeCount
    before :: Maybe EntryId -> Handler (Maybe (Filter Entry))
    before (Just eid) = fmap (fmap (EntryCreated_at <.)) (getTime eid)
    before Nothing = return Nothing
    after :: Maybe EntryId -> Handler (Maybe (Filter Entry))
    after (Just eid) = fmap (fmap (EntryCreated_at >.)) (getTime eid)
    after Nothing = return Nothing
    getTime :: EntryId -> Handler (Maybe UTCTime)
    getTime entryId = runDb $ fmap (fmap entryCreated_at) (get entryId)

-- | Transform the entry database type to the expected API type
-- | TODO: use joins!
toEntry :: Entity Entry -> Handler S.Entry
toEntry (Entity ekey e) = runDb $ do
  let eid       = fromSqlKey ekey
      createdAt = entryCreated_at e
      content   = entryContent e
      replyTo   = fromSqlKey <$> (entryReply_to e)

  maybeEntityAuthor <- getEntity (entryAuthor e)
  author <- case toUser <$> maybeEntityAuthor of
    Just author -> return author
    Nothing -> error "No author"

  entryLikes <- selectList [EntryLikesEntryId ==. ekey] []
  mapUsers <- getMany $ entryLikesUserId . entityVal <$> entryLikes
  let users = uncurry Entity <$> M.toList mapUsers
  let likedBy = toUser <$> users

  replies <- selectKeysList [EntryReply_to ==. Just ekey] []
  return $ S.Entry eid createdAt content author replyTo likedBy (fromSqlKey <$> replies)