{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.News where

import Servant
import Servant.API

import Data.Maybe
import Data.Time.Clock
import Database.Persist
import Database.Persist.Sql
import Db
import qualified Data.Text as T
import qualified Types as S

type NewsAPI =
      Summary "Get News item"
         :> "news"
         :> Capture "newsId" S.NewsId 
         :> Get '[JSON] S.News
 :<|> Summary "Get list of News items"
         :> Description "Results are ordered from newest to oldest."
         :> "news" 
         :> QueryParam "count" Int
         :> QueryParam "before" S.NewsId
         :> QueryParam "after" S.NewsId
         :> Get '[JSON] [S.News]

newsServer :: Server NewsAPI
newsServer = getNews :<|> getEntries

-- | Get news item by ID
getNews :: S.NewsId -> Handler (S.News)
getNews i = do
  maybeNews <- runDb (getEntity $ toSqlKey i)
  case maybeNews of
    Just news  -> return $ toNews news
    Nothing    -> throwError err404

-- | Get `count` news items appearing before and/or after specific news items.
getEntries :: Maybe Int -> Maybe S.NewsId -> Maybe S.NewsId -> Handler [S.News]
getEntries maybeCount maybeBeforeId maybeAfterId = do
  beforeFilter <- before (toSqlKey <$> maybeBeforeId)
  afterFilter <- after (toSqlKey <$> maybeAfterId)
  entries <- runDb $ selectList (catMaybes [beforeFilter, afterFilter]) [LimitTo count]
  return $ fmap toNews entries
  where 
    count :: Int
    count = maybe 3 id maybeCount
    before :: Maybe NewsId -> Handler (Maybe (Filter News))
    before (Just nid) = fmap (fmap (NewsCreated_at <.)) (getTime nid)
    before Nothing = return Nothing
    after :: Maybe NewsId -> Handler (Maybe (Filter News))
    after (Just nid) = fmap (fmap (NewsCreated_at >.)) (getTime nid)
    after Nothing = return Nothing
    getTime :: NewsId -> Handler (Maybe UTCTime)
    getTime newsId = runDb $ fmap (fmap newsCreated_at) (get newsId)


toNews :: Entity News -> S.News
toNews (Entity nkey n) = 
  S.News nid title content image_url url created_at 
  where 
    nid = fromSqlKey nkey
    title = newsTitle n 
    content = newsContent n 
    image_url = newsImage_url n 
    url = newsUrl n 
    created_at = newsCreated_at n 