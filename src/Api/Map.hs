{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Map where

import Database.Persist
import Database.Persist.Sql
import Servant

import Db
import qualified Types as S
import qualified Data.Text as T
import qualified Data.Text.Read as TR

type MapAPI = 
       Summary "Get map link"
        :> "maps" 
        :> QueryParam' '[Required, Description "latitude"] "lat" T.Text 
        :> QueryParam' '[Required, Description "longitude"] "lng" T.Text 
        :> Get '[JSON] S.Map
  :<|> Summary "Get map link for a board"
        :> "maps"
        :> "board"
        :> QueryParam' '[Required, Description "board id"] "id" S.BoardId
        :> Get '[JSON] S.Map

mapServer :: Server MapAPI
mapServer = getLink :<|> getBoard

-- | Get map link
getLink :: T.Text -> T.Text -> Handler S.Map
getLink = ((return . S.Map) .) . mkMapUrl

-- | Get board
getBoard :: S.BoardId -> Handler S.Map
getBoard boardId = do
  maybeBoard <- runDb (get $ toSqlKey boardId)
  case maybeBoard of
    Just board -> 
      let lat = T.pack $ show $ boardLatitude board
          long = T.pack $ show $ boardLongitude board
      in return $ S.Map $ mkMapUrl lat long
    Nothing -> throwError err404

mkMapUrl :: T.Text -> T.Text -> T.Text
mkMapUrl lat long = T.concat [prefix, T.intercalate "," [lat, long], suffix]
  where prefix = "https://use.mazemap.com/#v=1&config=uq&zlevel=1&center="
        suffix = "&zoom=16&campuses=uq&campusid=406"
