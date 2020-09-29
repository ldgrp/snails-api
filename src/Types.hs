{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
   ( URL(..)
   , Token(..)
   , Id(..)
   , EntryId(..)
   , UserId(..)
   , NewsId(..)
   , MessageId(..)
   , Entry(..)
   , Message(..)
   , Vehicle(..)
   , Map(..)
   , User(..)
   , WeatherCondition (..)
   , Weather(..)
   , News(..)
   ) where

import Data.Char
import Data.Time
import GHC.Generics
import Data.Int

import Data.Aeson as A
import Data.Swagger as S hiding (URL)
import qualified Data.Text as T


jsonOptions :: String -> Options
jsonOptions name = defaultOptions
   { A.fieldLabelModifier = formatFieldLabel name }

schemaOptions :: String -> SchemaOptions
schemaOptions name = defaultSchemaOptions
   { S.fieldLabelModifier = formatFieldLabel name }

formatFieldLabel :: String -> String -> String
formatFieldLabel = (camelCase .) . drop . length

camelCase :: String -> String
camelCase (x:xs) = toLower x : xs


type URL = T.Text

type Token = T.Text

type Id = Int64

type EntryId = Id

data Entry = Entry 
   { entryId :: EntryId
   , entryCreatedAt :: UTCTime
   , entryContent :: T.Text
   , entryAuthor :: User
   , entryReplyTo :: Maybe EntryId
   , entryLikedBy :: [User]
   , entryReplies :: [EntryId]
   } deriving (Eq, Show, Generic)

instance ToJSON Entry where
   toJSON = genericToJSON $ jsonOptions "entry"

instance ToSchema Entry where
   declareNamedSchema = genericDeclareNamedSchema $ schemaOptions "entry"


type MessageId = Id

data Message = Message 
    { messageFrom :: User
    , messageTo :: User
    , messageContent :: T.Text
    , messageCreatedAt :: UTCTime
    , messageId :: MessageId
    } deriving (Eq, Show, Generic)

instance ToJSON Message where
   toJSON = genericToJSON $ jsonOptions "message"

instance ToSchema Message where
   declareNamedSchema = genericDeclareNamedSchema $ schemaOptions "message"


data Vehicle = Vehicle 
    { vehicleName :: T.Text
    , vehicleCode :: T.Text
    , vehicleEta :: Integer
    } deriving (Eq, Show, Generic)

instance ToJSON Vehicle where
   toJSON = genericToJSON $ jsonOptions "vehicle"

instance ToSchema Vehicle where
   declareNamedSchema = genericDeclareNamedSchema $ schemaOptions "vehicle"


data Map = Map
    { mapUrl :: URL
    } deriving (Eq, Show, Generic)

instance ToJSON Map where
   toJSON = genericToJSON $ jsonOptions "map"

instance ToSchema Map where
   declareNamedSchema = genericDeclareNamedSchema $ schemaOptions "map"


type UserId = Id

data User = User 
   { userId :: UserId
   , userName :: T.Text
   , userUsername :: T.Text
   , userCreatedAt :: UTCTime
   } deriving (Eq, Show, Generic)

instance ToJSON User where
   toJSON = genericToJSON $ jsonOptions "user"

instance ToSchema User where
   declareNamedSchema = genericDeclareNamedSchema $ schemaOptions "user"


-- | See https://beta.design.bom.gov.au/components/weather-icons
data WeatherCondition 
        = Clear
        | Cloudy
        | Cyclone
        | Dust
        | Fog
        | Frost
        | Haze
        | HeavyShowers
        | LightRain
        | LightShowers
        | MostlySunny
        | PartlyCloudy
        | Rain
        | Showers
        | Snow
        | Storms
        | Sunny 
        | Wind
        deriving (Eq, Show, Enum, Generic)

instance ToJSON WeatherCondition

instance ToSchema WeatherCondition

data Weather = Weather 
   { weatherCreatedAt :: UTCTime 
   , weatherCurrentTemperature :: Maybe Integer
   , weatherMinTemperature :: Maybe Integer
   , weatherMaxTemperature :: Maybe Integer
   , weatherHumidity :: Maybe Double
   , weatherPrecipitation :: Maybe Double
   , weatherConditions :: Maybe WeatherCondition
   , weatherUvIndex :: Maybe Integer
   } deriving (Eq, Show, Generic)

instance ToJSON Weather where
   toJSON = genericToJSON $ jsonOptions "weather"

instance ToSchema Weather where
   declareNamedSchema = genericDeclareNamedSchema $ schemaOptions "weather"


type NewsId = Id

data News = News
   { newsId :: NewsId
   , newsTitle :: T.Text
   , newsContent :: T.Text
   , newsImageUrl :: URL
   , newsUrl :: URL
   , newsCreatedAt :: UTCTime
   } deriving (Eq, Show, Generic)

instance ToJSON News where
   toJSON = genericToJSON $ jsonOptions "news"

instance ToSchema News where
   declareNamedSchema = genericDeclareNamedSchema $ schemaOptions "news"
