{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Transport where

import Database.Persist
import Database.Persist.Sql
import Servant
import Servant.API

import Db
import qualified Data.Text as T
import qualified Types as S

type TransportAPI =
      Summary "Get vehicles" 
         :> "transport" 
         :> QueryParams "code" T.Text
         :> QueryParam "count" Int
         :> Get '[JSON] [S.Vehicle]

transportServer :: Server TransportAPI
transportServer = getVehicles

-- | Get `count` vehicles sorted by ETA
getVehicles :: [T.Text] -> Maybe Int -> Handler [S.Vehicle]
getVehicles codes maybeCount = do
  let filters = [VehicleCode ==. code | code <- codes]
  vehicles <- runDb $ selectList filters [Desc VehicleEta, LimitTo count]
  return $ fmap toVehicle vehicles
  where
    count :: Int
    count = maybe 5 id maybeCount


toVehicle :: Entity Vehicle -> S.Vehicle
toVehicle (Entity vkey v) = 
  S.Vehicle name code eta
  where 
    name = vehicleName v
    code = vehicleCode v
    eta = vehicleEta v