{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Transport where

import Servant
import Servant.API
import Types

import qualified Data.Text as T

type TransportAPI =
      Summary "Get vehicles" 
         :> "transport" 
         :> QueryParams "code" T.Text
         :> QueryParam "count" Integer
         :> Get '[JSON] [Vehicle]
