{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import           Data.Proxy
import           Data.Text

import           Database.Persist

import           Models

import           Servant.API

type Api
   = "sister" :> Capture "name" Text :> Get '[ JSON] Sister -- get sister
      :<|> "sister" :> ReqBody '[ JSON] Sister :> Post '[ JSON] Text -- add new sister
      :<|> "sister" :> Capture "name" Text :> ReqBody '[ JSON] Sister :> Put '[ JSON] Text -- update sister

api :: Proxy Api
api = Proxy
