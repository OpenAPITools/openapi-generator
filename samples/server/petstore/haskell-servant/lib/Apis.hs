{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module Apis (
      api
    , API
    ) where

import Api.UserApi (UserApi)
import Api.PetApi (PetApi)
import Api.StoreApi (StoreApi)

import Data.Proxy
import Servant.API
import Test.QuickCheck
import qualified Data.Map as Map
import Utils

type API = UserApi :<|> PetApi :<|> StoreApi

api :: Proxy API
api = Proxy
