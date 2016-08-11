{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.StoreApi (
      getInventory
    , placeOrder
    , getOrderById
    , deleteOrder
    , proxyStoreApi
    , StoreApi
    ) where

import GHC.Generics
import Data.Proxy
import Servant.API
import Servant.Client
import Network.URI (URI (..), URIAuth (..), parseURI)
import Data.Maybe (fromMaybe)
import Servant.Common.Text
import Data.List (intercalate)
import qualified Data.Text as T
import Utils
import Test.QuickCheck
import qualified Data.Map as Map
import Model.Order






type StoreApi = "store" :> "inventory" :> Get '[JSON] (Map.Map String Integer) -- getInventory
    :<|> "store" :> "order" :> ReqBody '[JSON] Order :> Post '[JSON] Order -- placeOrder
    :<|> "store" :> "order" :> Capture "orderId" String :> Get '[JSON] Order -- getOrderById
    :<|> "store" :> "order" :> Capture "orderId" String :> Delete '[JSON] () -- deleteOrder

proxyStoreApi :: Proxy StoreApi
proxyStoreApi = Proxy


serverPath :: String
serverPath = "http://petstore.swagger.io/v2"

parseHostPort :: String -> (String, Int)
parseHostPort path = (host,port)
    where
        authority = case parseURI path of
            Just x -> uriAuthority x
            _      -> Nothing
        (host, port) = case authority of
            Just y -> (uriRegName y, (getPort . uriPort) y)
            _      -> ("localhost", 8080)
        getPort p = case (length p) of
            0 -> 80
            _ -> (read . drop 1) p

(host, port) = parseHostPort serverPath

getInventory
    :<|> placeOrder
    :<|> getOrderById
    :<|> deleteOrder
    = client proxyStoreApi $ BaseUrl Http host port
