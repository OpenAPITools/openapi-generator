{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.UserApi (
      createUser
    , createUsersWithArrayInput
    , createUsersWithListInput
    , loginUser
    , logoutUser
    , getUserByName
    , updateUser
    , deleteUser
    , proxyUserApi
    , UserApi
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
import Model.User










type UserApi = "user" :> ReqBody '[JSON] User :> Post '[JSON] () -- createUser
    :<|> "user" :> "createWithArray" :> ReqBody '[JSON] [User] :> Post '[JSON] () -- createUsersWithArrayInput
    :<|> "user" :> "createWithList" :> ReqBody '[JSON] [User] :> Post '[JSON] () -- createUsersWithListInput
    :<|> "user" :> "login" :> QueryParam "username" String :> QueryParam "password" String :> Get '[JSON] String -- loginUser
    :<|> "user" :> "logout" :> Get '[JSON] () -- logoutUser
    :<|> "user" :> Capture "username" String :> Get '[JSON] User -- getUserByName
    :<|> "user" :> Capture "username" String :> ReqBody '[JSON] User :> Put '[JSON] () -- updateUser
    :<|> "user" :> Capture "username" String :> Delete '[JSON] () -- deleteUser

proxyUserApi :: Proxy UserApi
proxyUserApi = Proxy


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

createUser
    :<|> createUsersWithArrayInput
    :<|> createUsersWithListInput
    :<|> loginUser
    :<|> logoutUser
    :<|> getUserByName
    :<|> updateUser
    :<|> deleteUser
    = client proxyUserApi $ BaseUrl Http host port
