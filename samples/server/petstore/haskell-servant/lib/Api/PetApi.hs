{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.PetApi (
      updatePet
    , addPet
    , findPetsByStatus
    , findPetsByTags
    , getPetById
    , updatePetWithForm
    , deletePet
    , uploadFile
    , getPetByIdWithByteArray
    , addPetUsingByteArray
    , proxyPetApi
    , PetApi
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
import Model.Pet
import Model.Binary






data Formnamestatus = Formnamestatus
    { name :: String
    , status :: String
    } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded Formnamestatus where
    fromFormUrlEncoded inputs = Formnamestatus <$>  lkp inputs "name" <*>  lkp inputs "status"
instance ToFormUrlEncoded Formnamestatus where
    toFormUrlEncoded x = [((T.pack $ show $ Api.PetApi.name x), (T.pack $ show $ Api.PetApi.status x))]
instance Arbitrary Formnamestatus where
    arbitrary = Formnamestatus <$> arbitrary <*> arbitrary


data FormadditionalMetadatafile = FormadditionalMetadatafile
    { additionalMetadata :: String
    , file :: FilePath
    } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormadditionalMetadatafile where
    fromFormUrlEncoded inputs = FormadditionalMetadatafile <$>  lkp inputs "additionalMetadata" <*>  lkp inputs "file"
instance ToFormUrlEncoded FormadditionalMetadatafile where
    toFormUrlEncoded x = [((T.pack $ show $ Api.PetApi.additionalMetadata x), (T.pack $ show $ Api.PetApi.file x))]
instance Arbitrary FormadditionalMetadatafile where
    arbitrary = FormadditionalMetadatafile <$> arbitrary <*> arbitrary




type PetApi = "pet" :> ReqBody '[JSON] Pet :> Put '[JSON] () -- updatePet
    :<|> "pet" :> ReqBody '[JSON] Pet :> Post '[JSON] () -- addPet
    :<|> "pet" :> "findByStatus" :> QueryParam "status" [String] :> Get '[JSON] [Pet] -- findPetsByStatus
    :<|> "pet" :> "findByTags" :> QueryParam "tags" [String] :> Get '[JSON] [Pet] -- findPetsByTags
    :<|> "pet" :> Capture "petId" Integer :> Get '[JSON] Pet -- getPetById
    :<|> "pet" :> Capture "petId" String :> ReqBody '[FormUrlEncoded] Formnamestatus :> Post '[JSON] () -- updatePetWithForm
    :<|> "pet" :> Capture "petId" Integer :> Header "api_key" String :> Delete '[JSON] () -- deletePet
    :<|> "pet" :> Capture "petId" Integer :> "uploadImage" :> ReqBody '[FormUrlEncoded] FormadditionalMetadatafile :> Post '[JSON] () -- uploadFile
    :<|> "pet" :> Capture "petId" Integer?testing_byte_array=true :> Get '[JSON] Binary -- getPetByIdWithByteArray
    :<|> "pet?testing_byte_array=true" :> ReqBody '[JSON] Binary :> Post '[JSON] () -- addPetUsingByteArray

proxyPetApi :: Proxy PetApi
proxyPetApi = Proxy


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

updatePet
    :<|> addPet
    :<|> findPetsByStatus
    :<|> findPetsByTags
    :<|> getPetById
    :<|> updatePetWithForm
    :<|> deletePet
    :<|> uploadFile
    :<|> getPetByIdWithByteArray
    :<|> addPetUsingByteArray
    = client proxyPetApi $ BaseUrl Http host port
