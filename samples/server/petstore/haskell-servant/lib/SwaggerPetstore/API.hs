{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -fcontext-stack=328 #-}

module SwaggerPetstore.API
  -- * Client and Server
  ( ServerConfig(..)
  , SwaggerPetstoreBackend
  , createSwaggerPetstoreClient
  , runSwaggerPetstoreServer
  , runSwaggerPetstoreClient
  , runSwaggerPetstoreClientWithManager
  , SwaggerPetstoreClient
  -- ** Servant
  , SwaggerPetstoreAPI
  ) where

import SwaggerPetstore.Types

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class
import Data.Aeson (Value)
import Data.Coerce (coerce)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsString(..))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types.Method (methodOptions)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (ServantErr, serve)
import Servant.API
import Servant.API.Verbs (StdMethod(..), Verb)
import Servant.Client (Scheme(Http), ServantError, client)
import Servant.Common.BaseUrl (BaseUrl(..))
import Web.HttpApiData



data FormUpdatePetWithForm = FormUpdatePetWithForm
  { updatePetWithFormName :: Text
  , updatePetWithFormStatus :: Text
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormUpdatePetWithForm where
  fromFormUrlEncoded inputs = FormUpdatePetWithForm <$> lookupEither "name" inputs <*> lookupEither "status" inputs

instance ToFormUrlEncoded FormUpdatePetWithForm where
  toFormUrlEncoded value =
    [ ("name", toQueryParam $ updatePetWithFormName value)
    , ("status", toQueryParam $ updatePetWithFormStatus value)
    ]
data FormUploadFile = FormUploadFile
  { uploadFileAdditionalMetadata :: Text
  , uploadFileFile :: FilePath
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormUploadFile where
  fromFormUrlEncoded inputs = FormUploadFile <$> lookupEither "additionalMetadata" inputs <*> lookupEither "file" inputs

instance ToFormUrlEncoded FormUploadFile where
  toFormUrlEncoded value =
    [ ("additionalMetadata", toQueryParam $ uploadFileAdditionalMetadata value)
    , ("file", toQueryParam $ uploadFileFile value)
    ]

-- For the form data code generation.
lookupEither :: FromHttpApiData b => Text -> [(Text, Text)] -> Either String b
lookupEither key assocs =
  case lookup key assocs of
    Nothing -> Left $ "Could not find parameter " <> (T.unpack key) <> " in form data"
    Just value ->
      case parseQueryParam value of
        Left result -> Left $ T.unpack result
        Right result -> Right $ result

-- | Servant type-level API, generated from the Swagger spec for SwaggerPetstore.
type SwaggerPetstoreAPI
    =    "pet" :> ReqBody '[JSON] Pet :> Verb 'POST 200 '[JSON] () -- 'addPet' route
    :<|> "pet" :> Capture "petId" Integer :> Header "api_key" Text :> Verb 'DELETE 200 '[JSON] () -- 'deletePet' route
    :<|> "pet" :> "findByStatus" :> QueryParam "status" (QueryList 'CommaSeparated (Text)) :> Verb 'GET 200 '[JSON] [Pet] -- 'findPetsByStatus' route
    :<|> "pet" :> "findByTags" :> QueryParam "tags" (QueryList 'CommaSeparated (Text)) :> Verb 'GET 200 '[JSON] [Pet] -- 'findPetsByTags' route
    :<|> "pet" :> Capture "petId" Integer :> Verb 'GET 200 '[JSON] Pet -- 'getPetById' route
    :<|> "pet" :> ReqBody '[JSON] Pet :> Verb 'PUT 200 '[JSON] () -- 'updatePet' route
    :<|> "pet" :> Capture "petId" Integer :> ReqBody '[FormUrlEncoded] FormUpdatePetWithForm :> Verb 'POST 200 '[JSON] () -- 'updatePetWithForm' route
    :<|> "pet" :> Capture "petId" Integer :> "uploadImage" :> ReqBody '[FormUrlEncoded] FormUploadFile :> Verb 'POST 200 '[JSON] ApiResponse -- 'uploadFile' route
    :<|> "store" :> "order" :> Capture "orderId" Text :> Verb 'DELETE 200 '[JSON] () -- 'deleteOrder' route
    :<|> "store" :> "inventory" :> Verb 'GET 200 '[JSON] (Map.Map String Int) -- 'getInventory' route
    :<|> "store" :> "order" :> Capture "orderId" Integer :> Verb 'GET 200 '[JSON] Order -- 'getOrderById' route
    :<|> "store" :> "order" :> ReqBody '[JSON] Order :> Verb 'POST 200 '[JSON] Order -- 'placeOrder' route
    :<|> "user" :> ReqBody '[JSON] User :> Verb 'POST 200 '[JSON] () -- 'createUser' route
    :<|> "user" :> "createWithArray" :> ReqBody '[JSON] [User] :> Verb 'POST 200 '[JSON] () -- 'createUsersWithArrayInput' route
    :<|> "user" :> "createWithList" :> ReqBody '[JSON] [User] :> Verb 'POST 200 '[JSON] () -- 'createUsersWithListInput' route
    :<|> "user" :> Capture "username" Text :> Verb 'DELETE 200 '[JSON] () -- 'deleteUser' route
    :<|> "user" :> Capture "username" Text :> Verb 'GET 200 '[JSON] User -- 'getUserByName' route
    :<|> "user" :> "login" :> QueryParam "username" Text :> QueryParam "password" Text :> Verb 'GET 200 '[JSON] Text -- 'loginUser' route
    :<|> "user" :> "logout" :> Verb 'GET 200 '[JSON] () -- 'logoutUser' route
    :<|> "user" :> Capture "username" Text :> ReqBody '[JSON] User :> Verb 'PUT 200 '[JSON] () -- 'updateUser' route

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig
  { configHost :: String  -- ^ Hostname to serve on, e.g. "127.0.0.1"
  , configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)

-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Backend for SwaggerPetstore.
-- The backend can be used both for the client and the server. The client generated from the SwaggerPetstore Swagger spec
-- is a backend that executes actions by sending HTTP requests (see @createSwaggerPetstoreClient@). Alternatively, provided
-- a backend, the API can be served using @runSwaggerPetstoreServer@.
data SwaggerPetstoreBackend m = SwaggerPetstoreBackend
  { addPet :: Pet -> m (){- ^  -}
  , deletePet :: Integer -> Maybe Text -> m (){- ^  -}
  , findPetsByStatus :: Maybe [Text] -> m [Pet]{- ^ Multiple status values can be provided with comma separated strings -}
  , findPetsByTags :: Maybe [Text] -> m [Pet]{- ^ Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing. -}
  , getPetById :: Integer -> m Pet{- ^ Returns a single pet -}
  , updatePet :: Pet -> m (){- ^  -}
  , updatePetWithForm :: Integer -> FormUpdatePetWithForm -> m (){- ^  -}
  , uploadFile :: Integer -> FormUploadFile -> m ApiResponse{- ^  -}
  , deleteOrder :: Text -> m (){- ^ For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors -}
  , getInventory :: m (Map.Map String Int){- ^ Returns a map of status codes to quantities -}
  , getOrderById :: Integer -> m Order{- ^ For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions -}
  , placeOrder :: Order -> m Order{- ^  -}
  , createUser :: User -> m (){- ^ This can only be done by the logged in user. -}
  , createUsersWithArrayInput :: [User] -> m (){- ^  -}
  , createUsersWithListInput :: [User] -> m (){- ^  -}
  , deleteUser :: Text -> m (){- ^ This can only be done by the logged in user. -}
  , getUserByName :: Text -> m User{- ^  -}
  , loginUser :: Maybe Text -> Maybe Text -> m Text{- ^  -}
  , logoutUser :: m (){- ^  -}
  , updateUser :: Text -> User -> m (){- ^ This can only be done by the logged in user. -}
  }

newtype SwaggerPetstoreClient a = SwaggerPetstoreClient
  { runClient :: Manager -> BaseUrl -> ExceptT ServantError IO a
  } deriving Functor

instance Applicative SwaggerPetstoreClient where
  pure x = SwaggerPetstoreClient (\_ _ -> pure x)
  (SwaggerPetstoreClient f) <*> (SwaggerPetstoreClient x) =
    SwaggerPetstoreClient (\manager url -> f manager url <*> x manager url)

instance Monad SwaggerPetstoreClient where
  (SwaggerPetstoreClient a) >>= f =
    SwaggerPetstoreClient (\manager url -> do
      value <- a manager url
      runClient (f value) manager url)

instance MonadIO SwaggerPetstoreClient where
  liftIO io = SwaggerPetstoreClient (\_ _ -> liftIO io)

createSwaggerPetstoreClient :: SwaggerPetstoreBackend SwaggerPetstoreClient
createSwaggerPetstoreClient = SwaggerPetstoreBackend{..}
  where
    ((coerce -> addPet) :<|>
     (coerce -> deletePet) :<|>
     (coerce -> findPetsByStatus) :<|>
     (coerce -> findPetsByTags) :<|>
     (coerce -> getPetById) :<|>
     (coerce -> updatePet) :<|>
     (coerce -> updatePetWithForm) :<|>
     (coerce -> uploadFile) :<|>
     (coerce -> deleteOrder) :<|>
     (coerce -> getInventory) :<|>
     (coerce -> getOrderById) :<|>
     (coerce -> placeOrder) :<|>
     (coerce -> createUser) :<|>
     (coerce -> createUsersWithArrayInput) :<|>
     (coerce -> createUsersWithListInput) :<|>
     (coerce -> deleteUser) :<|>
     (coerce -> getUserByName) :<|>
     (coerce -> loginUser) :<|>
     (coerce -> logoutUser) :<|>
     (coerce -> updateUser)) = client (Proxy :: Proxy SwaggerPetstoreAPI)

-- | Run requests in the SwaggerPetstoreClient monad.
runSwaggerPetstoreClient :: ServerConfig -> SwaggerPetstoreClient a -> ExceptT ServantError IO a
runSwaggerPetstoreClient clientConfig cl = do
  manager <- liftIO $ newManager defaultManagerSettings
  runSwaggerPetstoreClientWithManager manager clientConfig cl

-- | Run requests in the SwaggerPetstoreClient monad using a custom manager.
runSwaggerPetstoreClientWithManager :: Manager -> ServerConfig -> SwaggerPetstoreClient a -> ExceptT ServantError IO a
runSwaggerPetstoreClientWithManager manager clientConfig cl =
  runClient cl manager $ BaseUrl Http (configHost clientConfig) (configPort clientConfig) ""

-- | Run the SwaggerPetstore server at the provided host and port.
runSwaggerPetstoreServer :: MonadIO m => ServerConfig -> SwaggerPetstoreBackend (ExceptT ServantErr IO)  -> m ()
runSwaggerPetstoreServer ServerConfig{..} backend =
  liftIO $ Warp.runSettings warpSettings $ serve (Proxy :: Proxy SwaggerPetstoreAPI) (serverFromBackend backend)
  where
    warpSettings = Warp.defaultSettings & Warp.setPort configPort & Warp.setHost (fromString configHost)
    serverFromBackend SwaggerPetstoreBackend{..} =
      (coerce addPet :<|>
       coerce deletePet :<|>
       coerce findPetsByStatus :<|>
       coerce findPetsByTags :<|>
       coerce getPetById :<|>
       coerce updatePet :<|>
       coerce updatePetWithForm :<|>
       coerce uploadFile :<|>
       coerce deleteOrder :<|>
       coerce getInventory :<|>
       coerce getOrderById :<|>
       coerce placeOrder :<|>
       coerce createUser :<|>
       coerce createUsersWithArrayInput :<|>
       coerce createUsersWithListInput :<|>
       coerce deleteUser :<|>
       coerce getUserByName :<|>
       coerce loginUser :<|>
       coerce logoutUser :<|>
       coerce updateUser)
