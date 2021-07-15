{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module OpenAPIPetstore.API
  ( -- * Client and Server
    Config(..)
  , OpenAPIPetstoreBackend(..)
  , createOpenAPIPetstoreClient
  , runOpenAPIPetstoreServer
  , runOpenAPIPetstoreMiddlewareServer
  , runOpenAPIPetstoreClient
  , runOpenAPIPetstoreClientWithManager
  , callOpenAPIPetstore
  , OpenAPIPetstoreClient
  , OpenAPIPetstoreClientError(..)
  -- ** Servant
  , OpenAPIPetstoreAPI
  -- ** Plain WAI Application
  , serverWaiApplicationOpenAPIPetstore
  ) where

import           OpenAPIPetstore.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.Coerce                        (coerce)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import           Data.Proxy                         (Proxy (..))
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time
import           Data.UUID                          (UUID)
import           GHC.Exts                           (IsString (..))
import           GHC.Generics                       (Generic)
import           Network.HTTP.Client                (Manager, newManager)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types.Method          (methodOptions)
import           Network.Wai                        (Middleware)
import qualified Network.Wai.Handler.Warp           as Warp
import           Servant                            (ServerError, serve)
import           Servant.API
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..), Application)
import           Servant.Server.StaticFiles         (serveDirectoryFileServer)
import           Web.FormUrlEncoded
import           Web.HttpApiData



data FormUpdatePetWithForm = FormUpdatePetWithForm
  { updatePetWithFormName :: Text
  , updatePetWithFormStatus :: Text
  } deriving (Show, Eq, Generic, Data)

instance FromForm FormUpdatePetWithForm
instance ToForm FormUpdatePetWithForm

data FormUploadFile = FormUploadFile
  { uploadFileAdditionalMetadata :: Text
  , uploadFileFile :: FilePath
  } deriving (Show, Eq, Generic, Data)

instance FromForm FormUploadFile
instance ToForm FormUploadFile


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


-- | Servant type-level API, generated from the OpenAPI spec for OpenAPIPetstore.
type OpenAPIPetstoreAPI
    =    "pet" :> ReqBody '[JSON] Pet :> Verb 'POST 200 '[JSON] NoContent -- 'addPet' route
    :<|> "pet" :> Capture "petId" Integer :> Header "api_key" Text :> Verb 'DELETE 200 '[JSON] NoContent -- 'deletePet' route
    :<|> "pet" :> "findByStatus" :> QueryParam "status" (QueryList 'CommaSeparated (Text)) :> Verb 'GET 200 '[JSON] [Pet] -- 'findPetsByStatus' route
    :<|> "pet" :> "findByTags" :> QueryParam "tags" (QueryList 'CommaSeparated (Text)) :> Verb 'GET 200 '[JSON] [Pet] -- 'findPetsByTags' route
    :<|> "pet" :> Capture "petId" Integer :> Verb 'GET 200 '[JSON] Pet -- 'getPetById' route
    :<|> "pet" :> ReqBody '[JSON] Pet :> Verb 'PUT 200 '[JSON] NoContent -- 'updatePet' route
    :<|> "pet" :> Capture "petId" Integer :> ReqBody '[FormUrlEncoded] FormUpdatePetWithForm :> Verb 'POST 200 '[JSON] NoContent -- 'updatePetWithForm' route
    :<|> "pet" :> Capture "petId" Integer :> "uploadImage" :> ReqBody '[FormUrlEncoded] FormUploadFile :> Verb 'POST 200 '[JSON] ApiResponse -- 'uploadFile' route
    :<|> "store" :> "order" :> Capture "orderId" Text :> Verb 'DELETE 200 '[JSON] NoContent -- 'deleteOrder' route
    :<|> "store" :> "inventory" :> Verb 'GET 200 '[JSON] ((Map.Map String Int)) -- 'getInventory' route
    :<|> "store" :> "order" :> Capture "orderId" Integer :> Verb 'GET 200 '[JSON] Order -- 'getOrderById' route
    :<|> "store" :> "order" :> ReqBody '[JSON] Order :> Verb 'POST 200 '[JSON] Order -- 'placeOrder' route
    :<|> "user" :> ReqBody '[JSON] User :> Verb 'POST 200 '[JSON] NoContent -- 'createUser' route
    :<|> "user" :> "createWithArray" :> ReqBody '[JSON] [User] :> Verb 'POST 200 '[JSON] NoContent -- 'createUsersWithArrayInput' route
    :<|> "user" :> "createWithList" :> ReqBody '[JSON] [User] :> Verb 'POST 200 '[JSON] NoContent -- 'createUsersWithListInput' route
    :<|> "user" :> Capture "username" Text :> Verb 'DELETE 200 '[JSON] NoContent -- 'deleteUser' route
    :<|> "user" :> Capture "username" Text :> Verb 'GET 200 '[JSON] User -- 'getUserByName' route
    :<|> "user" :> "login" :> QueryParam "username" Text :> QueryParam "password" Text :> Verb 'GET 200 '[JSON] Text -- 'loginUser' route
    :<|> "user" :> "logout" :> Verb 'GET 200 '[JSON] NoContent -- 'logoutUser' route
    :<|> "user" :> Capture "username" Text :> ReqBody '[JSON] User :> Verb 'PUT 200 '[JSON] NoContent -- 'updateUser' route
    :<|> Raw 


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype OpenAPIPetstoreClientError = OpenAPIPetstoreClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for OpenAPIPetstore.
-- The backend can be used both for the client and the server. The client generated from the OpenAPIPetstore OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createOpenAPIPetstoreClient@). Alternatively, provided
-- a backend, the API can be served using @runOpenAPIPetstoreMiddlewareServer@.
data OpenAPIPetstoreBackend m = OpenAPIPetstoreBackend
  { addPet :: Pet -> m NoContent{- ^  -}
  , deletePet :: Integer -> Maybe Text -> m NoContent{- ^  -}
  , findPetsByStatus :: Maybe [Text] -> m [Pet]{- ^ Multiple status values can be provided with comma separated strings -}
  , findPetsByTags :: Maybe [Text] -> m [Pet]{- ^ Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing. -}
  , getPetById :: Integer -> m Pet{- ^ Returns a single pet -}
  , updatePet :: Pet -> m NoContent{- ^  -}
  , updatePetWithForm :: Integer -> FormUpdatePetWithForm -> m NoContent{- ^  -}
  , uploadFile :: Integer -> FormUploadFile -> m ApiResponse{- ^  -}
  , deleteOrder :: Text -> m NoContent{- ^ For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors -}
  , getInventory :: m ((Map.Map String Int)){- ^ Returns a map of status codes to quantities -}
  , getOrderById :: Integer -> m Order{- ^ For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions -}
  , placeOrder :: Order -> m Order{- ^  -}
  , createUser :: User -> m NoContent{- ^ This can only be done by the logged in user. -}
  , createUsersWithArrayInput :: [User] -> m NoContent{- ^  -}
  , createUsersWithListInput :: [User] -> m NoContent{- ^  -}
  , deleteUser :: Text -> m NoContent{- ^ This can only be done by the logged in user. -}
  , getUserByName :: Text -> m User{- ^  -}
  , loginUser :: Maybe Text -> Maybe Text -> m Text{- ^  -}
  , logoutUser :: m NoContent{- ^  -}
  , updateUser :: Text -> User -> m NoContent{- ^ This can only be done by the logged in user. -}
  }

newtype OpenAPIPetstoreClient a = OpenAPIPetstoreClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative OpenAPIPetstoreClient where
  pure x = OpenAPIPetstoreClient (\_ -> pure x)
  (OpenAPIPetstoreClient f) <*> (OpenAPIPetstoreClient x) =
    OpenAPIPetstoreClient (\env -> f env <*> x env)

instance Monad OpenAPIPetstoreClient where
  (OpenAPIPetstoreClient a) >>= f =
    OpenAPIPetstoreClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO OpenAPIPetstoreClient where
  liftIO io = OpenAPIPetstoreClient (\_ -> liftIO io)

createOpenAPIPetstoreClient :: OpenAPIPetstoreBackend OpenAPIPetstoreClient
createOpenAPIPetstoreClient = OpenAPIPetstoreBackend{..}
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
     (coerce -> updateUser) :<|>
     _) = client (Proxy :: Proxy OpenAPIPetstoreAPI)

-- | Run requests in the OpenAPIPetstoreClient monad.
runOpenAPIPetstoreClient :: Config -> OpenAPIPetstoreClient a -> ExceptT ClientError IO a
runOpenAPIPetstoreClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runOpenAPIPetstoreClientWithManager manager clientConfig cl

-- | Run requests in the OpenAPIPetstoreClient monad using a custom manager.
runOpenAPIPetstoreClientWithManager :: Manager -> Config -> OpenAPIPetstoreClient a -> ExceptT ClientError IO a
runOpenAPIPetstoreClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a OpenAPIPetstoreClientError
callOpenAPIPetstore
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> OpenAPIPetstoreClient a -> m a
callOpenAPIPetstore env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (OpenAPIPetstoreClientError err)
    Right response -> pure response


requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the OpenAPIPetstore server at the provided host and port.
runOpenAPIPetstoreServer
  :: (MonadIO m, MonadThrow m)
  => Config -> OpenAPIPetstoreBackend (ExceptT ServerError IO) -> m ()
runOpenAPIPetstoreServer config backend = runOpenAPIPetstoreMiddlewareServer config requestMiddlewareId backend

-- | Run the OpenAPIPetstore server at the provided host and port.
runOpenAPIPetstoreMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> OpenAPIPetstoreBackend (ExceptT ServerError IO) -> m ()
runOpenAPIPetstoreMiddlewareServer Config{..} middleware backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serverWaiApplicationOpenAPIPetstore backend

-- | Plain "Network.Wai" Application for the OpenAPIPetstore server.
--
-- Can be used to implement e.g. tests that call the API without a full webserver.
serverWaiApplicationOpenAPIPetstore :: OpenAPIPetstoreBackend (ExceptT ServerError IO) -> Application
serverWaiApplicationOpenAPIPetstore backend = serve (Proxy :: Proxy OpenAPIPetstoreAPI) (serverFromBackend backend)
  where
    serverFromBackend OpenAPIPetstoreBackend{..} =
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
       coerce updateUser :<|>
       serveDirectoryFileServer "static")
