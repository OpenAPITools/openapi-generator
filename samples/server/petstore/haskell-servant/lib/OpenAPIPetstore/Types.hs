{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module OpenAPIPetstore.Types (
  ApiResponse (..),
  Category (..),
  Order (..),
  Pet (..),
  SpecialCharacters (..),
  Tag (..),
  User (..),
  ) where

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.Swagger (ToSchema, declareNamedSchema)
import qualified Data.Swagger as Swagger
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)


-- | Describes the result of uploading an image resource
data ApiResponse = ApiResponse
  { apiResponseCode :: Maybe Int -- ^ 
  , apiResponseType :: Maybe Text -- ^ 
  , apiResponseMessage :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ApiResponse where
  parseJSON = genericParseJSON optionsApiResponse
instance ToJSON ApiResponse where
  toJSON = genericToJSON optionsApiResponse
instance ToSchema ApiResponse where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsApiResponse

optionsApiResponse :: Options
optionsApiResponse =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("apiResponseCode", "code")
      , ("apiResponseType", "type")
      , ("apiResponseMessage", "message")
      ]


-- | A category for a pet
data Category = Category
  { categoryId :: Maybe Integer -- ^ 
  , categoryName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Category where
  parseJSON = genericParseJSON optionsCategory
instance ToJSON Category where
  toJSON = genericToJSON optionsCategory
instance ToSchema Category where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsCategory

optionsCategory :: Options
optionsCategory =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("categoryId", "id")
      , ("categoryName", "name")
      ]


-- | An order for a pets from the pet store
data Order = Order
  { orderId :: Maybe Integer -- ^ 
  , orderPetId :: Maybe Integer -- ^ 
  , orderQuantity :: Maybe Int -- ^ 
  , orderShipDate :: Maybe UTCTime -- ^ 
  , orderStatus :: Maybe Text -- ^ Order Status
  , orderComplete :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Order where
  parseJSON = genericParseJSON optionsOrder
instance ToJSON Order where
  toJSON = genericToJSON optionsOrder
instance ToSchema Order where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsOrder

optionsOrder :: Options
optionsOrder =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("orderId", "id")
      , ("orderPetId", "petId")
      , ("orderQuantity", "quantity")
      , ("orderShipDate", "shipDate")
      , ("orderStatus", "status")
      , ("orderComplete", "complete")
      ]


-- | A pet for sale in the pet store
data Pet = Pet
  { petId :: Maybe Integer -- ^ 
  , petCategory :: Maybe Category -- ^ 
  , petName :: Text -- ^ 
  , petPhotoUrls :: [Text] -- ^ 
  , petTags :: Maybe [Tag] -- ^ 
  , petStatus :: Maybe Text -- ^ pet status in the store
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Pet where
  parseJSON = genericParseJSON optionsPet
instance ToJSON Pet where
  toJSON = genericToJSON optionsPet
instance ToSchema Pet where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsPet

optionsPet :: Options
optionsPet =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("petId", "id")
      , ("petCategory", "category")
      , ("petName", "name")
      , ("petPhotoUrls", "photoUrls")
      , ("petTags", "tags")
      , ("petStatus", "status")
      ]


-- | description
data SpecialCharacters = SpecialCharacters
  { specialCharactersDoubleQuote :: Text -- ^ double quote
  , specialCharactersBackSlash :: Text -- ^ backslash
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SpecialCharacters where
  parseJSON = genericParseJSON optionsSpecialCharacters
instance ToJSON SpecialCharacters where
  toJSON = genericToJSON optionsSpecialCharacters
instance ToSchema SpecialCharacters where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsSpecialCharacters

optionsSpecialCharacters :: Options
optionsSpecialCharacters =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("specialCharactersDoubleQuote", "\"")
      , ("specialCharactersBackSlash", "\\")
      ]


-- | A tag for a pet
data Tag = Tag
  { tagId :: Maybe Integer -- ^ 
  , tagName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Tag where
  parseJSON = genericParseJSON optionsTag
instance ToJSON Tag where
  toJSON = genericToJSON optionsTag
instance ToSchema Tag where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsTag

optionsTag :: Options
optionsTag =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tagId", "id")
      , ("tagName", "name")
      ]


-- | A User who is purchasing from the pet store
data User = User
  { userId :: Maybe Integer -- ^ 
  , userUsername :: Maybe Text -- ^ 
  , userFirstName :: Maybe Text -- ^ 
  , userLastName :: Maybe Text -- ^ 
  , userEmail :: Maybe Text -- ^ 
  , userPassword :: Maybe Text -- ^ 
  , userPhone :: Maybe Text -- ^ 
  , userUserStatus :: Maybe Int -- ^ User Status
  } deriving (Show, Eq, Generic, Data)

instance FromJSON User where
  parseJSON = genericParseJSON optionsUser
instance ToJSON User where
  toJSON = genericToJSON optionsUser
instance ToSchema User where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ optionsUser

optionsUser :: Options
optionsUser =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("userId", "id")
      , ("userUsername", "username")
      , ("userFirstName", "firstName")
      , ("userLastName", "lastName")
      , ("userEmail", "email")
      , ("userPassword", "password")
      , ("userPhone", "phone")
      , ("userUserStatus", "userStatus")
      ]

