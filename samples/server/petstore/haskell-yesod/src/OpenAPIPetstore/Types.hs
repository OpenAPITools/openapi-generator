{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE NoImplicitPrelude          #-}
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

import ClassyPrelude.Yesod
import Data.Foldable (foldl)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)


-- | Describes the result of uploading an image resource
data ApiResponse = ApiResponse
  { apiResponseCode :: Maybe Int -- ^ 
  , apiResponseType :: Maybe Text -- ^ 
  , apiResponseMessage :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON ApiResponse where
  parseJSON = genericParseJSON optionsApiResponse
instance ToJSON ApiResponse where
  toJSON = genericToJSON optionsApiResponse

optionsApiResponse :: Options
optionsApiResponse =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ List.lookup s table
    }
  where
    table =
      [ ("apiResponseCode", "code")
      , ("apiResponseType", "type")
      , ("apiResponseMessage", "message")
      ]


-- | A category for a pet
data Category = Category
  { categoryId :: Maybe Int64 -- ^ 
  , categoryName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Category where
  parseJSON = genericParseJSON optionsCategory
instance ToJSON Category where
  toJSON = genericToJSON optionsCategory

optionsCategory :: Options
optionsCategory =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ List.lookup s table
    }
  where
    table =
      [ ("categoryId", "id")
      , ("categoryName", "name")
      ]


-- | An order for a pets from the pet store
data Order = Order
  { orderId :: Maybe Int64 -- ^ 
  , orderPetId :: Maybe Int64 -- ^ 
  , orderQuantity :: Maybe Int -- ^ 
  , orderShipDate :: Maybe UTCTime -- ^ 
  , orderStatus :: Maybe Text -- ^ Order Status
  , orderComplete :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Order where
  parseJSON = genericParseJSON optionsOrder
instance ToJSON Order where
  toJSON = genericToJSON optionsOrder

optionsOrder :: Options
optionsOrder =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ List.lookup s table
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
  { petId :: Maybe Int64 -- ^ 
  , petCategory :: Maybe Category -- ^ 
  , petName :: Text -- ^ 
  , petPhotoUrls :: [Text] -- ^ 
  , petTags :: Maybe [Tag] -- ^ 
  , petStatus :: Maybe Text -- ^ pet status in the store
  } deriving (Show, Eq, Generic)

instance FromJSON Pet where
  parseJSON = genericParseJSON optionsPet
instance ToJSON Pet where
  toJSON = genericToJSON optionsPet

optionsPet :: Options
optionsPet =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ List.lookup s table
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
  } deriving (Show, Eq, Generic)

instance FromJSON SpecialCharacters where
  parseJSON = genericParseJSON optionsSpecialCharacters
instance ToJSON SpecialCharacters where
  toJSON = genericToJSON optionsSpecialCharacters

optionsSpecialCharacters :: Options
optionsSpecialCharacters =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ List.lookup s table
    }
  where
    table =
      [ ("specialCharactersDoubleQuote", "\"")
      , ("specialCharactersBackSlash", "\\")
      ]


-- | A tag for a pet
data Tag = Tag
  { tagId :: Maybe Int64 -- ^ 
  , tagName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Tag where
  parseJSON = genericParseJSON optionsTag
instance ToJSON Tag where
  toJSON = genericToJSON optionsTag

optionsTag :: Options
optionsTag =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ List.lookup s table
    }
  where
    table =
      [ ("tagId", "id")
      , ("tagName", "name")
      ]


-- | A User who is purchasing from the pet store
data User = User
  { userId :: Maybe Int64 -- ^ 
  , userUsername :: Maybe Text -- ^ 
  , userFirstName :: Maybe Text -- ^ 
  , userLastName :: Maybe Text -- ^ 
  , userEmail :: Maybe Text -- ^ 
  , userPassword :: Maybe Text -- ^ 
  , userPhone :: Maybe Text -- ^ 
  , userUserStatus :: Maybe Int -- ^ User Status
  } deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON optionsUser
instance ToJSON User where
  toJSON = genericToJSON optionsUser

optionsUser :: Options
optionsUser =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ List.lookup s table
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

