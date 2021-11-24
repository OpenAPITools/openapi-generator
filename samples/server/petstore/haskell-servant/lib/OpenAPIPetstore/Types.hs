{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module OpenAPIPetstore.Types (
  ApiResponse (..),
  Category (..),
  Order (..),
  Pet (..),
  Tag (..),
  User (..),
  ) where

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (stripPrefix)
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
import Data.Function ((&))


-- | Describes the result of uploading an image resource
data ApiResponse = ApiResponse
  { apiResponseCode :: Maybe Int -- ^ 
  , apiResponseType :: Maybe Text -- ^ 
  , apiResponseMessage :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ApiResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "apiResponse")
instance ToJSON ApiResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "apiResponse")
instance ToSchema ApiResponse where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "apiResponse"


-- | A category for a pet
data Category = Category
  { categoryId :: Maybe Integer -- ^ 
  , categoryName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Category where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "category")
instance ToJSON Category where
  toJSON = genericToJSON (removeFieldLabelPrefix False "category")
instance ToSchema Category where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "category"


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
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "order")
instance ToJSON Order where
  toJSON = genericToJSON (removeFieldLabelPrefix False "order")
instance ToSchema Order where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "order"


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
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pet")
instance ToJSON Pet where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pet")
instance ToSchema Pet where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "pet"


-- | A tag for a pet
data Tag = Tag
  { tagId :: Maybe Integer -- ^ 
  , tagName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Tag where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "tag")
instance ToJSON Tag where
  toJSON = genericToJSON (removeFieldLabelPrefix False "tag")
instance ToSchema Tag where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "tag"


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
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "user")
instance ToJSON User where
  toJSON = genericToJSON (removeFieldLabelPrefix False "user")
instance ToSchema User where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "user"


uncapitalize :: String -> String
uncapitalize (first:rest) = Char.toLower first : rest
uncapitalize [] = []

-- | Remove a field label prefix during JSON parsing.
--   Also perform any replacements for special characters.
--   The @forParsing@ parameter is to distinguish between the cases in which we're using this
--   to power a @FromJSON@ or a @ToJSON@ instance. In the first case we're parsing, and we want
--   to replace special characters with their quoted equivalents (because we cannot have special
--   chars in identifier names), while we want to do vice versa when sending data instead.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = uncapitalize . fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("@", "'At")
      , ("\\", "'Back_Slash")
      , ("<=", "'Less_Than_Or_Equal_To")
      , ("\"", "'Double_Quote")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("^", "'Caret")
      , ("_", "'Underscore")
      , ("`", "'Backtick")
      , ("!", "'Exclamation")
      , ("#", "'Hash")
      , ("$", "'Dollar")
      , ("%", "'Percent")
      , ("&", "'Ampersand")
      , ("'", "'Quote")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("*", "'Star")
      , ("+", "'Plus")
      , (",", "'Comma")
      , ("-", "'Dash")
      , (".", "'Period")
      , ("/", "'Slash")
      , (":", "'Colon")
      , (";", "'Semicolon")
      , ("{", "'Left_Curly_Bracket")
      , ("|", "'Pipe")
      , ("<", "'LessThan")
      , ("!=", "'Not_Equal")
      , ("=", "'Equal")
      , ("}", "'Right_Curly_Bracket")
      , (">", "'GreaterThan")
      , ("~", "'Tilde")
      , ("?", "'Question_Mark")
      , (">=", "'Greater_Than_Or_Equal_To")
      , ("~=", "'Tilde_Equal")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
