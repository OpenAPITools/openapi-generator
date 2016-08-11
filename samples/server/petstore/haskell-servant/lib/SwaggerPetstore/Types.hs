{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SwaggerPetstore.Types (
    ApiResponse (..),
    Category (..),
    Order (..),
    Pet (..),
    Tag (..),
    User (..),
    ) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | 
data ApiResponse = ApiResponse
    { apiResponseCode :: Int -- ^ 
    , apiResponseType_ :: Text -- ^ 
    , apiResponseMessage :: Text -- ^ 
    } deriving (Show, Eq, Generic)

instance FromJSON ApiResponse where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "apiResponse")
instance ToJSON ApiResponse where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "apiResponse")

-- | 
data Category = Category
    { categoryId :: Integer -- ^ 
    , categoryName :: Text -- ^ 
    } deriving (Show, Eq, Generic)

instance FromJSON Category where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "category")
instance ToJSON Category where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "category")

-- | 
data Order = Order
    { orderId :: Integer -- ^ 
    , orderPetId :: Integer -- ^ 
    , orderQuantity :: Int -- ^ 
    , orderShipDate :: Integer -- ^ 
    , orderStatus :: Text -- ^ Order Status
    , orderComplete :: Bool -- ^ 
    } deriving (Show, Eq, Generic)

instance FromJSON Order where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "order")
instance ToJSON Order where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "order")

-- | 
data Pet = Pet
    { petId :: Integer -- ^ 
    , petCategory :: Category -- ^ 
    , petName :: Text -- ^ 
    , petPhotoUrls :: [Text] -- ^ 
    , petTags :: [Tag] -- ^ 
    , petStatus :: Text -- ^ pet status in the store
    } deriving (Show, Eq, Generic)

instance FromJSON Pet where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "pet")
instance ToJSON Pet where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "pet")

-- | 
data Tag = Tag
    { tagId :: Integer -- ^ 
    , tagName :: Text -- ^ 
    } deriving (Show, Eq, Generic)

instance FromJSON Tag where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "tag")
instance ToJSON Tag where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "tag")

-- | 
data User = User
    { userId :: Integer -- ^ 
    , userUsername :: Text -- ^ 
    , userFirstName :: Text -- ^ 
    , userLastName :: Text -- ^ 
    , userEmail :: Text -- ^ 
    , userPassword :: Text -- ^ 
    , userPhone :: Text -- ^ 
    , userUserStatus :: Int -- ^ User Status
    } deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "user")
instance ToJSON User where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "user")

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars = [("#", "'Hash"), ("!", "'Exclamation"), ("&", "'Ampersand"), ("@", "'At"), ("$", "'Dollar"), ("%", "'Percent"), ("*", "'Star"), ("+", "'Plus"), (">=", "'Greater_Than_Or_Equal_To"), ("-", "'Dash"), ("<=", "'Less_Than_Or_Equal_To"), ("!=", "'Greater_Than_Or_Equal_To"), (":", "'Colon"), ("^", "'Caret"), ("|", "'Pipe"), (">", "'GreaterThan"), ("=", "'Equal"), ("<", "'LessThan")]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer = if forParsing then flip T.replace else T.replace


