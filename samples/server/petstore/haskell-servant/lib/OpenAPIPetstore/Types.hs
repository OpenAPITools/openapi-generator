{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module OpenAPIPetstore.Types (
  ApiResponse (..),
  Category (..),
  Order (..),
  Pet (..),
  Tag (..),
  User (..),
  Day,
  LocalTime
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
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime)


-- | Describes the result of uploading an image resource
data ApiResponse = ApiResponse
  { apiResponse_code :: Int -- ^
  , apiResponse_type :: Text -- ^
  , apiResponse_message :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ApiResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "apiResponse_")
instance ToJSON ApiResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "apiResponse_")

-- | A category for a pet
data Category = Category
  { category_id :: Integer -- ^
  , category_name :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Category where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "category_")
instance ToJSON Category where
  toJSON = genericToJSON (removeFieldLabelPrefix False "category_")

-- | An order for a pets from the pet store
data Order = Order
  { order_id :: Integer -- ^
  , order_petId :: Integer -- ^
  , order_quantity :: Int -- ^
  , order_shipDate :: LocalTime -- ^
  , order_status :: Text -- ^Order Status
  , order_complete :: Bool -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Order where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "order_")
instance ToJSON Order where
  toJSON = genericToJSON (removeFieldLabelPrefix False "order_")

-- | A pet for sale in the pet store
data Pet = Pet
  { pet_id :: Integer -- ^
  , pet_category :: Category -- ^
  , pet_name :: Text -- ^
  , pet_photoUrls :: [Text] -- ^
  , pet_tags :: [Tag] -- ^
  , pet_status :: Text -- ^pet status in the store
  } deriving (Show, Eq, Generic)

instance FromJSON Pet where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pet_")
instance ToJSON Pet where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pet_")

-- | A tag for a pet
data Tag = Tag
  { tag_id :: Integer -- ^
  , tag_name :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Tag where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "tag_")
instance ToJSON Tag where
  toJSON = genericToJSON (removeFieldLabelPrefix False "tag_")

-- | A User who is purchasing from the pet store
data User = User
  { user_id :: Integer -- ^
  , user_username :: Text -- ^
  , user_firstName :: Text -- ^
  , user_lastName :: Text -- ^
  , user_email :: Text -- ^
  , user_password :: Text -- ^
  , user_phone :: Text -- ^
  , user_userStatus :: Int -- ^User Status
  } deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "user_")
instance ToJSON User where
  toJSON = genericToJSON (removeFieldLabelPrefix False "user_")

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
  {fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars}
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
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
