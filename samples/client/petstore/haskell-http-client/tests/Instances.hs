{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Instances where

import Data.Text (Text, pack)
import Data.Char (isSpace)
import Data.List (sort)
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Test.QuickCheck
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set

import ApproxEq
import SwaggerPetstore.Model

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (ModifiedJulianDay <$>) . shrink . toModifiedJulianDay

instance Arbitrary UTCTime where
  arbitrary =
    UTCTime <$> arbitrary <*> (secondsToDiffTime <$> choose (0, 86401))

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq Day where
  (=~) = (==)

-- * Models
 
instance Arbitrary ApiResponse where
  arbitrary =
    ApiResponse
    <$> arbitrary -- apiResponseCode :: Maybe Int
    <*> arbitrary -- apiResponseType :: Maybe Text
    <*> arbitrary -- apiResponseMessage :: Maybe Text
    

instance Arbitrary Category where
  arbitrary =
    Category
    <$> arbitrary -- categoryId :: Maybe Integer
    <*> arbitrary -- categoryName :: Maybe Text
    

instance Arbitrary Order where
  arbitrary =
    Order
    <$> arbitrary -- orderId :: Maybe Integer
    <*> arbitrary -- orderPetId :: Maybe Integer
    <*> arbitrary -- orderQuantity :: Maybe Int
    <*> arbitrary -- orderShipDate :: Maybe UTCTime
    <*> arbitrary -- orderStatus :: Maybe Text
    <*> arbitrary -- orderComplete :: Maybe Bool
    

instance Arbitrary Pet where
  arbitrary =
    Pet
    <$> arbitrary -- petId :: Maybe Integer
    <*> arbitrary -- petCategory :: Maybe Category
    <*> arbitrary -- petName :: Text
    <*> arbitrary -- petPhotoUrls :: [Text]
    <*> arbitrary -- petTags :: Maybe [Tag]
    <*> arbitrary -- petStatus :: Maybe Text
    

instance Arbitrary Tag where
  arbitrary =
    Tag
    <$> arbitrary -- tagId :: Maybe Integer
    <*> arbitrary -- tagName :: Maybe Text
    

instance Arbitrary User where
  arbitrary =
    User
    <$> arbitrary -- userId :: Maybe Integer
    <*> arbitrary -- userUsername :: Maybe Text
    <*> arbitrary -- userFirstName :: Maybe Text
    <*> arbitrary -- userLastName :: Maybe Text
    <*> arbitrary -- userEmail :: Maybe Text
    <*> arbitrary -- userPassword :: Maybe Text
    <*> arbitrary -- userPhone :: Maybe Text
    <*> arbitrary -- userUserStatus :: Maybe Int
    


