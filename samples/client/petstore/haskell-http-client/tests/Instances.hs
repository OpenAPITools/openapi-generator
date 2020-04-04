{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import OpenAPIPetstore.Model
import OpenAPIPetstore.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
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

instance ApproxEq TI.Day where
  (=~) = (==)
    
arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models
 
instance Arbitrary ApiResponse where
  arbitrary = sized genApiResponse

genApiResponse :: Int -> Gen ApiResponse
genApiResponse n =
  ApiResponse
    <$> arbitraryReducedMaybe n -- apiResponseCode :: Maybe Int
    <*> arbitraryReducedMaybe n -- apiResponseType :: Maybe Text
    <*> arbitraryReducedMaybe n -- apiResponseMessage :: Maybe Text
  
instance Arbitrary Category where
  arbitrary = sized genCategory

genCategory :: Int -> Gen Category
genCategory n =
  Category
    <$> arbitraryReducedMaybe n -- categoryId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- categoryName :: Maybe Text
  
instance Arbitrary Order where
  arbitrary = sized genOrder

genOrder :: Int -> Gen Order
genOrder n =
  Order
    <$> arbitraryReducedMaybe n -- orderId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- orderPetId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- orderQuantity :: Maybe Int
    <*> arbitraryReducedMaybe n -- orderShipDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- orderStatus :: Maybe E'Status
    <*> arbitraryReducedMaybe n -- orderComplete :: Maybe Bool
  
instance Arbitrary Pet where
  arbitrary = sized genPet

genPet :: Int -> Gen Pet
genPet n =
  Pet
    <$> arbitraryReducedMaybe n -- petId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- petCategory :: Maybe Category
    <*> arbitrary -- petName :: Text
    <*> arbitrary -- petPhotoUrls :: [Text]
    <*> arbitraryReducedMaybe n -- petTags :: Maybe [Tag]
    <*> arbitraryReducedMaybe n -- petStatus :: Maybe E'Status2
  
instance Arbitrary Tag where
  arbitrary = sized genTag

genTag :: Int -> Gen Tag
genTag n =
  Tag
    <$> arbitraryReducedMaybe n -- tagId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- tagName :: Maybe Text
  
instance Arbitrary User where
  arbitrary = sized genUser

genUser :: Int -> Gen User
genUser n =
  User
    <$> arbitraryReducedMaybe n -- userId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- userUsername :: Maybe Text
    <*> arbitraryReducedMaybe n -- userFirstName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userLastName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- userPassword :: Maybe Text
    <*> arbitraryReducedMaybe n -- userPhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- userUserStatus :: Maybe Int
  



instance Arbitrary E'Status where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status2 where
  arbitrary = arbitraryBoundedEnum

