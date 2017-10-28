{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Instances where

import SwaggerPetstore.Model
import SwaggerPetstore.Core

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

-- * Models
 
instance Arbitrary AdditionalPropertiesClass where
  arbitrary =
    AdditionalPropertiesClass
      <$> arbitrary -- additionalPropertiesClassMapProperty :: Maybe (Map.Map String Text)
      <*> arbitrary -- additionalPropertiesClassMapOfMapProperty :: Maybe (Map.Map String (Map.Map String Text))
    
instance Arbitrary Animal where
  arbitrary =
    Animal
      <$> arbitrary -- animalClassName :: Text
      <*> arbitrary -- animalColor :: Maybe Text
    
instance Arbitrary AnimalFarm where
  arbitrary =
    
    pure AnimalFarm
     
instance Arbitrary ApiResponse where
  arbitrary =
    ApiResponse
      <$> arbitrary -- apiResponseCode :: Maybe Int
      <*> arbitrary -- apiResponseType :: Maybe Text
      <*> arbitrary -- apiResponseMessage :: Maybe Text
    
instance Arbitrary ArrayOfArrayOfNumberOnly where
  arbitrary =
    ArrayOfArrayOfNumberOnly
      <$> arbitrary -- arrayOfArrayOfNumberOnlyArrayArrayNumber :: Maybe [[Double]]
    
instance Arbitrary ArrayOfNumberOnly where
  arbitrary =
    ArrayOfNumberOnly
      <$> arbitrary -- arrayOfNumberOnlyArrayNumber :: Maybe [Double]
    
instance Arbitrary ArrayTest where
  arbitrary =
    ArrayTest
      <$> arbitrary -- arrayTestArrayOfString :: Maybe [Text]
      <*> arbitrary -- arrayTestArrayArrayOfInteger :: Maybe [[Integer]]
      <*> arbitrary -- arrayTestArrayArrayOfModel :: Maybe [[ReadOnlyFirst]]
    
instance Arbitrary Capitalization where
  arbitrary =
    Capitalization
      <$> arbitrary -- capitalizationSmallCamel :: Maybe Text
      <*> arbitrary -- capitalizationCapitalCamel :: Maybe Text
      <*> arbitrary -- capitalizationSmallSnake :: Maybe Text
      <*> arbitrary -- capitalizationCapitalSnake :: Maybe Text
      <*> arbitrary -- capitalizationScaEthFlowPoints :: Maybe Text
      <*> arbitrary -- capitalizationAttName :: Maybe Text
    
instance Arbitrary Category where
  arbitrary =
    Category
      <$> arbitrary -- categoryId :: Maybe Integer
      <*> arbitrary -- categoryName :: Maybe Text
    
instance Arbitrary ClassModel where
  arbitrary =
    ClassModel
      <$> arbitrary -- classModelClass :: Maybe Text
    
instance Arbitrary Client where
  arbitrary =
    Client
      <$> arbitrary -- clientClient :: Maybe Text
    
instance Arbitrary EnumArrays where
  arbitrary =
    EnumArrays
      <$> arbitrary -- enumArraysJustSymbol :: Maybe Text
      <*> arbitrary -- enumArraysArrayEnum :: Maybe [Text]
    
instance Arbitrary EnumTest where
  arbitrary =
    EnumTest
      <$> arbitrary -- enumTestEnumString :: Maybe Text
      <*> arbitrary -- enumTestEnumInteger :: Maybe Int
      <*> arbitrary -- enumTestEnumNumber :: Maybe Double
      <*> arbitrary -- enumTestOuterEnum :: Maybe OuterEnum
    
instance Arbitrary FormatTest where
  arbitrary =
    FormatTest
      <$> arbitrary -- formatTestInteger :: Maybe Int
      <*> arbitrary -- formatTestInt32 :: Maybe Int
      <*> arbitrary -- formatTestInt64 :: Maybe Integer
      <*> arbitrary -- formatTestNumber :: Double
      <*> arbitrary -- formatTestFloat :: Maybe Float
      <*> arbitrary -- formatTestDouble :: Maybe Double
      <*> arbitrary -- formatTestString :: Maybe Text
      <*> arbitrary -- formatTestByte :: ByteArray
      <*> arbitrary -- formatTestBinary :: Maybe Binary
      <*> arbitrary -- formatTestDate :: Date
      <*> arbitrary -- formatTestDateTime :: Maybe DateTime
      <*> arbitrary -- formatTestUuid :: Maybe Text
      <*> arbitrary -- formatTestPassword :: Text
    
instance Arbitrary HasOnlyReadOnly where
  arbitrary =
    HasOnlyReadOnly
      <$> arbitrary -- hasOnlyReadOnlyBar :: Maybe Text
      <*> arbitrary -- hasOnlyReadOnlyFoo :: Maybe Text
    
instance Arbitrary MapTest where
  arbitrary =
    MapTest
      <$> arbitrary -- mapTestMapMapOfString :: Maybe (Map.Map String (Map.Map String Text))
      <*> arbitrary -- mapTestMapOfEnumString :: Maybe (Map.Map String Text)
    
instance Arbitrary MixedPropertiesAndAdditionalPropertiesClass where
  arbitrary =
    MixedPropertiesAndAdditionalPropertiesClass
      <$> arbitrary -- mixedPropertiesAndAdditionalPropertiesClassUuid :: Maybe Text
      <*> arbitrary -- mixedPropertiesAndAdditionalPropertiesClassDateTime :: Maybe DateTime
      <*> arbitrary -- mixedPropertiesAndAdditionalPropertiesClassMap :: Maybe (Map.Map String Animal)
    
instance Arbitrary Model200Response where
  arbitrary =
    Model200Response
      <$> arbitrary -- model200ResponseName :: Maybe Int
      <*> arbitrary -- model200ResponseClass :: Maybe Text
    
instance Arbitrary ModelList where
  arbitrary =
    ModelList
      <$> arbitrary -- modelList123List :: Maybe Text
    
instance Arbitrary ModelReturn where
  arbitrary =
    ModelReturn
      <$> arbitrary -- modelReturnReturn :: Maybe Int
    
instance Arbitrary Name where
  arbitrary =
    Name
      <$> arbitrary -- nameName :: Int
      <*> arbitrary -- nameSnakeCase :: Maybe Int
      <*> arbitrary -- nameProperty :: Maybe Text
      <*> arbitrary -- name123Number :: Maybe Int
    
instance Arbitrary NumberOnly where
  arbitrary =
    NumberOnly
      <$> arbitrary -- numberOnlyJustNumber :: Maybe Double
    
instance Arbitrary Order where
  arbitrary =
    Order
      <$> arbitrary -- orderId :: Maybe Integer
      <*> arbitrary -- orderPetId :: Maybe Integer
      <*> arbitrary -- orderQuantity :: Maybe Int
      <*> arbitrary -- orderShipDate :: Maybe DateTime
      <*> arbitrary -- orderStatus :: Maybe Text
      <*> arbitrary -- orderComplete :: Maybe Bool
    
instance Arbitrary OuterBoolean where
  arbitrary =
    OuterBoolean <$> arbitrary
instance Arbitrary OuterComposite where
  arbitrary =
    OuterComposite
      <$> arbitrary -- outerCompositeMyNumber :: Maybe OuterNumber
      <*> arbitrary -- outerCompositeMyString :: Maybe OuterString
      <*> arbitrary -- outerCompositeMyBoolean :: Maybe OuterBoolean
    
instance Arbitrary OuterNumber where
  arbitrary =
    OuterNumber <$> arbitrary
instance Arbitrary OuterString where
  arbitrary =
    OuterString <$> arbitrary
instance Arbitrary Pet where
  arbitrary =
    Pet
      <$> arbitrary -- petId :: Maybe Integer
      <*> arbitrary -- petCategory :: Maybe Category
      <*> arbitrary -- petName :: Text
      <*> arbitrary -- petPhotoUrls :: [Text]
      <*> arbitrary -- petTags :: Maybe [Tag]
      <*> arbitrary -- petStatus :: Maybe Text
    
instance Arbitrary ReadOnlyFirst where
  arbitrary =
    ReadOnlyFirst
      <$> arbitrary -- readOnlyFirstBar :: Maybe Text
      <*> arbitrary -- readOnlyFirstBaz :: Maybe Text
    
instance Arbitrary SpecialModelName where
  arbitrary =
    SpecialModelName
      <$> arbitrary -- specialModelNameSpecialPropertyName :: Maybe Integer
    
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
    
instance Arbitrary Cat where
  arbitrary =
    Cat
      <$> arbitrary -- catClassName :: Text
      <*> arbitrary -- catColor :: Maybe Text
      <*> arbitrary -- catDeclawed :: Maybe Bool
    
instance Arbitrary Dog where
  arbitrary =
    Dog
      <$> arbitrary -- dogClassName :: Text
      <*> arbitrary -- dogColor :: Maybe Text
      <*> arbitrary -- dogBreed :: Maybe Text
    



instance Arbitrary E'ArrayEnum where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'EnumFormString where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'EnumInteger where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'EnumNumber where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'EnumQueryInteger where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'EnumString where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'JustSymbol where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary EnumClass where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OuterEnum where
  arbitrary = arbitraryBoundedEnum
