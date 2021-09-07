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

instance Arbitrary AdditionalPropertiesAnyType where
  arbitrary = sized genAdditionalPropertiesAnyType

genAdditionalPropertiesAnyType :: Int -> Gen AdditionalPropertiesAnyType
genAdditionalPropertiesAnyType n =
  AdditionalPropertiesAnyType
    <$> arbitraryReducedMaybe n -- additionalPropertiesAnyTypeName :: Maybe Text
  
instance Arbitrary AdditionalPropertiesArray where
  arbitrary = sized genAdditionalPropertiesArray

genAdditionalPropertiesArray :: Int -> Gen AdditionalPropertiesArray
genAdditionalPropertiesArray n =
  AdditionalPropertiesArray
    <$> arbitraryReducedMaybe n -- additionalPropertiesArrayName :: Maybe Text
  
instance Arbitrary AdditionalPropertiesBoolean where
  arbitrary = sized genAdditionalPropertiesBoolean

genAdditionalPropertiesBoolean :: Int -> Gen AdditionalPropertiesBoolean
genAdditionalPropertiesBoolean n =
  AdditionalPropertiesBoolean
    <$> arbitraryReducedMaybe n -- additionalPropertiesBooleanName :: Maybe Text
  
instance Arbitrary AdditionalPropertiesClass where
  arbitrary = sized genAdditionalPropertiesClass

genAdditionalPropertiesClass :: Int -> Gen AdditionalPropertiesClass
genAdditionalPropertiesClass n =
  AdditionalPropertiesClass
    <$> arbitraryReducedMaybe n -- additionalPropertiesClassMapString :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- additionalPropertiesClassMapNumber :: Maybe (Map.Map String Double)
    <*> arbitraryReducedMaybe n -- additionalPropertiesClassMapInteger :: Maybe (Map.Map String Int)
    <*> arbitraryReducedMaybe n -- additionalPropertiesClassMapBoolean :: Maybe (Map.Map String Bool)
    <*> arbitraryReducedMaybe n -- additionalPropertiesClassMapArrayInteger :: Maybe (Map.Map String [Int])
    <*> arbitraryReducedMaybe n -- additionalPropertiesClassMapArrayAnytype :: Maybe (Map.Map String [A.Value])
    <*> arbitraryReducedMaybe n -- additionalPropertiesClassMapMapString :: Maybe (Map.Map String (Map.Map String Text))
    <*> arbitraryReducedMaybe n -- additionalPropertiesClassMapMapAnytype :: Maybe (Map.Map String (Map.Map String A.Value))
    <*> arbitraryReducedMaybeValue n -- additionalPropertiesClassAnytype1 :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- additionalPropertiesClassAnytype2 :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- additionalPropertiesClassAnytype3 :: Maybe A.Value
  
instance Arbitrary AdditionalPropertiesInteger where
  arbitrary = sized genAdditionalPropertiesInteger

genAdditionalPropertiesInteger :: Int -> Gen AdditionalPropertiesInteger
genAdditionalPropertiesInteger n =
  AdditionalPropertiesInteger
    <$> arbitraryReducedMaybe n -- additionalPropertiesIntegerName :: Maybe Text
  
instance Arbitrary AdditionalPropertiesNumber where
  arbitrary = sized genAdditionalPropertiesNumber

genAdditionalPropertiesNumber :: Int -> Gen AdditionalPropertiesNumber
genAdditionalPropertiesNumber n =
  AdditionalPropertiesNumber
    <$> arbitraryReducedMaybe n -- additionalPropertiesNumberName :: Maybe Text
  
instance Arbitrary AdditionalPropertiesObject where
  arbitrary = sized genAdditionalPropertiesObject

genAdditionalPropertiesObject :: Int -> Gen AdditionalPropertiesObject
genAdditionalPropertiesObject n =
  AdditionalPropertiesObject
    <$> arbitraryReducedMaybe n -- additionalPropertiesObjectName :: Maybe Text
  
instance Arbitrary AdditionalPropertiesString where
  arbitrary = sized genAdditionalPropertiesString

genAdditionalPropertiesString :: Int -> Gen AdditionalPropertiesString
genAdditionalPropertiesString n =
  AdditionalPropertiesString
    <$> arbitraryReducedMaybe n -- additionalPropertiesStringName :: Maybe Text
  
instance Arbitrary Animal where
  arbitrary = sized genAnimal

genAnimal :: Int -> Gen Animal
genAnimal n =
  Animal
    <$> arbitrary -- animalClassName :: Text
    <*> arbitraryReducedMaybe n -- animalColor :: Maybe Text
  
instance Arbitrary ApiResponse where
  arbitrary = sized genApiResponse

genApiResponse :: Int -> Gen ApiResponse
genApiResponse n =
  ApiResponse
    <$> arbitraryReducedMaybe n -- apiResponseCode :: Maybe Int
    <*> arbitraryReducedMaybe n -- apiResponseType :: Maybe Text
    <*> arbitraryReducedMaybe n -- apiResponseMessage :: Maybe Text
  
instance Arbitrary ArrayOfArrayOfNumberOnly where
  arbitrary = sized genArrayOfArrayOfNumberOnly

genArrayOfArrayOfNumberOnly :: Int -> Gen ArrayOfArrayOfNumberOnly
genArrayOfArrayOfNumberOnly n =
  ArrayOfArrayOfNumberOnly
    <$> arbitraryReducedMaybe n -- arrayOfArrayOfNumberOnlyArrayArrayNumber :: Maybe [[Double]]
  
instance Arbitrary ArrayOfNumberOnly where
  arbitrary = sized genArrayOfNumberOnly

genArrayOfNumberOnly :: Int -> Gen ArrayOfNumberOnly
genArrayOfNumberOnly n =
  ArrayOfNumberOnly
    <$> arbitraryReducedMaybe n -- arrayOfNumberOnlyArrayNumber :: Maybe [Double]
  
instance Arbitrary ArrayTest where
  arbitrary = sized genArrayTest

genArrayTest :: Int -> Gen ArrayTest
genArrayTest n =
  ArrayTest
    <$> arbitraryReducedMaybe n -- arrayTestArrayOfString :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- arrayTestArrayArrayOfInteger :: Maybe [[Integer]]
    <*> arbitraryReducedMaybe n -- arrayTestArrayArrayOfModel :: Maybe [[ReadOnlyFirst]]
  
instance Arbitrary BigCat where
  arbitrary = sized genBigCat

genBigCat :: Int -> Gen BigCat
genBigCat n =
  BigCat
    <$> arbitrary -- bigCatClassName :: Text
    <*> arbitraryReducedMaybe n -- bigCatColor :: Maybe Text
    <*> arbitraryReducedMaybe n -- bigCatDeclawed :: Maybe Bool
    <*> arbitraryReducedMaybe n -- bigCatKind :: Maybe E'Kind
  
instance Arbitrary BigCatAllOf where
  arbitrary = sized genBigCatAllOf

genBigCatAllOf :: Int -> Gen BigCatAllOf
genBigCatAllOf n =
  BigCatAllOf
    <$> arbitraryReducedMaybe n -- bigCatAllOfKind :: Maybe E'Kind
  
instance Arbitrary Capitalization where
  arbitrary = sized genCapitalization

genCapitalization :: Int -> Gen Capitalization
genCapitalization n =
  Capitalization
    <$> arbitraryReducedMaybe n -- capitalizationSmallCamel :: Maybe Text
    <*> arbitraryReducedMaybe n -- capitalizationCapitalCamel :: Maybe Text
    <*> arbitraryReducedMaybe n -- capitalizationSmallSnake :: Maybe Text
    <*> arbitraryReducedMaybe n -- capitalizationCapitalSnake :: Maybe Text
    <*> arbitraryReducedMaybe n -- capitalizationScaEthFlowPoints :: Maybe Text
    <*> arbitraryReducedMaybe n -- capitalizationAttName :: Maybe Text
  
instance Arbitrary Cat where
  arbitrary = sized genCat

genCat :: Int -> Gen Cat
genCat n =
  Cat
    <$> arbitrary -- catClassName :: Text
    <*> arbitraryReducedMaybe n -- catColor :: Maybe Text
    <*> arbitraryReducedMaybe n -- catDeclawed :: Maybe Bool
  
instance Arbitrary CatAllOf where
  arbitrary = sized genCatAllOf

genCatAllOf :: Int -> Gen CatAllOf
genCatAllOf n =
  CatAllOf
    <$> arbitraryReducedMaybe n -- catAllOfDeclawed :: Maybe Bool
  
instance Arbitrary Category where
  arbitrary = sized genCategory

genCategory :: Int -> Gen Category
genCategory n =
  Category
    <$> arbitraryReducedMaybe n -- categoryId :: Maybe Integer
    <*> arbitrary -- categoryName :: Text
  
instance Arbitrary ClassModel where
  arbitrary = sized genClassModel

genClassModel :: Int -> Gen ClassModel
genClassModel n =
  ClassModel
    <$> arbitraryReducedMaybe n -- classModelClass :: Maybe Text
  
instance Arbitrary Client where
  arbitrary = sized genClient

genClient :: Int -> Gen Client
genClient n =
  Client
    <$> arbitraryReducedMaybe n -- clientClient :: Maybe Text
  
instance Arbitrary Dog where
  arbitrary = sized genDog

genDog :: Int -> Gen Dog
genDog n =
  Dog
    <$> arbitrary -- dogClassName :: Text
    <*> arbitraryReducedMaybe n -- dogColor :: Maybe Text
    <*> arbitraryReducedMaybe n -- dogBreed :: Maybe Text
  
instance Arbitrary DogAllOf where
  arbitrary = sized genDogAllOf

genDogAllOf :: Int -> Gen DogAllOf
genDogAllOf n =
  DogAllOf
    <$> arbitraryReducedMaybe n -- dogAllOfBreed :: Maybe Text
  
instance Arbitrary EnumArrays where
  arbitrary = sized genEnumArrays

genEnumArrays :: Int -> Gen EnumArrays
genEnumArrays n =
  EnumArrays
    <$> arbitraryReducedMaybe n -- enumArraysJustSymbol :: Maybe E'JustSymbol
    <*> arbitraryReducedMaybe n -- enumArraysArrayEnum :: Maybe [E'ArrayEnum]
  
instance Arbitrary EnumTest where
  arbitrary = sized genEnumTest

genEnumTest :: Int -> Gen EnumTest
genEnumTest n =
  EnumTest
    <$> arbitraryReducedMaybe n -- enumTestEnumString :: Maybe E'EnumString
    <*> arbitrary -- enumTestEnumStringRequired :: E'EnumString
    <*> arbitraryReducedMaybe n -- enumTestEnumInteger :: Maybe E'EnumInteger
    <*> arbitraryReducedMaybe n -- enumTestEnumNumber :: Maybe E'EnumNumber
    <*> arbitraryReducedMaybe n -- enumTestOuterEnum :: Maybe OuterEnum
  
instance Arbitrary File where
  arbitrary = sized genFile

genFile :: Int -> Gen File
genFile n =
  File
    <$> arbitraryReducedMaybe n -- fileSourceUri :: Maybe Text
  
instance Arbitrary FileSchemaTestClass where
  arbitrary = sized genFileSchemaTestClass

genFileSchemaTestClass :: Int -> Gen FileSchemaTestClass
genFileSchemaTestClass n =
  FileSchemaTestClass
    <$> arbitraryReducedMaybe n -- fileSchemaTestClassFile :: Maybe File
    <*> arbitraryReducedMaybe n -- fileSchemaTestClassFiles :: Maybe [File]
  
instance Arbitrary FormatTest where
  arbitrary = sized genFormatTest

genFormatTest :: Int -> Gen FormatTest
genFormatTest n =
  FormatTest
    <$> arbitraryReducedMaybe n -- formatTestInteger :: Maybe Int
    <*> arbitraryReducedMaybe n -- formatTestInt32 :: Maybe Int
    <*> arbitraryReducedMaybe n -- formatTestInt64 :: Maybe Integer
    <*> arbitrary -- formatTestNumber :: Double
    <*> arbitraryReducedMaybe n -- formatTestFloat :: Maybe Float
    <*> arbitraryReducedMaybe n -- formatTestDouble :: Maybe Double
    <*> arbitraryReducedMaybe n -- formatTestString :: Maybe Text
    <*> arbitraryReduced n -- formatTestByte :: ByteArray
    <*> arbitraryReducedMaybe n -- formatTestBinary :: Maybe FilePath
    <*> arbitraryReduced n -- formatTestDate :: Date
    <*> arbitraryReducedMaybe n -- formatTestDateTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- formatTestUuid :: Maybe Text
    <*> arbitrary -- formatTestPassword :: Text
    <*> arbitraryReducedMaybe n -- formatTestBigDecimal :: Maybe Double
  
instance Arbitrary HasOnlyReadOnly where
  arbitrary = sized genHasOnlyReadOnly

genHasOnlyReadOnly :: Int -> Gen HasOnlyReadOnly
genHasOnlyReadOnly n =
  HasOnlyReadOnly
    <$> arbitraryReducedMaybe n -- hasOnlyReadOnlyBar :: Maybe Text
    <*> arbitraryReducedMaybe n -- hasOnlyReadOnlyFoo :: Maybe Text
  
instance Arbitrary MapTest where
  arbitrary = sized genMapTest

genMapTest :: Int -> Gen MapTest
genMapTest n =
  MapTest
    <$> arbitraryReducedMaybe n -- mapTestMapMapOfString :: Maybe (Map.Map String (Map.Map String Text))
    <*> arbitraryReducedMaybe n -- mapTestMapOfEnumString :: Maybe (Map.Map String E'Inner)
    <*> arbitraryReducedMaybe n -- mapTestDirectMap :: Maybe (Map.Map String Bool)
    <*> arbitraryReducedMaybe n -- mapTestIndirectMap :: Maybe (Map.Map String Bool)
  
instance Arbitrary MixedPropertiesAndAdditionalPropertiesClass where
  arbitrary = sized genMixedPropertiesAndAdditionalPropertiesClass

genMixedPropertiesAndAdditionalPropertiesClass :: Int -> Gen MixedPropertiesAndAdditionalPropertiesClass
genMixedPropertiesAndAdditionalPropertiesClass n =
  MixedPropertiesAndAdditionalPropertiesClass
    <$> arbitraryReducedMaybe n -- mixedPropertiesAndAdditionalPropertiesClassUuid :: Maybe Text
    <*> arbitraryReducedMaybe n -- mixedPropertiesAndAdditionalPropertiesClassDateTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- mixedPropertiesAndAdditionalPropertiesClassMap :: Maybe (Map.Map String Animal)
  
instance Arbitrary Model200Response where
  arbitrary = sized genModel200Response

genModel200Response :: Int -> Gen Model200Response
genModel200Response n =
  Model200Response
    <$> arbitraryReducedMaybe n -- model200ResponseName :: Maybe Int
    <*> arbitraryReducedMaybe n -- model200ResponseClass :: Maybe Text
  
instance Arbitrary ModelList where
  arbitrary = sized genModelList

genModelList :: Int -> Gen ModelList
genModelList n =
  ModelList
    <$> arbitraryReducedMaybe n -- modelList123list :: Maybe Text
  
instance Arbitrary ModelReturn where
  arbitrary = sized genModelReturn

genModelReturn :: Int -> Gen ModelReturn
genModelReturn n =
  ModelReturn
    <$> arbitraryReducedMaybe n -- modelReturnReturn :: Maybe Int
  
instance Arbitrary Name where
  arbitrary = sized genName

genName :: Int -> Gen Name
genName n =
  Name
    <$> arbitrary -- nameName :: Int
    <*> arbitraryReducedMaybe n -- nameSnakeCase :: Maybe Int
    <*> arbitraryReducedMaybe n -- nameProperty :: Maybe Text
    <*> arbitraryReducedMaybe n -- name123number :: Maybe Int
  
instance Arbitrary NumberOnly where
  arbitrary = sized genNumberOnly

genNumberOnly :: Int -> Gen NumberOnly
genNumberOnly n =
  NumberOnly
    <$> arbitraryReducedMaybe n -- numberOnlyJustNumber :: Maybe Double
  
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
  
instance Arbitrary OuterComposite where
  arbitrary = sized genOuterComposite

genOuterComposite :: Int -> Gen OuterComposite
genOuterComposite n =
  OuterComposite
    <$> arbitraryReducedMaybe n -- outerCompositeMyNumber :: Maybe Double
    <*> arbitraryReducedMaybe n -- outerCompositeMyString :: Maybe Text
    <*> arbitraryReducedMaybe n -- outerCompositeMyBoolean :: Maybe Bool
  
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
  
instance Arbitrary ReadOnlyFirst where
  arbitrary = sized genReadOnlyFirst

genReadOnlyFirst :: Int -> Gen ReadOnlyFirst
genReadOnlyFirst n =
  ReadOnlyFirst
    <$> arbitraryReducedMaybe n -- readOnlyFirstBar :: Maybe Text
    <*> arbitraryReducedMaybe n -- readOnlyFirstBaz :: Maybe Text
  
instance Arbitrary SpecialModelName where
  arbitrary = sized genSpecialModelName

genSpecialModelName :: Int -> Gen SpecialModelName
genSpecialModelName n =
  SpecialModelName
    <$> arbitraryReducedMaybe n -- specialModelNameSpecialPropertyName :: Maybe Integer
  
instance Arbitrary Tag where
  arbitrary = sized genTag

genTag :: Int -> Gen Tag
genTag n =
  Tag
    <$> arbitraryReducedMaybe n -- tagId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- tagName :: Maybe Text
  
instance Arbitrary TypeHolderDefault where
  arbitrary = sized genTypeHolderDefault

genTypeHolderDefault :: Int -> Gen TypeHolderDefault
genTypeHolderDefault n =
  TypeHolderDefault
    <$> arbitrary -- typeHolderDefaultStringItem :: Text
    <*> arbitrary -- typeHolderDefaultNumberItem :: Double
    <*> arbitrary -- typeHolderDefaultIntegerItem :: Int
    <*> arbitrary -- typeHolderDefaultBoolItem :: Bool
    <*> arbitrary -- typeHolderDefaultArrayItem :: [Int]
  
instance Arbitrary TypeHolderExample where
  arbitrary = sized genTypeHolderExample

genTypeHolderExample :: Int -> Gen TypeHolderExample
genTypeHolderExample n =
  TypeHolderExample
    <$> arbitrary -- typeHolderExampleStringItem :: Text
    <*> arbitrary -- typeHolderExampleNumberItem :: Double
    <*> arbitrary -- typeHolderExampleFloatItem :: Float
    <*> arbitrary -- typeHolderExampleIntegerItem :: Int
    <*> arbitrary -- typeHolderExampleBoolItem :: Bool
    <*> arbitrary -- typeHolderExampleArrayItem :: [Int]
  
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
  
instance Arbitrary XmlItem where
  arbitrary = sized genXmlItem

genXmlItem :: Int -> Gen XmlItem
genXmlItem n =
  XmlItem
    <$> arbitraryReducedMaybe n -- xmlItemAttributeString :: Maybe Text
    <*> arbitraryReducedMaybe n -- xmlItemAttributeNumber :: Maybe Double
    <*> arbitraryReducedMaybe n -- xmlItemAttributeInteger :: Maybe Int
    <*> arbitraryReducedMaybe n -- xmlItemAttributeBoolean :: Maybe Bool
    <*> arbitraryReducedMaybe n -- xmlItemWrappedArray :: Maybe [Int]
    <*> arbitraryReducedMaybe n -- xmlItemNameString :: Maybe Text
    <*> arbitraryReducedMaybe n -- xmlItemNameNumber :: Maybe Double
    <*> arbitraryReducedMaybe n -- xmlItemNameInteger :: Maybe Int
    <*> arbitraryReducedMaybe n -- xmlItemNameBoolean :: Maybe Bool
    <*> arbitraryReducedMaybe n -- xmlItemNameArray :: Maybe [Int]
    <*> arbitraryReducedMaybe n -- xmlItemNameWrappedArray :: Maybe [Int]
    <*> arbitraryReducedMaybe n -- xmlItemPrefixString :: Maybe Text
    <*> arbitraryReducedMaybe n -- xmlItemPrefixNumber :: Maybe Double
    <*> arbitraryReducedMaybe n -- xmlItemPrefixInteger :: Maybe Int
    <*> arbitraryReducedMaybe n -- xmlItemPrefixBoolean :: Maybe Bool
    <*> arbitraryReducedMaybe n -- xmlItemPrefixArray :: Maybe [Int]
    <*> arbitraryReducedMaybe n -- xmlItemPrefixWrappedArray :: Maybe [Int]
    <*> arbitraryReducedMaybe n -- xmlItemNamespaceString :: Maybe Text
    <*> arbitraryReducedMaybe n -- xmlItemNamespaceNumber :: Maybe Double
    <*> arbitraryReducedMaybe n -- xmlItemNamespaceInteger :: Maybe Int
    <*> arbitraryReducedMaybe n -- xmlItemNamespaceBoolean :: Maybe Bool
    <*> arbitraryReducedMaybe n -- xmlItemNamespaceArray :: Maybe [Int]
    <*> arbitraryReducedMaybe n -- xmlItemNamespaceWrappedArray :: Maybe [Int]
    <*> arbitraryReducedMaybe n -- xmlItemPrefixNsString :: Maybe Text
    <*> arbitraryReducedMaybe n -- xmlItemPrefixNsNumber :: Maybe Double
    <*> arbitraryReducedMaybe n -- xmlItemPrefixNsInteger :: Maybe Int
    <*> arbitraryReducedMaybe n -- xmlItemPrefixNsBoolean :: Maybe Bool
    <*> arbitraryReducedMaybe n -- xmlItemPrefixNsArray :: Maybe [Int]
    <*> arbitraryReducedMaybe n -- xmlItemPrefixNsWrappedArray :: Maybe [Int]
  



instance Arbitrary E'ArrayEnum where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'EnumFormString where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'EnumFormStringArray where
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

instance Arbitrary E'JustSymbol where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Kind where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary EnumClass where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OuterEnum where
  arbitrary = arbitraryBoundedEnum

