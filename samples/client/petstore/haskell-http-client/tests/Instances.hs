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
    <$> arbitraryReducedMaybe n -- additionalPropertiesAnyTypeName :: Maybe 
  
instance Arbitrary AdditionalPropertiesArray where
  arbitrary = sized genAdditionalPropertiesArray

genAdditionalPropertiesArray :: Int -> Gen AdditionalPropertiesArray
genAdditionalPropertiesArray n =
  AdditionalPropertiesArray
    <$> arbitraryReducedMaybe n -- additionalPropertiesArrayName :: Maybe 
  
instance Arbitrary AdditionalPropertiesBoolean where
  arbitrary = sized genAdditionalPropertiesBoolean

genAdditionalPropertiesBoolean :: Int -> Gen AdditionalPropertiesBoolean
genAdditionalPropertiesBoolean n =
  AdditionalPropertiesBoolean
    <$> arbitraryReducedMaybe n -- additionalPropertiesBooleanName :: Maybe 
  
instance Arbitrary AdditionalPropertiesClass where
  arbitrary = sized genAdditionalPropertiesClass

genAdditionalPropertiesClass :: Int -> Gen AdditionalPropertiesClass
genAdditionalPropertiesClass n =
  AdditionalPropertiesClass
    <$> arbitraryReducedMaybe n -- additionalPropertiesClassMapString :: Maybe 
    <*> arbitraryReducedMaybe n -- additionalPropertiesClassMapNumber :: Maybe 
    <*> arbitraryReducedMaybe n -- additionalPropertiesClassMapInteger :: Maybe 
    <*> arbitraryReducedMaybe n -- additionalPropertiesClassMapBoolean :: Maybe 
    <*> arbitraryReducedMaybe n -- additionalPropertiesClassMapArrayInteger :: Maybe 
    <*> arbitraryReducedMaybe n -- additionalPropertiesClassMapArrayAnytype :: Maybe 
    <*> arbitraryReducedMaybe n -- additionalPropertiesClassMapMapString :: Maybe 
    <*> arbitraryReducedMaybe n -- additionalPropertiesClassMapMapAnytype :: Maybe 
    <*> arbitraryReducedMaybe n -- additionalPropertiesClassAnytype1 :: Maybe 
    <*> arbitraryReducedMaybe n -- additionalPropertiesClassAnytype2 :: Maybe 
    <*> arbitraryReducedMaybe n -- additionalPropertiesClassAnytype3 :: Maybe 
  
instance Arbitrary AdditionalPropertiesInteger where
  arbitrary = sized genAdditionalPropertiesInteger

genAdditionalPropertiesInteger :: Int -> Gen AdditionalPropertiesInteger
genAdditionalPropertiesInteger n =
  AdditionalPropertiesInteger
    <$> arbitraryReducedMaybe n -- additionalPropertiesIntegerName :: Maybe 
  
instance Arbitrary AdditionalPropertiesNumber where
  arbitrary = sized genAdditionalPropertiesNumber

genAdditionalPropertiesNumber :: Int -> Gen AdditionalPropertiesNumber
genAdditionalPropertiesNumber n =
  AdditionalPropertiesNumber
    <$> arbitraryReducedMaybe n -- additionalPropertiesNumberName :: Maybe 
  
instance Arbitrary AdditionalPropertiesObject where
  arbitrary = sized genAdditionalPropertiesObject

genAdditionalPropertiesObject :: Int -> Gen AdditionalPropertiesObject
genAdditionalPropertiesObject n =
  AdditionalPropertiesObject
    <$> arbitraryReducedMaybe n -- additionalPropertiesObjectName :: Maybe 
  
instance Arbitrary AdditionalPropertiesString where
  arbitrary = sized genAdditionalPropertiesString

genAdditionalPropertiesString :: Int -> Gen AdditionalPropertiesString
genAdditionalPropertiesString n =
  AdditionalPropertiesString
    <$> arbitraryReducedMaybe n -- additionalPropertiesStringName :: Maybe 
  
instance Arbitrary Animal where
  arbitrary = sized genAnimal

genAnimal :: Int -> Gen Animal
genAnimal n =
  Animal
    <$> arbitrary -- animalClassName :: 
    <*> arbitraryReducedMaybe n -- animalColor :: Maybe 
  
instance Arbitrary ApiResponse where
  arbitrary = sized genApiResponse

genApiResponse :: Int -> Gen ApiResponse
genApiResponse n =
  ApiResponse
    <$> arbitraryReducedMaybe n -- apiResponseCode :: Maybe 
    <*> arbitraryReducedMaybe n -- apiResponseType :: Maybe 
    <*> arbitraryReducedMaybe n -- apiResponseMessage :: Maybe 
  
instance Arbitrary ArrayOfArrayOfNumberOnly where
  arbitrary = sized genArrayOfArrayOfNumberOnly

genArrayOfArrayOfNumberOnly :: Int -> Gen ArrayOfArrayOfNumberOnly
genArrayOfArrayOfNumberOnly n =
  ArrayOfArrayOfNumberOnly
    <$> arbitraryReducedMaybe n -- arrayOfArrayOfNumberOnlyArrayArrayNumber :: Maybe 
  
instance Arbitrary ArrayOfNumberOnly where
  arbitrary = sized genArrayOfNumberOnly

genArrayOfNumberOnly :: Int -> Gen ArrayOfNumberOnly
genArrayOfNumberOnly n =
  ArrayOfNumberOnly
    <$> arbitraryReducedMaybe n -- arrayOfNumberOnlyArrayNumber :: Maybe 
  
instance Arbitrary ArrayTest where
  arbitrary = sized genArrayTest

genArrayTest :: Int -> Gen ArrayTest
genArrayTest n =
  ArrayTest
    <$> arbitraryReducedMaybe n -- arrayTestArrayOfString :: Maybe 
    <*> arbitraryReducedMaybe n -- arrayTestArrayArrayOfInteger :: Maybe 
    <*> arbitraryReducedMaybe n -- arrayTestArrayArrayOfModel :: Maybe 
  
instance Arbitrary BigCat where
  arbitrary = sized genBigCat

genBigCat :: Int -> Gen BigCat
genBigCat n =
  BigCat
    <$> arbitrary -- bigCatClassName :: 
    <*> arbitraryReducedMaybe n -- bigCatColor :: Maybe 
    <*> arbitraryReducedMaybe n -- bigCatDeclawed :: Maybe 
    <*> arbitraryReducedMaybe n -- bigCatKind :: Maybe 
  
instance Arbitrary BigCatAllOf where
  arbitrary = sized genBigCatAllOf

genBigCatAllOf :: Int -> Gen BigCatAllOf
genBigCatAllOf n =
  BigCatAllOf
    <$> arbitraryReducedMaybe n -- bigCatAllOfKind :: Maybe 
  
instance Arbitrary Capitalization where
  arbitrary = sized genCapitalization

genCapitalization :: Int -> Gen Capitalization
genCapitalization n =
  Capitalization
    <$> arbitraryReducedMaybe n -- capitalizationSmallCamel :: Maybe 
    <*> arbitraryReducedMaybe n -- capitalizationCapitalCamel :: Maybe 
    <*> arbitraryReducedMaybe n -- capitalizationSmallSnake :: Maybe 
    <*> arbitraryReducedMaybe n -- capitalizationCapitalSnake :: Maybe 
    <*> arbitraryReducedMaybe n -- capitalizationScaEthFlowPoints :: Maybe 
    <*> arbitraryReducedMaybe n -- capitalizationAttName :: Maybe 
  
instance Arbitrary Cat where
  arbitrary = sized genCat

genCat :: Int -> Gen Cat
genCat n =
  Cat
    <$> arbitrary -- catClassName :: 
    <*> arbitraryReducedMaybe n -- catColor :: Maybe 
    <*> arbitraryReducedMaybe n -- catDeclawed :: Maybe 
  
instance Arbitrary CatAllOf where
  arbitrary = sized genCatAllOf

genCatAllOf :: Int -> Gen CatAllOf
genCatAllOf n =
  CatAllOf
    <$> arbitraryReducedMaybe n -- catAllOfDeclawed :: Maybe 
  
instance Arbitrary Category where
  arbitrary = sized genCategory

genCategory :: Int -> Gen Category
genCategory n =
  Category
    <$> arbitraryReducedMaybe n -- categoryId :: Maybe 
    <*> arbitrary -- categoryName :: 
  
instance Arbitrary ClassModel where
  arbitrary = sized genClassModel

genClassModel :: Int -> Gen ClassModel
genClassModel n =
  ClassModel
    <$> arbitraryReducedMaybe n -- classModelClass :: Maybe 
  
instance Arbitrary Client where
  arbitrary = sized genClient

genClient :: Int -> Gen Client
genClient n =
  Client
    <$> arbitraryReducedMaybe n -- clientClient :: Maybe 
  
instance Arbitrary Dog where
  arbitrary = sized genDog

genDog :: Int -> Gen Dog
genDog n =
  Dog
    <$> arbitrary -- dogClassName :: 
    <*> arbitraryReducedMaybe n -- dogColor :: Maybe 
    <*> arbitraryReducedMaybe n -- dogBreed :: Maybe 
  
instance Arbitrary DogAllOf where
  arbitrary = sized genDogAllOf

genDogAllOf :: Int -> Gen DogAllOf
genDogAllOf n =
  DogAllOf
    <$> arbitraryReducedMaybe n -- dogAllOfBreed :: Maybe 
  
instance Arbitrary EnumArrays where
  arbitrary = sized genEnumArrays

genEnumArrays :: Int -> Gen EnumArrays
genEnumArrays n =
  EnumArrays
    <$> arbitraryReducedMaybe n -- enumArraysJustSymbol :: Maybe 
    <*> arbitraryReducedMaybe n -- enumArraysArrayEnum :: Maybe 
  
instance Arbitrary EnumTest where
  arbitrary = sized genEnumTest

genEnumTest :: Int -> Gen EnumTest
genEnumTest n =
  EnumTest
    <$> arbitraryReducedMaybe n -- enumTestEnumString :: Maybe 
    <*> arbitrary -- enumTestEnumStringRequired :: 
    <*> arbitraryReducedMaybe n -- enumTestEnumInteger :: Maybe 
    <*> arbitraryReducedMaybe n -- enumTestEnumNumber :: Maybe 
    <*> arbitraryReducedMaybe n -- enumTestOuterEnum :: Maybe 
  
instance Arbitrary File where
  arbitrary = sized genFile

genFile :: Int -> Gen File
genFile n =
  File
    <$> arbitraryReducedMaybe n -- fileSourceUri :: Maybe 
  
instance Arbitrary FileSchemaTestClass where
  arbitrary = sized genFileSchemaTestClass

genFileSchemaTestClass :: Int -> Gen FileSchemaTestClass
genFileSchemaTestClass n =
  FileSchemaTestClass
    <$> arbitraryReducedMaybe n -- fileSchemaTestClassFile :: Maybe 
    <*> arbitraryReducedMaybe n -- fileSchemaTestClassFiles :: Maybe 
  
instance Arbitrary FormatTest where
  arbitrary = sized genFormatTest

genFormatTest :: Int -> Gen FormatTest
genFormatTest n =
  FormatTest
    <$> arbitraryReducedMaybe n -- formatTestInteger :: Maybe 
    <*> arbitraryReducedMaybe n -- formatTestInt32 :: Maybe 
    <*> arbitraryReducedMaybe n -- formatTestInt64 :: Maybe 
    <*> arbitrary -- formatTestNumber :: 
    <*> arbitraryReducedMaybe n -- formatTestFloat :: Maybe 
    <*> arbitraryReducedMaybe n -- formatTestDouble :: Maybe 
    <*> arbitraryReducedMaybe n -- formatTestString :: Maybe 
    <*> arbitraryReduced n -- formatTestByte :: 
    <*> arbitraryReducedMaybe n -- formatTestBinary :: Maybe 
    <*> arbitraryReduced n -- formatTestDate :: 
    <*> arbitraryReducedMaybe n -- formatTestDateTime :: Maybe 
    <*> arbitraryReducedMaybe n -- formatTestUuid :: Maybe 
    <*> arbitrary -- formatTestPassword :: 
    <*> arbitraryReducedMaybe n -- formatTestBigDecimal :: Maybe 
  
instance Arbitrary HasOnlyReadOnly where
  arbitrary = sized genHasOnlyReadOnly

genHasOnlyReadOnly :: Int -> Gen HasOnlyReadOnly
genHasOnlyReadOnly n =
  HasOnlyReadOnly
    <$> arbitraryReducedMaybe n -- hasOnlyReadOnlyBar :: Maybe 
    <*> arbitraryReducedMaybe n -- hasOnlyReadOnlyFoo :: Maybe 
  
instance Arbitrary MapTest where
  arbitrary = sized genMapTest

genMapTest :: Int -> Gen MapTest
genMapTest n =
  MapTest
    <$> arbitraryReducedMaybe n -- mapTestMapMapOfString :: Maybe 
    <*> arbitraryReducedMaybe n -- mapTestMapOfEnumString :: Maybe 
    <*> arbitraryReducedMaybe n -- mapTestDirectMap :: Maybe 
    <*> arbitraryReducedMaybe n -- mapTestIndirectMap :: Maybe 
  
instance Arbitrary MixedPropertiesAndAdditionalPropertiesClass where
  arbitrary = sized genMixedPropertiesAndAdditionalPropertiesClass

genMixedPropertiesAndAdditionalPropertiesClass :: Int -> Gen MixedPropertiesAndAdditionalPropertiesClass
genMixedPropertiesAndAdditionalPropertiesClass n =
  MixedPropertiesAndAdditionalPropertiesClass
    <$> arbitraryReducedMaybe n -- mixedPropertiesAndAdditionalPropertiesClassUuid :: Maybe 
    <*> arbitraryReducedMaybe n -- mixedPropertiesAndAdditionalPropertiesClassDateTime :: Maybe 
    <*> arbitraryReducedMaybe n -- mixedPropertiesAndAdditionalPropertiesClassMap :: Maybe 
  
instance Arbitrary Model200Response where
  arbitrary = sized genModel200Response

genModel200Response :: Int -> Gen Model200Response
genModel200Response n =
  Model200Response
    <$> arbitraryReducedMaybe n -- model200ResponseName :: Maybe 
    <*> arbitraryReducedMaybe n -- model200ResponseClass :: Maybe 
  
instance Arbitrary ModelList where
  arbitrary = sized genModelList

genModelList :: Int -> Gen ModelList
genModelList n =
  ModelList
    <$> arbitraryReducedMaybe n -- modelList123list :: Maybe 
  
instance Arbitrary ModelReturn where
  arbitrary = sized genModelReturn

genModelReturn :: Int -> Gen ModelReturn
genModelReturn n =
  ModelReturn
    <$> arbitraryReducedMaybe n -- modelReturnReturn :: Maybe 
  
instance Arbitrary Name where
  arbitrary = sized genName

genName :: Int -> Gen Name
genName n =
  Name
    <$> arbitrary -- nameName :: 
    <*> arbitraryReducedMaybe n -- nameSnakeCase :: Maybe 
    <*> arbitraryReducedMaybe n -- nameProperty :: Maybe 
    <*> arbitraryReducedMaybe n -- name123number :: Maybe 
  
instance Arbitrary NumberOnly where
  arbitrary = sized genNumberOnly

genNumberOnly :: Int -> Gen NumberOnly
genNumberOnly n =
  NumberOnly
    <$> arbitraryReducedMaybe n -- numberOnlyJustNumber :: Maybe 
  
instance Arbitrary Order where
  arbitrary = sized genOrder

genOrder :: Int -> Gen Order
genOrder n =
  Order
    <$> arbitraryReducedMaybe n -- orderId :: Maybe 
    <*> arbitraryReducedMaybe n -- orderPetId :: Maybe 
    <*> arbitraryReducedMaybe n -- orderQuantity :: Maybe 
    <*> arbitraryReducedMaybe n -- orderShipDate :: Maybe 
    <*> arbitraryReducedMaybe n -- orderStatus :: Maybe 
    <*> arbitraryReducedMaybe n -- orderComplete :: Maybe 
  
instance Arbitrary OuterComposite where
  arbitrary = sized genOuterComposite

genOuterComposite :: Int -> Gen OuterComposite
genOuterComposite n =
  OuterComposite
    <$> arbitraryReducedMaybe n -- outerCompositeMyNumber :: Maybe 
    <*> arbitraryReducedMaybe n -- outerCompositeMyString :: Maybe 
    <*> arbitraryReducedMaybe n -- outerCompositeMyBoolean :: Maybe 
  
instance Arbitrary Pet where
  arbitrary = sized genPet

genPet :: Int -> Gen Pet
genPet n =
  Pet
    <$> arbitraryReducedMaybe n -- petId :: Maybe 
    <*> arbitraryReducedMaybe n -- petCategory :: Maybe 
    <*> arbitrary -- petName :: 
    <*> arbitrary -- petPhotoUrls :: 
    <*> arbitraryReducedMaybe n -- petTags :: Maybe 
    <*> arbitraryReducedMaybe n -- petStatus :: Maybe 
  
instance Arbitrary ReadOnlyFirst where
  arbitrary = sized genReadOnlyFirst

genReadOnlyFirst :: Int -> Gen ReadOnlyFirst
genReadOnlyFirst n =
  ReadOnlyFirst
    <$> arbitraryReducedMaybe n -- readOnlyFirstBar :: Maybe 
    <*> arbitraryReducedMaybe n -- readOnlyFirstBaz :: Maybe 
  
instance Arbitrary SpecialModelName where
  arbitrary = sized genSpecialModelName

genSpecialModelName :: Int -> Gen SpecialModelName
genSpecialModelName n =
  SpecialModelName
    <$> arbitraryReducedMaybe n -- specialModelNameSpecialPropertyName :: Maybe 
  
instance Arbitrary Tag where
  arbitrary = sized genTag

genTag :: Int -> Gen Tag
genTag n =
  Tag
    <$> arbitraryReducedMaybe n -- tagId :: Maybe 
    <*> arbitraryReducedMaybe n -- tagName :: Maybe 
  
instance Arbitrary TypeHolderDefault where
  arbitrary = sized genTypeHolderDefault

genTypeHolderDefault :: Int -> Gen TypeHolderDefault
genTypeHolderDefault n =
  TypeHolderDefault
    <$> arbitrary -- typeHolderDefaultStringItem :: 
    <*> arbitrary -- typeHolderDefaultNumberItem :: 
    <*> arbitrary -- typeHolderDefaultIntegerItem :: 
    <*> arbitrary -- typeHolderDefaultBoolItem :: 
    <*> arbitrary -- typeHolderDefaultArrayItem :: 
  
instance Arbitrary TypeHolderExample where
  arbitrary = sized genTypeHolderExample

genTypeHolderExample :: Int -> Gen TypeHolderExample
genTypeHolderExample n =
  TypeHolderExample
    <$> arbitrary -- typeHolderExampleStringItem :: 
    <*> arbitrary -- typeHolderExampleNumberItem :: 
    <*> arbitrary -- typeHolderExampleFloatItem :: 
    <*> arbitrary -- typeHolderExampleIntegerItem :: 
    <*> arbitrary -- typeHolderExampleBoolItem :: 
    <*> arbitrary -- typeHolderExampleArrayItem :: 
  
instance Arbitrary User where
  arbitrary = sized genUser

genUser :: Int -> Gen User
genUser n =
  User
    <$> arbitraryReducedMaybe n -- userId :: Maybe 
    <*> arbitraryReducedMaybe n -- userUsername :: Maybe 
    <*> arbitraryReducedMaybe n -- userFirstName :: Maybe 
    <*> arbitraryReducedMaybe n -- userLastName :: Maybe 
    <*> arbitraryReducedMaybe n -- userEmail :: Maybe 
    <*> arbitraryReducedMaybe n -- userPassword :: Maybe 
    <*> arbitraryReducedMaybe n -- userPhone :: Maybe 
    <*> arbitraryReducedMaybe n -- userUserStatus :: Maybe 
  
instance Arbitrary XmlItem where
  arbitrary = sized genXmlItem

genXmlItem :: Int -> Gen XmlItem
genXmlItem n =
  XmlItem
    <$> arbitraryReducedMaybe n -- xmlItemAttributeString :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemAttributeNumber :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemAttributeInteger :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemAttributeBoolean :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemWrappedArray :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemNameString :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemNameNumber :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemNameInteger :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemNameBoolean :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemNameArray :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemNameWrappedArray :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemPrefixString :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemPrefixNumber :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemPrefixInteger :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemPrefixBoolean :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemPrefixArray :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemPrefixWrappedArray :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemNamespaceString :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemNamespaceNumber :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemNamespaceInteger :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemNamespaceBoolean :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemNamespaceArray :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemNamespaceWrappedArray :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemPrefixNsString :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemPrefixNsNumber :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemPrefixNsInteger :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemPrefixNsBoolean :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemPrefixNsArray :: Maybe 
    <*> arbitraryReducedMaybe n -- xmlItemPrefixNsWrappedArray :: Maybe 
  



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

