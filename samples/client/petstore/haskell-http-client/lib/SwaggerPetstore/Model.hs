{-|
Module : SwaggerPetstore.Model
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SwaggerPetstore.Model where

import Data.Aeson ((.:),(.:!),(.:?),(.=))

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64.Lazy as BL64
import qualified Data.Data as P (Data, Typeable)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as P
import qualified Data.Foldable as P
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH
import qualified Control.DeepSeq as NF
import qualified Data.Ix as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Control.Arrow as P (left)
import Data.Text (Text)

import qualified Data.Time as TI
import qualified Data.Time.ISO8601 as TI

import Control.Applicative ((<|>))
import Control.Applicative (Alternative)
import Prelude (($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P



-- * Models


-- ** AdditionalPropertiesClass
-- |
data AdditionalPropertiesClass = AdditionalPropertiesClass
  { additionalPropertiesClassMapProperty :: !(Maybe (Map.Map String Text)) -- ^ "map_property"
  , additionalPropertiesClassMapOfMapProperty :: !(Maybe (Map.Map String (Map.Map String Text))) -- ^ "map_of_map_property"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON AdditionalPropertiesClass where
  parseJSON = A.withObject "AdditionalPropertiesClass" $ \o ->
    AdditionalPropertiesClass
      <$> (o .:? "map_property")
      <*> (o .:? "map_of_map_property")

instance A.ToJSON AdditionalPropertiesClass where
  toJSON AdditionalPropertiesClass {..} =
   _omitNulls
      [ "map_property" .= additionalPropertiesClassMapProperty
      , "map_of_map_property" .= additionalPropertiesClassMapOfMapProperty
      ]


-- | Construct a value of type 'AdditionalPropertiesClass' (by applying it's required fields, if any)
mkAdditionalPropertiesClass
  :: AdditionalPropertiesClass
mkAdditionalPropertiesClass =
  AdditionalPropertiesClass
  { additionalPropertiesClassMapProperty = Nothing
  , additionalPropertiesClassMapOfMapProperty = Nothing
  }
  

-- ** Animal
-- |
data Animal = Animal
  { animalClassName :: !(Text) -- ^ /Required/ "className"
  , animalColor :: !(Maybe Text) -- ^ "color"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON Animal where
  parseJSON = A.withObject "Animal" $ \o ->
    Animal
      <$> (o .:  "className")
      <*> (o .:? "color")

instance A.ToJSON Animal where
  toJSON Animal {..} =
   _omitNulls
      [ "className" .= animalClassName
      , "color" .= animalColor
      ]


-- | Construct a value of type 'Animal' (by applying it's required fields, if any)
mkAnimal
  :: Text -- ^ 'animalClassName' 
  -> Animal
mkAnimal animalClassName =
  Animal
  { animalClassName
  , animalColor = Nothing
  }
  

-- ** AnimalFarm
-- |
data AnimalFarm = AnimalFarm
  { 
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON AnimalFarm where
  parseJSON = A.withObject "AnimalFarm" $ \o ->
    pure AnimalFarm
      

instance A.ToJSON AnimalFarm where
  toJSON AnimalFarm  =
   _omitNulls
      [ 
      ]


-- | Construct a value of type 'AnimalFarm' (by applying it's required fields, if any)
mkAnimalFarm
  :: AnimalFarm
mkAnimalFarm =
  AnimalFarm
  { 
  }
  

-- ** ApiResponse
-- |
data ApiResponse = ApiResponse
  { apiResponseCode :: !(Maybe Int) -- ^ "code"
  , apiResponseType :: !(Maybe Text) -- ^ "type"
  , apiResponseMessage :: !(Maybe Text) -- ^ "message"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON ApiResponse where
  parseJSON = A.withObject "ApiResponse" $ \o ->
    ApiResponse
      <$> (o .:? "code")
      <*> (o .:? "type")
      <*> (o .:? "message")

instance A.ToJSON ApiResponse where
  toJSON ApiResponse {..} =
   _omitNulls
      [ "code" .= apiResponseCode
      , "type" .= apiResponseType
      , "message" .= apiResponseMessage
      ]


-- | Construct a value of type 'ApiResponse' (by applying it's required fields, if any)
mkApiResponse
  :: ApiResponse
mkApiResponse =
  ApiResponse
  { apiResponseCode = Nothing
  , apiResponseType = Nothing
  , apiResponseMessage = Nothing
  }
  

-- ** ArrayOfArrayOfNumberOnly
-- |
data ArrayOfArrayOfNumberOnly = ArrayOfArrayOfNumberOnly
  { arrayOfArrayOfNumberOnlyArrayArrayNumber :: !(Maybe [[Double]]) -- ^ "ArrayArrayNumber"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON ArrayOfArrayOfNumberOnly where
  parseJSON = A.withObject "ArrayOfArrayOfNumberOnly" $ \o ->
    ArrayOfArrayOfNumberOnly
      <$> (o .:? "ArrayArrayNumber")

instance A.ToJSON ArrayOfArrayOfNumberOnly where
  toJSON ArrayOfArrayOfNumberOnly {..} =
   _omitNulls
      [ "ArrayArrayNumber" .= arrayOfArrayOfNumberOnlyArrayArrayNumber
      ]


-- | Construct a value of type 'ArrayOfArrayOfNumberOnly' (by applying it's required fields, if any)
mkArrayOfArrayOfNumberOnly
  :: ArrayOfArrayOfNumberOnly
mkArrayOfArrayOfNumberOnly =
  ArrayOfArrayOfNumberOnly
  { arrayOfArrayOfNumberOnlyArrayArrayNumber = Nothing
  }
  

-- ** ArrayOfNumberOnly
-- |
data ArrayOfNumberOnly = ArrayOfNumberOnly
  { arrayOfNumberOnlyArrayNumber :: !(Maybe [Double]) -- ^ "ArrayNumber"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON ArrayOfNumberOnly where
  parseJSON = A.withObject "ArrayOfNumberOnly" $ \o ->
    ArrayOfNumberOnly
      <$> (o .:? "ArrayNumber")

instance A.ToJSON ArrayOfNumberOnly where
  toJSON ArrayOfNumberOnly {..} =
   _omitNulls
      [ "ArrayNumber" .= arrayOfNumberOnlyArrayNumber
      ]


-- | Construct a value of type 'ArrayOfNumberOnly' (by applying it's required fields, if any)
mkArrayOfNumberOnly
  :: ArrayOfNumberOnly
mkArrayOfNumberOnly =
  ArrayOfNumberOnly
  { arrayOfNumberOnlyArrayNumber = Nothing
  }
  

-- ** ArrayTest
-- |
data ArrayTest = ArrayTest
  { arrayTestArrayOfString :: !(Maybe [Text]) -- ^ "array_of_string"
  , arrayTestArrayArrayOfInteger :: !(Maybe [[Integer]]) -- ^ "array_array_of_integer"
  , arrayTestArrayArrayOfModel :: !(Maybe [[ReadOnlyFirst]]) -- ^ "array_array_of_model"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON ArrayTest where
  parseJSON = A.withObject "ArrayTest" $ \o ->
    ArrayTest
      <$> (o .:? "array_of_string")
      <*> (o .:? "array_array_of_integer")
      <*> (o .:? "array_array_of_model")

instance A.ToJSON ArrayTest where
  toJSON ArrayTest {..} =
   _omitNulls
      [ "array_of_string" .= arrayTestArrayOfString
      , "array_array_of_integer" .= arrayTestArrayArrayOfInteger
      , "array_array_of_model" .= arrayTestArrayArrayOfModel
      ]


-- | Construct a value of type 'ArrayTest' (by applying it's required fields, if any)
mkArrayTest
  :: ArrayTest
mkArrayTest =
  ArrayTest
  { arrayTestArrayOfString = Nothing
  , arrayTestArrayArrayOfInteger = Nothing
  , arrayTestArrayArrayOfModel = Nothing
  }
  

-- ** Capitalization
-- |
data Capitalization = Capitalization
  { capitalizationSmallCamel :: !(Maybe Text) -- ^ "smallCamel"
  , capitalizationCapitalCamel :: !(Maybe Text) -- ^ "CapitalCamel"
  , capitalizationSmallSnake :: !(Maybe Text) -- ^ "small_Snake"
  , capitalizationCapitalSnake :: !(Maybe Text) -- ^ "Capital_Snake"
  , capitalizationScaEthFlowPoints :: !(Maybe Text) -- ^ "SCA_ETH_Flow_Points"
  , capitalizationAttName :: !(Maybe Text) -- ^ "ATT_NAME" - Name of the pet 
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON Capitalization where
  parseJSON = A.withObject "Capitalization" $ \o ->
    Capitalization
      <$> (o .:? "smallCamel")
      <*> (o .:? "CapitalCamel")
      <*> (o .:? "small_Snake")
      <*> (o .:? "Capital_Snake")
      <*> (o .:? "SCA_ETH_Flow_Points")
      <*> (o .:? "ATT_NAME")

instance A.ToJSON Capitalization where
  toJSON Capitalization {..} =
   _omitNulls
      [ "smallCamel" .= capitalizationSmallCamel
      , "CapitalCamel" .= capitalizationCapitalCamel
      , "small_Snake" .= capitalizationSmallSnake
      , "Capital_Snake" .= capitalizationCapitalSnake
      , "SCA_ETH_Flow_Points" .= capitalizationScaEthFlowPoints
      , "ATT_NAME" .= capitalizationAttName
      ]


-- | Construct a value of type 'Capitalization' (by applying it's required fields, if any)
mkCapitalization
  :: Capitalization
mkCapitalization =
  Capitalization
  { capitalizationSmallCamel = Nothing
  , capitalizationCapitalCamel = Nothing
  , capitalizationSmallSnake = Nothing
  , capitalizationCapitalSnake = Nothing
  , capitalizationScaEthFlowPoints = Nothing
  , capitalizationAttName = Nothing
  }
  

-- ** Category
-- |
data Category = Category
  { categoryId :: !(Maybe Integer) -- ^ "id"
  , categoryName :: !(Maybe Text) -- ^ "name"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON Category where
  parseJSON = A.withObject "Category" $ \o ->
    Category
      <$> (o .:? "id")
      <*> (o .:? "name")

instance A.ToJSON Category where
  toJSON Category {..} =
   _omitNulls
      [ "id" .= categoryId
      , "name" .= categoryName
      ]


-- | Construct a value of type 'Category' (by applying it's required fields, if any)
mkCategory
  :: Category
mkCategory =
  Category
  { categoryId = Nothing
  , categoryName = Nothing
  }
  

-- ** ClassModel
-- |
-- Model for testing model with \"_class\" property
data ClassModel = ClassModel
  { classModelClass :: !(Maybe Text) -- ^ "_class"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON ClassModel where
  parseJSON = A.withObject "ClassModel" $ \o ->
    ClassModel
      <$> (o .:? "_class")

instance A.ToJSON ClassModel where
  toJSON ClassModel {..} =
   _omitNulls
      [ "_class" .= classModelClass
      ]


-- | Construct a value of type 'ClassModel' (by applying it's required fields, if any)
mkClassModel
  :: ClassModel
mkClassModel =
  ClassModel
  { classModelClass = Nothing
  }
  

-- ** Client
-- |
data Client = Client
  { clientClient :: !(Maybe Text) -- ^ "client"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON Client where
  parseJSON = A.withObject "Client" $ \o ->
    Client
      <$> (o .:? "client")

instance A.ToJSON Client where
  toJSON Client {..} =
   _omitNulls
      [ "client" .= clientClient
      ]


-- | Construct a value of type 'Client' (by applying it's required fields, if any)
mkClient
  :: Client
mkClient =
  Client
  { clientClient = Nothing
  }
  

-- ** EnumArrays
-- |
data EnumArrays = EnumArrays
  { enumArraysJustSymbol :: !(Maybe Text) -- ^ "just_symbol"
  , enumArraysArrayEnum :: !(Maybe [Text]) -- ^ "array_enum"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON EnumArrays where
  parseJSON = A.withObject "EnumArrays" $ \o ->
    EnumArrays
      <$> (o .:? "just_symbol")
      <*> (o .:? "array_enum")

instance A.ToJSON EnumArrays where
  toJSON EnumArrays {..} =
   _omitNulls
      [ "just_symbol" .= enumArraysJustSymbol
      , "array_enum" .= enumArraysArrayEnum
      ]


-- | Construct a value of type 'EnumArrays' (by applying it's required fields, if any)
mkEnumArrays
  :: EnumArrays
mkEnumArrays =
  EnumArrays
  { enumArraysJustSymbol = Nothing
  , enumArraysArrayEnum = Nothing
  }
  

-- ** EnumClass
-- |
data EnumClass = EnumClass
  { 
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON EnumClass where
  parseJSON = A.withObject "EnumClass" $ \o ->
    pure EnumClass
      

instance A.ToJSON EnumClass where
  toJSON EnumClass  =
   _omitNulls
      [ 
      ]


-- | Construct a value of type 'EnumClass' (by applying it's required fields, if any)
mkEnumClass
  :: EnumClass
mkEnumClass =
  EnumClass
  { 
  }
  

-- ** EnumTest
-- |
data EnumTest = EnumTest
  { enumTestEnumString :: !(Maybe Text) -- ^ "enum_string"
  , enumTestEnumInteger :: !(Maybe Int) -- ^ "enum_integer"
  , enumTestEnumNumber :: !(Maybe Double) -- ^ "enum_number"
  , enumTestOuterEnum :: !(Maybe OuterEnum) -- ^ "outerEnum"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON EnumTest where
  parseJSON = A.withObject "EnumTest" $ \o ->
    EnumTest
      <$> (o .:? "enum_string")
      <*> (o .:? "enum_integer")
      <*> (o .:? "enum_number")
      <*> (o .:? "outerEnum")

instance A.ToJSON EnumTest where
  toJSON EnumTest {..} =
   _omitNulls
      [ "enum_string" .= enumTestEnumString
      , "enum_integer" .= enumTestEnumInteger
      , "enum_number" .= enumTestEnumNumber
      , "outerEnum" .= enumTestOuterEnum
      ]


-- | Construct a value of type 'EnumTest' (by applying it's required fields, if any)
mkEnumTest
  :: EnumTest
mkEnumTest =
  EnumTest
  { enumTestEnumString = Nothing
  , enumTestEnumInteger = Nothing
  , enumTestEnumNumber = Nothing
  , enumTestOuterEnum = Nothing
  }
  

-- ** FormatTest
-- |
data FormatTest = FormatTest
  { formatTestInteger :: !(Maybe Int) -- ^ "integer"
  , formatTestInt32 :: !(Maybe Int) -- ^ "int32"
  , formatTestInt64 :: !(Maybe Integer) -- ^ "int64"
  , formatTestNumber :: !(Double) -- ^ /Required/ "number"
  , formatTestFloat :: !(Maybe Float) -- ^ "float"
  , formatTestDouble :: !(Maybe Double) -- ^ "double"
  , formatTestString :: !(Maybe Text) -- ^ "string"
  , formatTestByte :: !(ByteArray) -- ^ /Required/ "byte"
  , formatTestBinary :: !(Maybe Binary) -- ^ "binary"
  , formatTestDate :: !(Date) -- ^ /Required/ "date"
  , formatTestDateTime :: !(Maybe DateTime) -- ^ "dateTime"
  , formatTestUuid :: !(Maybe Text) -- ^ "uuid"
  , formatTestPassword :: !(Text) -- ^ /Required/ "password"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON FormatTest where
  parseJSON = A.withObject "FormatTest" $ \o ->
    FormatTest
      <$> (o .:? "integer")
      <*> (o .:? "int32")
      <*> (o .:? "int64")
      <*> (o .:  "number")
      <*> (o .:? "float")
      <*> (o .:? "double")
      <*> (o .:? "string")
      <*> (o .:  "byte")
      <*> (o .:? "binary")
      <*> (o .:  "date")
      <*> (o .:? "dateTime")
      <*> (o .:? "uuid")
      <*> (o .:  "password")

instance A.ToJSON FormatTest where
  toJSON FormatTest {..} =
   _omitNulls
      [ "integer" .= formatTestInteger
      , "int32" .= formatTestInt32
      , "int64" .= formatTestInt64
      , "number" .= formatTestNumber
      , "float" .= formatTestFloat
      , "double" .= formatTestDouble
      , "string" .= formatTestString
      , "byte" .= formatTestByte
      , "binary" .= formatTestBinary
      , "date" .= formatTestDate
      , "dateTime" .= formatTestDateTime
      , "uuid" .= formatTestUuid
      , "password" .= formatTestPassword
      ]


-- | Construct a value of type 'FormatTest' (by applying it's required fields, if any)
mkFormatTest
  :: Double -- ^ 'formatTestNumber' 
  -> ByteArray -- ^ 'formatTestByte' 
  -> Date -- ^ 'formatTestDate' 
  -> Text -- ^ 'formatTestPassword' 
  -> FormatTest
mkFormatTest formatTestNumber formatTestByte formatTestDate formatTestPassword =
  FormatTest
  { formatTestInteger = Nothing
  , formatTestInt32 = Nothing
  , formatTestInt64 = Nothing
  , formatTestNumber
  , formatTestFloat = Nothing
  , formatTestDouble = Nothing
  , formatTestString = Nothing
  , formatTestByte
  , formatTestBinary = Nothing
  , formatTestDate
  , formatTestDateTime = Nothing
  , formatTestUuid = Nothing
  , formatTestPassword
  }
  

-- ** HasOnlyReadOnly
-- |
data HasOnlyReadOnly = HasOnlyReadOnly
  { hasOnlyReadOnlyBar :: !(Maybe Text) -- ^ "bar"
  , hasOnlyReadOnlyFoo :: !(Maybe Text) -- ^ "foo"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON HasOnlyReadOnly where
  parseJSON = A.withObject "HasOnlyReadOnly" $ \o ->
    HasOnlyReadOnly
      <$> (o .:? "bar")
      <*> (o .:? "foo")

instance A.ToJSON HasOnlyReadOnly where
  toJSON HasOnlyReadOnly {..} =
   _omitNulls
      [ "bar" .= hasOnlyReadOnlyBar
      , "foo" .= hasOnlyReadOnlyFoo
      ]


-- | Construct a value of type 'HasOnlyReadOnly' (by applying it's required fields, if any)
mkHasOnlyReadOnly
  :: HasOnlyReadOnly
mkHasOnlyReadOnly =
  HasOnlyReadOnly
  { hasOnlyReadOnlyBar = Nothing
  , hasOnlyReadOnlyFoo = Nothing
  }
  

-- ** MapTest
-- |
data MapTest = MapTest
  { mapTestMapMapOfString :: !(Maybe (Map.Map String (Map.Map String Text))) -- ^ "map_map_of_string"
  , mapTestMapOfEnumString :: !(Maybe (Map.Map String Text)) -- ^ "map_of_enum_string"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON MapTest where
  parseJSON = A.withObject "MapTest" $ \o ->
    MapTest
      <$> (o .:? "map_map_of_string")
      <*> (o .:? "map_of_enum_string")

instance A.ToJSON MapTest where
  toJSON MapTest {..} =
   _omitNulls
      [ "map_map_of_string" .= mapTestMapMapOfString
      , "map_of_enum_string" .= mapTestMapOfEnumString
      ]


-- | Construct a value of type 'MapTest' (by applying it's required fields, if any)
mkMapTest
  :: MapTest
mkMapTest =
  MapTest
  { mapTestMapMapOfString = Nothing
  , mapTestMapOfEnumString = Nothing
  }
  

-- ** MixedPropertiesAndAdditionalPropertiesClass
-- |
data MixedPropertiesAndAdditionalPropertiesClass = MixedPropertiesAndAdditionalPropertiesClass
  { mixedPropertiesAndAdditionalPropertiesClassUuid :: !(Maybe Text) -- ^ "uuid"
  , mixedPropertiesAndAdditionalPropertiesClassDateTime :: !(Maybe DateTime) -- ^ "dateTime"
  , mixedPropertiesAndAdditionalPropertiesClassMap :: !(Maybe (Map.Map String Animal)) -- ^ "map"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON MixedPropertiesAndAdditionalPropertiesClass where
  parseJSON = A.withObject "MixedPropertiesAndAdditionalPropertiesClass" $ \o ->
    MixedPropertiesAndAdditionalPropertiesClass
      <$> (o .:? "uuid")
      <*> (o .:? "dateTime")
      <*> (o .:? "map")

instance A.ToJSON MixedPropertiesAndAdditionalPropertiesClass where
  toJSON MixedPropertiesAndAdditionalPropertiesClass {..} =
   _omitNulls
      [ "uuid" .= mixedPropertiesAndAdditionalPropertiesClassUuid
      , "dateTime" .= mixedPropertiesAndAdditionalPropertiesClassDateTime
      , "map" .= mixedPropertiesAndAdditionalPropertiesClassMap
      ]


-- | Construct a value of type 'MixedPropertiesAndAdditionalPropertiesClass' (by applying it's required fields, if any)
mkMixedPropertiesAndAdditionalPropertiesClass
  :: MixedPropertiesAndAdditionalPropertiesClass
mkMixedPropertiesAndAdditionalPropertiesClass =
  MixedPropertiesAndAdditionalPropertiesClass
  { mixedPropertiesAndAdditionalPropertiesClassUuid = Nothing
  , mixedPropertiesAndAdditionalPropertiesClassDateTime = Nothing
  , mixedPropertiesAndAdditionalPropertiesClassMap = Nothing
  }
  

-- ** Model200Response
-- |
-- Model for testing model name starting with number
data Model200Response = Model200Response
  { model200ResponseName :: !(Maybe Int) -- ^ "name"
  , model200ResponseClass :: !(Maybe Text) -- ^ "class"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON Model200Response where
  parseJSON = A.withObject "Model200Response" $ \o ->
    Model200Response
      <$> (o .:? "name")
      <*> (o .:? "class")

instance A.ToJSON Model200Response where
  toJSON Model200Response {..} =
   _omitNulls
      [ "name" .= model200ResponseName
      , "class" .= model200ResponseClass
      ]


-- | Construct a value of type 'Model200Response' (by applying it's required fields, if any)
mkModel200Response
  :: Model200Response
mkModel200Response =
  Model200Response
  { model200ResponseName = Nothing
  , model200ResponseClass = Nothing
  }
  

-- ** ModelList
-- |
data ModelList = ModelList
  { modelList123List :: !(Maybe Text) -- ^ "123-list"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON ModelList where
  parseJSON = A.withObject "ModelList" $ \o ->
    ModelList
      <$> (o .:? "123-list")

instance A.ToJSON ModelList where
  toJSON ModelList {..} =
   _omitNulls
      [ "123-list" .= modelList123List
      ]


-- | Construct a value of type 'ModelList' (by applying it's required fields, if any)
mkModelList
  :: ModelList
mkModelList =
  ModelList
  { modelList123List = Nothing
  }
  

-- ** ModelReturn
-- |
-- Model for testing reserved words
data ModelReturn = ModelReturn
  { modelReturnReturn :: !(Maybe Int) -- ^ "return"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON ModelReturn where
  parseJSON = A.withObject "ModelReturn" $ \o ->
    ModelReturn
      <$> (o .:? "return")

instance A.ToJSON ModelReturn where
  toJSON ModelReturn {..} =
   _omitNulls
      [ "return" .= modelReturnReturn
      ]


-- | Construct a value of type 'ModelReturn' (by applying it's required fields, if any)
mkModelReturn
  :: ModelReturn
mkModelReturn =
  ModelReturn
  { modelReturnReturn = Nothing
  }
  

-- ** Name
-- |
-- Model for testing model name same as property name
data Name = Name
  { nameName :: !(Int) -- ^ /Required/ "name"
  , nameSnakeCase :: !(Maybe Int) -- ^ "snake_case"
  , nameProperty :: !(Maybe Text) -- ^ "property"
  , name123Number :: !(Maybe Int) -- ^ "123Number"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON Name where
  parseJSON = A.withObject "Name" $ \o ->
    Name
      <$> (o .:  "name")
      <*> (o .:? "snake_case")
      <*> (o .:? "property")
      <*> (o .:? "123Number")

instance A.ToJSON Name where
  toJSON Name {..} =
   _omitNulls
      [ "name" .= nameName
      , "snake_case" .= nameSnakeCase
      , "property" .= nameProperty
      , "123Number" .= name123Number
      ]


-- | Construct a value of type 'Name' (by applying it's required fields, if any)
mkName
  :: Int -- ^ 'nameName' 
  -> Name
mkName nameName =
  Name
  { nameName
  , nameSnakeCase = Nothing
  , nameProperty = Nothing
  , name123Number = Nothing
  }
  

-- ** NumberOnly
-- |
data NumberOnly = NumberOnly
  { numberOnlyJustNumber :: !(Maybe Double) -- ^ "JustNumber"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON NumberOnly where
  parseJSON = A.withObject "NumberOnly" $ \o ->
    NumberOnly
      <$> (o .:? "JustNumber")

instance A.ToJSON NumberOnly where
  toJSON NumberOnly {..} =
   _omitNulls
      [ "JustNumber" .= numberOnlyJustNumber
      ]


-- | Construct a value of type 'NumberOnly' (by applying it's required fields, if any)
mkNumberOnly
  :: NumberOnly
mkNumberOnly =
  NumberOnly
  { numberOnlyJustNumber = Nothing
  }
  

-- ** Order
-- |
data Order = Order
  { orderId :: !(Maybe Integer) -- ^ "id"
  , orderPetId :: !(Maybe Integer) -- ^ "petId"
  , orderQuantity :: !(Maybe Int) -- ^ "quantity"
  , orderShipDate :: !(Maybe DateTime) -- ^ "shipDate"
  , orderStatus :: !(Maybe Text) -- ^ "status" - Order Status
  , orderComplete :: !(Maybe Bool) -- ^ "complete"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON Order where
  parseJSON = A.withObject "Order" $ \o ->
    Order
      <$> (o .:? "id")
      <*> (o .:? "petId")
      <*> (o .:? "quantity")
      <*> (o .:? "shipDate")
      <*> (o .:? "status")
      <*> (o .:? "complete")

instance A.ToJSON Order where
  toJSON Order {..} =
   _omitNulls
      [ "id" .= orderId
      , "petId" .= orderPetId
      , "quantity" .= orderQuantity
      , "shipDate" .= orderShipDate
      , "status" .= orderStatus
      , "complete" .= orderComplete
      ]


-- | Construct a value of type 'Order' (by applying it's required fields, if any)
mkOrder
  :: Order
mkOrder =
  Order
  { orderId = Nothing
  , orderPetId = Nothing
  , orderQuantity = Nothing
  , orderShipDate = Nothing
  , orderStatus = Nothing
  , orderComplete = Nothing
  }
  

-- ** OuterBoolean
-- |
data OuterBoolean = OuterBoolean
  { 
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON OuterBoolean where
  parseJSON = A.withObject "OuterBoolean" $ \o ->
    pure OuterBoolean
      

instance A.ToJSON OuterBoolean where
  toJSON OuterBoolean  =
   _omitNulls
      [ 
      ]


-- | Construct a value of type 'OuterBoolean' (by applying it's required fields, if any)
mkOuterBoolean
  :: OuterBoolean
mkOuterBoolean =
  OuterBoolean
  { 
  }
  

-- ** OuterComposite
-- |
data OuterComposite = OuterComposite
  { outerCompositeMyNumber :: !(Maybe OuterNumber) -- ^ "my_number"
  , outerCompositeMyString :: !(Maybe OuterString) -- ^ "my_string"
  , outerCompositeMyBoolean :: !(Maybe OuterBoolean) -- ^ "my_boolean"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON OuterComposite where
  parseJSON = A.withObject "OuterComposite" $ \o ->
    OuterComposite
      <$> (o .:? "my_number")
      <*> (o .:? "my_string")
      <*> (o .:? "my_boolean")

instance A.ToJSON OuterComposite where
  toJSON OuterComposite {..} =
   _omitNulls
      [ "my_number" .= outerCompositeMyNumber
      , "my_string" .= outerCompositeMyString
      , "my_boolean" .= outerCompositeMyBoolean
      ]


-- | Construct a value of type 'OuterComposite' (by applying it's required fields, if any)
mkOuterComposite
  :: OuterComposite
mkOuterComposite =
  OuterComposite
  { outerCompositeMyNumber = Nothing
  , outerCompositeMyString = Nothing
  , outerCompositeMyBoolean = Nothing
  }
  

-- ** OuterEnum
-- |
data OuterEnum = OuterEnum
  { 
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON OuterEnum where
  parseJSON = A.withObject "OuterEnum" $ \o ->
    pure OuterEnum
      

instance A.ToJSON OuterEnum where
  toJSON OuterEnum  =
   _omitNulls
      [ 
      ]


-- | Construct a value of type 'OuterEnum' (by applying it's required fields, if any)
mkOuterEnum
  :: OuterEnum
mkOuterEnum =
  OuterEnum
  { 
  }
  

-- ** OuterNumber
-- |
data OuterNumber = OuterNumber
  { 
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON OuterNumber where
  parseJSON = A.withObject "OuterNumber" $ \o ->
    pure OuterNumber
      

instance A.ToJSON OuterNumber where
  toJSON OuterNumber  =
   _omitNulls
      [ 
      ]


-- | Construct a value of type 'OuterNumber' (by applying it's required fields, if any)
mkOuterNumber
  :: OuterNumber
mkOuterNumber =
  OuterNumber
  { 
  }
  

-- ** OuterString
-- |
data OuterString = OuterString
  { 
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON OuterString where
  parseJSON = A.withObject "OuterString" $ \o ->
    pure OuterString
      

instance A.ToJSON OuterString where
  toJSON OuterString  =
   _omitNulls
      [ 
      ]


-- | Construct a value of type 'OuterString' (by applying it's required fields, if any)
mkOuterString
  :: OuterString
mkOuterString =
  OuterString
  { 
  }
  

-- ** Pet
-- |
data Pet = Pet
  { petId :: !(Maybe Integer) -- ^ "id"
  , petCategory :: !(Maybe Category) -- ^ "category"
  , petName :: !(Text) -- ^ /Required/ "name"
  , petPhotoUrls :: !([Text]) -- ^ /Required/ "photoUrls"
  , petTags :: !(Maybe [Tag]) -- ^ "tags"
  , petStatus :: !(Maybe Text) -- ^ "status" - pet status in the store
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON Pet where
  parseJSON = A.withObject "Pet" $ \o ->
    Pet
      <$> (o .:? "id")
      <*> (o .:? "category")
      <*> (o .:  "name")
      <*> (o .:  "photoUrls")
      <*> (o .:? "tags")
      <*> (o .:? "status")

instance A.ToJSON Pet where
  toJSON Pet {..} =
   _omitNulls
      [ "id" .= petId
      , "category" .= petCategory
      , "name" .= petName
      , "photoUrls" .= petPhotoUrls
      , "tags" .= petTags
      , "status" .= petStatus
      ]


-- | Construct a value of type 'Pet' (by applying it's required fields, if any)
mkPet
  :: Text -- ^ 'petName' 
  -> [Text] -- ^ 'petPhotoUrls' 
  -> Pet
mkPet petName petPhotoUrls =
  Pet
  { petId = Nothing
  , petCategory = Nothing
  , petName
  , petPhotoUrls
  , petTags = Nothing
  , petStatus = Nothing
  }
  

-- ** ReadOnlyFirst
-- |
data ReadOnlyFirst = ReadOnlyFirst
  { readOnlyFirstBar :: !(Maybe Text) -- ^ "bar"
  , readOnlyFirstBaz :: !(Maybe Text) -- ^ "baz"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON ReadOnlyFirst where
  parseJSON = A.withObject "ReadOnlyFirst" $ \o ->
    ReadOnlyFirst
      <$> (o .:? "bar")
      <*> (o .:? "baz")

instance A.ToJSON ReadOnlyFirst where
  toJSON ReadOnlyFirst {..} =
   _omitNulls
      [ "bar" .= readOnlyFirstBar
      , "baz" .= readOnlyFirstBaz
      ]


-- | Construct a value of type 'ReadOnlyFirst' (by applying it's required fields, if any)
mkReadOnlyFirst
  :: ReadOnlyFirst
mkReadOnlyFirst =
  ReadOnlyFirst
  { readOnlyFirstBar = Nothing
  , readOnlyFirstBaz = Nothing
  }
  

-- ** SpecialModelName
-- |
data SpecialModelName = SpecialModelName
  { specialModelNameSpecialPropertyName :: !(Maybe Integer) -- ^ "$special[property.name]"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON SpecialModelName where
  parseJSON = A.withObject "SpecialModelName" $ \o ->
    SpecialModelName
      <$> (o .:? "$special[property.name]")

instance A.ToJSON SpecialModelName where
  toJSON SpecialModelName {..} =
   _omitNulls
      [ "$special[property.name]" .= specialModelNameSpecialPropertyName
      ]


-- | Construct a value of type 'SpecialModelName' (by applying it's required fields, if any)
mkSpecialModelName
  :: SpecialModelName
mkSpecialModelName =
  SpecialModelName
  { specialModelNameSpecialPropertyName = Nothing
  }
  

-- ** Tag
-- |
data Tag = Tag
  { tagId :: !(Maybe Integer) -- ^ "id"
  , tagName :: !(Maybe Text) -- ^ "name"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON Tag where
  parseJSON = A.withObject "Tag" $ \o ->
    Tag
      <$> (o .:? "id")
      <*> (o .:? "name")

instance A.ToJSON Tag where
  toJSON Tag {..} =
   _omitNulls
      [ "id" .= tagId
      , "name" .= tagName
      ]


-- | Construct a value of type 'Tag' (by applying it's required fields, if any)
mkTag
  :: Tag
mkTag =
  Tag
  { tagId = Nothing
  , tagName = Nothing
  }
  

-- ** User
-- |
data User = User
  { userId :: !(Maybe Integer) -- ^ "id"
  , userUsername :: !(Maybe Text) -- ^ "username"
  , userFirstName :: !(Maybe Text) -- ^ "firstName"
  , userLastName :: !(Maybe Text) -- ^ "lastName"
  , userEmail :: !(Maybe Text) -- ^ "email"
  , userPassword :: !(Maybe Text) -- ^ "password"
  , userPhone :: !(Maybe Text) -- ^ "phone"
  , userUserStatus :: !(Maybe Int) -- ^ "userStatus" - User Status
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON User where
  parseJSON = A.withObject "User" $ \o ->
    User
      <$> (o .:? "id")
      <*> (o .:? "username")
      <*> (o .:? "firstName")
      <*> (o .:? "lastName")
      <*> (o .:? "email")
      <*> (o .:? "password")
      <*> (o .:? "phone")
      <*> (o .:? "userStatus")

instance A.ToJSON User where
  toJSON User {..} =
   _omitNulls
      [ "id" .= userId
      , "username" .= userUsername
      , "firstName" .= userFirstName
      , "lastName" .= userLastName
      , "email" .= userEmail
      , "password" .= userPassword
      , "phone" .= userPhone
      , "userStatus" .= userUserStatus
      ]


-- | Construct a value of type 'User' (by applying it's required fields, if any)
mkUser
  :: User
mkUser =
  User
  { userId = Nothing
  , userUsername = Nothing
  , userFirstName = Nothing
  , userLastName = Nothing
  , userEmail = Nothing
  , userPassword = Nothing
  , userPhone = Nothing
  , userUserStatus = Nothing
  }
  

-- ** Cat
-- |
data Cat = Cat
  { catClassName :: !(Text) -- ^ /Required/ "className"
  , catColor :: !(Maybe Text) -- ^ "color"
  , catDeclawed :: !(Maybe Bool) -- ^ "declawed"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON Cat where
  parseJSON = A.withObject "Cat" $ \o ->
    Cat
      <$> (o .:  "className")
      <*> (o .:? "color")
      <*> (o .:? "declawed")

instance A.ToJSON Cat where
  toJSON Cat {..} =
   _omitNulls
      [ "className" .= catClassName
      , "color" .= catColor
      , "declawed" .= catDeclawed
      ]


-- | Construct a value of type 'Cat' (by applying it's required fields, if any)
mkCat
  :: Text -- ^ 'catClassName' 
  -> Cat
mkCat catClassName =
  Cat
  { catClassName
  , catColor = Nothing
  , catDeclawed = Nothing
  }
  

-- ** Dog
-- |
data Dog = Dog
  { dogClassName :: !(Text) -- ^ /Required/ "className"
  , dogColor :: !(Maybe Text) -- ^ "color"
  , dogBreed :: !(Maybe Text) -- ^ "breed"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON Dog where
  parseJSON = A.withObject "Dog" $ \o ->
    Dog
      <$> (o .:  "className")
      <*> (o .:? "color")
      <*> (o .:? "breed")

instance A.ToJSON Dog where
  toJSON Dog {..} =
   _omitNulls
      [ "className" .= dogClassName
      , "color" .= dogColor
      , "breed" .= dogBreed
      ]


-- | Construct a value of type 'Dog' (by applying it's required fields, if any)
mkDog
  :: Text -- ^ 'dogClassName' 
  -> Dog
mkDog dogClassName =
  Dog
  { dogClassName
  , dogColor = Nothing
  , dogBreed = Nothing
  }
  

-- * Utils

-- | Removes Null fields.  (OpenAPI-Specification 2.0 does not allow Null in JSON)

_omitNulls :: [(Text, A.Value)] -> A.Value
_omitNulls = A.object . P.filter notNull
  where
    notNull (_, A.Null) = False
    notNull _ = True

_toFormItem :: (WH.ToHttpApiData a, Functor f) => t -> f a -> f (t, [Text])
_toFormItem name x = (name,) . (:[]) . WH.toQueryParam <$> x

_emptyToNothing :: Maybe String -> Maybe String
_emptyToNothing (Just "") = Nothing
_emptyToNothing x = x
{-# INLINE _emptyToNothing #-}

_memptyToNothing :: (P.Monoid a, P.Eq a) => Maybe a -> Maybe a
_memptyToNothing (Just x) | x P.== P.mempty = Nothing
_memptyToNothing x = x
{-# INLINE _memptyToNothing #-}

-- * DateTime Formatting

newtype DateTime = DateTime { unDateTime :: TI.UTCTime }
  deriving (P.Eq,P.Data,P.Ord,P.Typeable,NF.NFData,TI.ParseTime,TI.FormatTime)
instance A.FromJSON DateTime where
  parseJSON = A.withText "DateTime" (_readDateTime . T.unpack)
instance A.ToJSON DateTime where
  toJSON (DateTime t) = A.toJSON (_showDateTime t)
instance WH.FromHttpApiData DateTime where
  parseUrlPiece = P.left T.pack . _readDateTime . T.unpack
instance WH.ToHttpApiData DateTime where
  toUrlPiece (DateTime t) = T.pack (_showDateTime t)
instance P.Show DateTime where
  show (DateTime t) = _showDateTime t

-- | @_parseISO8601@
_readDateTime :: (TI.ParseTime t, Monad m, Alternative m) => String -> m t
_readDateTime =
  _parseISO8601
{-# INLINE _readDateTime #-}

-- | @TI.formatISO8601Millis@
_showDateTime :: (t ~ TI.UTCTime, TI.FormatTime t) => t -> String
_showDateTime =
  TI.formatISO8601Millis
{-# INLINE _showDateTime #-}

_parseISO8601 :: (TI.ParseTime t, Monad m, Alternative m) => String -> m t
_parseISO8601 t =
  P.asum $
  P.flip (TI.parseTimeM True TI.defaultTimeLocale) t <$>
  ["%FT%T%QZ", "%FT%T%Q%z", "%FT%T%Q%Z"]
{-# INLINE _parseISO8601 #-}

-- * Date Formatting

newtype Date = Date { unDate :: TI.Day }
  deriving (P.Enum,P.Eq,P.Data,P.Ord,P.Ix,NF.NFData,TI.ParseTime,TI.FormatTime)
instance A.FromJSON Date where
  parseJSON = A.withText "Date" (_readDate . T.unpack)
instance A.ToJSON Date where
  toJSON (Date t) = A.toJSON (_showDate t)
instance WH.FromHttpApiData Date where
  parseUrlPiece = P.left T.pack . _readDate . T.unpack
instance WH.ToHttpApiData Date where
  toUrlPiece (Date t) = T.pack (_showDate t)
instance P.Show Date where
  show (Date t) = _showDate t

-- | @TI.parseTimeM True TI.defaultTimeLocale "%Y-%m-%d"@
_readDate :: (TI.ParseTime t, Monad m) => String -> m t
_readDate =
  TI.parseTimeM True TI.defaultTimeLocale "%Y-%m-%d"
{-# INLINE _readDate #-}

-- | @TI.formatTime TI.defaultTimeLocale "%Y-%m-%d"@
_showDate :: TI.FormatTime t => t -> String
_showDate =
  TI.formatTime TI.defaultTimeLocale "%Y-%m-%d"
{-# INLINE _showDate #-}

-- * Byte/Binary Formatting

  
-- | base64 encoded characters
newtype ByteArray = ByteArray { unByteArray :: BL.ByteString }
  deriving (P.Eq,P.Data,P.Ord,P.Typeable,NF.NFData)

instance A.FromJSON ByteArray where
  parseJSON = A.withText "ByteArray" _readByteArray
instance A.ToJSON ByteArray where
  toJSON = A.toJSON . _showByteArray
instance WH.FromHttpApiData ByteArray where
  parseUrlPiece = P.left T.pack . _readByteArray
instance WH.ToHttpApiData ByteArray where
  toUrlPiece = _showByteArray
instance P.Show ByteArray where
  show = T.unpack . _showByteArray

-- | read base64 encoded characters
_readByteArray :: Monad m => Text -> m ByteArray
_readByteArray = P.either P.fail (pure . ByteArray) . BL64.decode . BL.fromStrict . T.encodeUtf8
{-# INLINE _readByteArray #-}

-- | show base64 encoded characters
_showByteArray :: ByteArray -> Text
_showByteArray = T.decodeUtf8 . BL.toStrict . BL64.encode . unByteArray
{-# INLINE _showByteArray #-}

-- | any sequence of octets
newtype Binary = Binary { unBinary :: BL.ByteString }
  deriving (P.Eq,P.Data,P.Ord,P.Typeable,NF.NFData)

instance A.FromJSON Binary where
  parseJSON = A.withText "Binary" _readBinaryBase64
instance A.ToJSON Binary where
  toJSON = A.toJSON . _showBinaryBase64
instance WH.FromHttpApiData Binary where
  parseUrlPiece = P.left T.pack . _readBinaryBase64
instance WH.ToHttpApiData Binary where
  toUrlPiece = _showBinaryBase64
instance P.Show Binary where
  show = T.unpack . _showBinaryBase64

_readBinaryBase64 :: Monad m => Text -> m Binary
_readBinaryBase64 = P.either P.fail (pure . Binary) . BL64.decode . BL.fromStrict . T.encodeUtf8
{-# INLINE _readBinaryBase64 #-}

_showBinaryBase64 :: Binary -> Text
_showBinaryBase64 = T.decodeUtf8 . BL.toStrict . BL64.encode . unBinary
{-# INLINE _showBinaryBase64 #-}