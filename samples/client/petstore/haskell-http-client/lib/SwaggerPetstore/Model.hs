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
import Data.Text (Text)

import Data.Aeson (Value)
import Data.ByteString.Lazy (ByteString)

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Data as P (Data, Typeable)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Foldable as P
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import qualified Data.Time as TI
import qualified Data.Time.ISO8601 as TI
import Data.Time (UTCTime)

import Control.Applicative ((<|>))
import Control.Applicative (Alternative)
import Prelude (($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P



-- * Models


-- ** ApiResponse
-- |
-- An uploaded response
-- 
-- Describes the result of uploading an image resource
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
      [ "code" .=  apiResponseCode
      , "type" .=  apiResponseType
      , "message" .=  apiResponseMessage
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
  


-- ** Category
-- |
-- Pet catehgry
-- 
-- A category for a pet
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
      [ "id" .=  categoryId
      , "name" .=  categoryName
      ]


-- | Construct a value of type 'Category' (by applying it's required fields, if any)
mkCategory
  :: Category
mkCategory =
  Category
  { categoryId = Nothing
  , categoryName = Nothing
  }
  


-- ** Order
-- |
-- Pet Order
-- 
-- An order for a pets from the pet store
data Order = Order
  { orderId :: !(Maybe Integer) -- ^ "id"
  , orderPetId :: !(Maybe Integer) -- ^ "petId"
  , orderQuantity :: !(Maybe Int) -- ^ "quantity"
  , orderShipDate :: !(Maybe UTCTime) -- ^ "shipDate"
  , orderStatus :: !(Maybe Text) -- ^ "status" - Order Status
  , orderComplete :: !(Maybe Bool) -- ^ "complete"
  } deriving (P.Show,P.Eq,P.Typeable)

instance A.FromJSON Order where
  parseJSON = A.withObject "Order" $ \o ->
    Order
      <$> (o .:? "id")
      <*> (o .:? "petId")
      <*> (o .:? "quantity")
      <*> (o .:? "shipDate" >>= P.mapM _readDateTime)
      <*> (o .:? "status")
      <*> (o .:? "complete")

instance A.ToJSON Order where
  toJSON Order {..} =
   _omitNulls
      [ "id" .=  orderId
      , "petId" .=  orderPetId
      , "quantity" .=  orderQuantity
      , "shipDate" .= P.fmap _showDateTime orderShipDate
      , "status" .=  orderStatus
      , "complete" .=  orderComplete
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
  


-- ** Pet
-- |
-- a Pet
-- 
-- A pet for sale in the pet store
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
      [ "id" .=  petId
      , "category" .=  petCategory
      , "name" .=  petName
      , "photoUrls" .=  petPhotoUrls
      , "tags" .=  petTags
      , "status" .=  petStatus
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
  


-- ** Tag
-- |
-- Pet Tag
-- 
-- A tag for a pet
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
      [ "id" .=  tagId
      , "name" .=  tagName
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
-- a User
-- 
-- A User who is purchasing from the pet store
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
      [ "id" .=  userId
      , "username" .=  userUsername
      , "firstName" .=  userFirstName
      , "lastName" .=  userLastName
      , "email" .=  userEmail
      , "password" .=  userPassword
      , "phone" .=  userPhone
      , "userStatus" .=  userUserStatus
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

-- | @_parseISO8601@
_readDateTime :: (TI.ParseTime t, Monad m, Alternative m) => String -> m t
_readDateTime =
  _parseISO8601
{-# INLINE _readDateTime #-}

-- | @TI.formatISO8601Millis@
_showDateTime :: (t ~ UTCTime, TI.FormatTime t) => t -> String
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
