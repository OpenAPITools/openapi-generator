{-|
Module : SwaggerPetstore.Lens
-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-matches -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SwaggerPetstore.Lens where

import Data.Text (Text)

import qualified Data.Aeson as A
import Data.Aeson (Value)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.Data as P (Data, Typeable)
import qualified Data.Map as Map

import qualified Data.Time as TI
import Data.Time (UTCTime)

import Prelude (($), (.),(<$>),(<*>),(=<<),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

import SwaggerPetstore.Model

-- * Type Aliases

type Lens_' s a = Lens_ s s a a
type Lens_ s t a b = forall (f :: * -> *). Functor f => (a -> f b) -> s -> f t


-- * ApiResponse

-- | 'apiResponseCode' Lens
apiResponseCodeL :: Lens_' ApiResponse (Maybe Int)
apiResponseCodeL f ApiResponse{..} = (\apiResponseCode -> ApiResponse { apiResponseCode, ..} ) <$> f apiResponseCode
{-# INLINE apiResponseCodeL #-}

-- | 'apiResponseType' Lens
apiResponseTypeL :: Lens_' ApiResponse (Maybe Text)
apiResponseTypeL f ApiResponse{..} = (\apiResponseType -> ApiResponse { apiResponseType, ..} ) <$> f apiResponseType
{-# INLINE apiResponseTypeL #-}

-- | 'apiResponseMessage' Lens
apiResponseMessageL :: Lens_' ApiResponse (Maybe Text)
apiResponseMessageL f ApiResponse{..} = (\apiResponseMessage -> ApiResponse { apiResponseMessage, ..} ) <$> f apiResponseMessage
{-# INLINE apiResponseMessageL #-}



-- * Category

-- | 'categoryId' Lens
categoryIdL :: Lens_' Category (Maybe Integer)
categoryIdL f Category{..} = (\categoryId -> Category { categoryId, ..} ) <$> f categoryId
{-# INLINE categoryIdL #-}

-- | 'categoryName' Lens
categoryNameL :: Lens_' Category (Maybe Text)
categoryNameL f Category{..} = (\categoryName -> Category { categoryName, ..} ) <$> f categoryName
{-# INLINE categoryNameL #-}



-- * Order

-- | 'orderId' Lens
orderIdL :: Lens_' Order (Maybe Integer)
orderIdL f Order{..} = (\orderId -> Order { orderId, ..} ) <$> f orderId
{-# INLINE orderIdL #-}

-- | 'orderPetId' Lens
orderPetIdL :: Lens_' Order (Maybe Integer)
orderPetIdL f Order{..} = (\orderPetId -> Order { orderPetId, ..} ) <$> f orderPetId
{-# INLINE orderPetIdL #-}

-- | 'orderQuantity' Lens
orderQuantityL :: Lens_' Order (Maybe Int)
orderQuantityL f Order{..} = (\orderQuantity -> Order { orderQuantity, ..} ) <$> f orderQuantity
{-# INLINE orderQuantityL #-}

-- | 'orderShipDate' Lens
orderShipDateL :: Lens_' Order (Maybe UTCTime)
orderShipDateL f Order{..} = (\orderShipDate -> Order { orderShipDate, ..} ) <$> f orderShipDate
{-# INLINE orderShipDateL #-}

-- | 'orderStatus' Lens
orderStatusL :: Lens_' Order (Maybe Text)
orderStatusL f Order{..} = (\orderStatus -> Order { orderStatus, ..} ) <$> f orderStatus
{-# INLINE orderStatusL #-}

-- | 'orderComplete' Lens
orderCompleteL :: Lens_' Order (Maybe Bool)
orderCompleteL f Order{..} = (\orderComplete -> Order { orderComplete, ..} ) <$> f orderComplete
{-# INLINE orderCompleteL #-}



-- * Pet

-- | 'petId' Lens
petIdL :: Lens_' Pet (Maybe Integer)
petIdL f Pet{..} = (\petId -> Pet { petId, ..} ) <$> f petId
{-# INLINE petIdL #-}

-- | 'petCategory' Lens
petCategoryL :: Lens_' Pet (Maybe Category)
petCategoryL f Pet{..} = (\petCategory -> Pet { petCategory, ..} ) <$> f petCategory
{-# INLINE petCategoryL #-}

-- | 'petName' Lens
petNameL :: Lens_' Pet (Text)
petNameL f Pet{..} = (\petName -> Pet { petName, ..} ) <$> f petName
{-# INLINE petNameL #-}

-- | 'petPhotoUrls' Lens
petPhotoUrlsL :: Lens_' Pet ([Text])
petPhotoUrlsL f Pet{..} = (\petPhotoUrls -> Pet { petPhotoUrls, ..} ) <$> f petPhotoUrls
{-# INLINE petPhotoUrlsL #-}

-- | 'petTags' Lens
petTagsL :: Lens_' Pet (Maybe [Tag])
petTagsL f Pet{..} = (\petTags -> Pet { petTags, ..} ) <$> f petTags
{-# INLINE petTagsL #-}

-- | 'petStatus' Lens
petStatusL :: Lens_' Pet (Maybe Text)
petStatusL f Pet{..} = (\petStatus -> Pet { petStatus, ..} ) <$> f petStatus
{-# INLINE petStatusL #-}



-- * Tag

-- | 'tagId' Lens
tagIdL :: Lens_' Tag (Maybe Integer)
tagIdL f Tag{..} = (\tagId -> Tag { tagId, ..} ) <$> f tagId
{-# INLINE tagIdL #-}

-- | 'tagName' Lens
tagNameL :: Lens_' Tag (Maybe Text)
tagNameL f Tag{..} = (\tagName -> Tag { tagName, ..} ) <$> f tagName
{-# INLINE tagNameL #-}



-- * User

-- | 'userId' Lens
userIdL :: Lens_' User (Maybe Integer)
userIdL f User{..} = (\userId -> User { userId, ..} ) <$> f userId
{-# INLINE userIdL #-}

-- | 'userUsername' Lens
userUsernameL :: Lens_' User (Maybe Text)
userUsernameL f User{..} = (\userUsername -> User { userUsername, ..} ) <$> f userUsername
{-# INLINE userUsernameL #-}

-- | 'userFirstName' Lens
userFirstNameL :: Lens_' User (Maybe Text)
userFirstNameL f User{..} = (\userFirstName -> User { userFirstName, ..} ) <$> f userFirstName
{-# INLINE userFirstNameL #-}

-- | 'userLastName' Lens
userLastNameL :: Lens_' User (Maybe Text)
userLastNameL f User{..} = (\userLastName -> User { userLastName, ..} ) <$> f userLastName
{-# INLINE userLastNameL #-}

-- | 'userEmail' Lens
userEmailL :: Lens_' User (Maybe Text)
userEmailL f User{..} = (\userEmail -> User { userEmail, ..} ) <$> f userEmail
{-# INLINE userEmailL #-}

-- | 'userPassword' Lens
userPasswordL :: Lens_' User (Maybe Text)
userPasswordL f User{..} = (\userPassword -> User { userPassword, ..} ) <$> f userPassword
{-# INLINE userPasswordL #-}

-- | 'userPhone' Lens
userPhoneL :: Lens_' User (Maybe Text)
userPhoneL f User{..} = (\userPhone -> User { userPhone, ..} ) <$> f userPhone
{-# INLINE userPhoneL #-}

-- | 'userUserStatus' Lens
userUserStatusL :: Lens_' User (Maybe Int)
userUserStatusL f User{..} = (\userUserStatus -> User { userUserStatus, ..} ) <$> f userUserStatus
{-# INLINE userUserStatusL #-}


