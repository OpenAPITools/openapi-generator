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

type Traversal_' s a = Traversal_ s s a a
type Traversal_ s t a b = forall (f :: * -> *). Applicative f => (a -> f b) -> s -> f t
type Lens_' s a = Lens_ s s a a
type Lens_ s t a b = forall (f :: * -> *). Functor f => (a -> f b) -> s -> f t


-- * ApiResponse

-- | 'apiResponseCode' Traversal
apiResponseCodeT :: Traversal_' ApiResponse Int
apiResponseCodeT f s = _mtraversal apiResponseCode (\b -> s { apiResponseCode = Just b}) f s
{-# INLINE apiResponseCodeT #-}

-- | 'apiResponseType' Traversal
apiResponseTypeT :: Traversal_' ApiResponse Text
apiResponseTypeT f s = _mtraversal apiResponseType (\b -> s { apiResponseType = Just b}) f s
{-# INLINE apiResponseTypeT #-}

-- | 'apiResponseMessage' Traversal
apiResponseMessageT :: Traversal_' ApiResponse Text
apiResponseMessageT f s = _mtraversal apiResponseMessage (\b -> s { apiResponseMessage = Just b}) f s
{-# INLINE apiResponseMessageT #-}



-- * Category

-- | 'categoryId' Traversal
categoryIdT :: Traversal_' Category Integer
categoryIdT f s = _mtraversal categoryId (\b -> s { categoryId = Just b}) f s
{-# INLINE categoryIdT #-}

-- | 'categoryName' Traversal
categoryNameT :: Traversal_' Category Text
categoryNameT f s = _mtraversal categoryName (\b -> s { categoryName = Just b}) f s
{-# INLINE categoryNameT #-}



-- * Order

-- | 'orderId' Traversal
orderIdT :: Traversal_' Order Integer
orderIdT f s = _mtraversal orderId (\b -> s { orderId = Just b}) f s
{-# INLINE orderIdT #-}

-- | 'orderPetId' Traversal
orderPetIdT :: Traversal_' Order Integer
orderPetIdT f s = _mtraversal orderPetId (\b -> s { orderPetId = Just b}) f s
{-# INLINE orderPetIdT #-}

-- | 'orderQuantity' Traversal
orderQuantityT :: Traversal_' Order Int
orderQuantityT f s = _mtraversal orderQuantity (\b -> s { orderQuantity = Just b}) f s
{-# INLINE orderQuantityT #-}

-- | 'orderShipDate' Traversal
orderShipDateT :: Traversal_' Order UTCTime
orderShipDateT f s = _mtraversal orderShipDate (\b -> s { orderShipDate = Just b}) f s
{-# INLINE orderShipDateT #-}

-- | 'orderStatus' Traversal
orderStatusT :: Traversal_' Order Text
orderStatusT f s = _mtraversal orderStatus (\b -> s { orderStatus = Just b}) f s
{-# INLINE orderStatusT #-}

-- | 'orderComplete' Traversal
orderCompleteT :: Traversal_' Order Bool
orderCompleteT f s = _mtraversal orderComplete (\b -> s { orderComplete = Just b}) f s
{-# INLINE orderCompleteT #-}



-- * Pet

-- | 'petId' Traversal
petIdT :: Traversal_' Pet Integer
petIdT f s = _mtraversal petId (\b -> s { petId = Just b}) f s
{-# INLINE petIdT #-}

-- | 'petCategory' Traversal
petCategoryT :: Traversal_' Pet Category
petCategoryT f s = _mtraversal petCategory (\b -> s { petCategory = Just b}) f s
{-# INLINE petCategoryT #-}

-- | 'petName' Lens
petNameL :: Lens_' Pet Text
petNameL f Pet{..} = (\petName -> Pet { petName, ..} ) <$> f petName
{-# INLINE petNameL #-}

-- | 'petPhotoUrls' Lens
petPhotoUrlsL :: Lens_' Pet [Text]
petPhotoUrlsL f Pet{..} = (\petPhotoUrls -> Pet { petPhotoUrls, ..} ) <$> f petPhotoUrls
{-# INLINE petPhotoUrlsL #-}

-- | 'petTags' Traversal
petTagsT :: Traversal_' Pet [Tag]
petTagsT f s = _mtraversal petTags (\b -> s { petTags = Just b}) f s
{-# INLINE petTagsT #-}

-- | 'petStatus' Traversal
petStatusT :: Traversal_' Pet Text
petStatusT f s = _mtraversal petStatus (\b -> s { petStatus = Just b}) f s
{-# INLINE petStatusT #-}



-- * Tag

-- | 'tagId' Traversal
tagIdT :: Traversal_' Tag Integer
tagIdT f s = _mtraversal tagId (\b -> s { tagId = Just b}) f s
{-# INLINE tagIdT #-}

-- | 'tagName' Traversal
tagNameT :: Traversal_' Tag Text
tagNameT f s = _mtraversal tagName (\b -> s { tagName = Just b}) f s
{-# INLINE tagNameT #-}



-- * User

-- | 'userId' Traversal
userIdT :: Traversal_' User Integer
userIdT f s = _mtraversal userId (\b -> s { userId = Just b}) f s
{-# INLINE userIdT #-}

-- | 'userUsername' Traversal
userUsernameT :: Traversal_' User Text
userUsernameT f s = _mtraversal userUsername (\b -> s { userUsername = Just b}) f s
{-# INLINE userUsernameT #-}

-- | 'userFirstName' Traversal
userFirstNameT :: Traversal_' User Text
userFirstNameT f s = _mtraversal userFirstName (\b -> s { userFirstName = Just b}) f s
{-# INLINE userFirstNameT #-}

-- | 'userLastName' Traversal
userLastNameT :: Traversal_' User Text
userLastNameT f s = _mtraversal userLastName (\b -> s { userLastName = Just b}) f s
{-# INLINE userLastNameT #-}

-- | 'userEmail' Traversal
userEmailT :: Traversal_' User Text
userEmailT f s = _mtraversal userEmail (\b -> s { userEmail = Just b}) f s
{-# INLINE userEmailT #-}

-- | 'userPassword' Traversal
userPasswordT :: Traversal_' User Text
userPasswordT f s = _mtraversal userPassword (\b -> s { userPassword = Just b}) f s
{-# INLINE userPasswordT #-}

-- | 'userPhone' Traversal
userPhoneT :: Traversal_' User Text
userPhoneT f s = _mtraversal userPhone (\b -> s { userPhone = Just b}) f s
{-# INLINE userPhoneT #-}

-- | 'userUserStatus' Traversal
userUserStatusT :: Traversal_' User Int
userUserStatusT f s = _mtraversal userUserStatus (\b -> s { userUserStatus = Just b}) f s
{-# INLINE userUserStatusT #-}




-- * Helpers

_mtraversal :: Applicative f => (b -> Maybe t) -> (a -> b) -> (t -> f a) -> b -> f b
_mtraversal x fsb f s = maybe (pure s) (\a -> fsb <$> f a) (x s)
{-# INLINE _mtraversal #-}
