
{-|
Module : SwaggerPetstore.MimeTypes
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SwaggerPetstore.MimeTypes where


import qualified Data.Aeson as A

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BCL

import qualified Network.HTTP.Media as ME

import qualified Web.FormUrlEncoded as WH

import qualified Data.Data as P (Typeable)
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Control.Arrow as P (left)

import Prelude (($), (.),(<$>),(<*>),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty)
import qualified Prelude as P

-- * Content Negotiation

-- | A type for responses without content-body.
data NoContent = NoContent
  deriving (P.Show, P.Eq)

-- ** Mime Types

data MimeJSON = MimeJSON deriving (P.Typeable)
data MimeXML = MimeXML deriving (P.Typeable)
data MimePlainText = MimePlainText deriving (P.Typeable)
data MimeFormUrlEncoded = MimeFormUrlEncoded deriving (P.Typeable)
data MimeMultipartFormData = MimeMultipartFormData deriving (P.Typeable)
data MimeOctetStream = MimeOctetStream deriving (P.Typeable)
data MimeNoContent = MimeNoContent deriving (P.Typeable)


-- ** MimeType Class

class P.Typeable mtype => MimeType mtype  where
  {-# MINIMAL mimeType | mimeTypes #-}

  mimeTypes :: P.Proxy mtype -> [ME.MediaType]
  mimeTypes p =
    case mimeType p of
      Just x -> [x]
      Nothing -> []

  mimeType :: P.Proxy mtype -> Maybe ME.MediaType
  mimeType p =
    case mimeTypes p of
      [] -> Nothing
      (x:_) -> Just x

  mimeType' :: mtype -> Maybe ME.MediaType
  mimeType' _ = mimeType (P.Proxy :: P.Proxy mtype)
  mimeTypes' :: mtype -> [ME.MediaType]
  mimeTypes' _ = mimeTypes (P.Proxy :: P.Proxy mtype)

-- ** MimeType Instances

-- | @application/json@
instance MimeType MimeJSON where
  mimeTypes _ =
    [ "application" ME.// "json" ME./: ("charset", "utf-8")
    , "application" ME.// "json"
    ]

-- | @application/xml@
instance MimeType MimeXML where
  mimeType _ = Just $ "application" ME.// "xml"

-- | @application/x-www-form-urlencoded@
instance MimeType MimeFormUrlEncoded where
  mimeType _ = Just $ "application" ME.// "x-www-form-urlencoded"

-- | @multipart/form-data@
instance MimeType MimeMultipartFormData where
  mimeType _ = Just $ "multipart" ME.// "form-data"

-- | @text/plain;charset=utf-8@
instance MimeType MimePlainText where
  mimeType _ = Just $ "text" ME.// "plain" ME./: ("charset", "utf-8")
instance MimeType MimeOctetStream where
  mimeType _ = Just $ "application" ME.// "octet-stream"
instance MimeType MimeNoContent where
  mimeType _ = Nothing


-- ** MimeRender Class

class MimeType mtype => MimeRender mtype x where
    mimeRender  :: P.Proxy mtype -> x -> BL.ByteString
    mimeRender' :: mtype -> x -> BL.ByteString
    mimeRender' _ x = mimeRender (P.Proxy :: P.Proxy mtype) x


-- ** MimeRender Instances

-- | `A.encode`
instance A.ToJSON a => MimeRender MimeJSON a where mimeRender _ = A.encode
-- | @WH.urlEncodeAsForm@
instance WH.ToForm a => MimeRender MimeFormUrlEncoded a where mimeRender _ = WH.urlEncodeAsForm

-- | @P.id@
instance MimeRender MimePlainText BL.ByteString where mimeRender _ = P.id
-- | @BL.fromStrict . T.encodeUtf8@
instance MimeRender MimePlainText T.Text where mimeRender _ = BL.fromStrict . T.encodeUtf8
-- | @BCL.pack@
instance MimeRender MimePlainText String where mimeRender _ = BCL.pack

-- | @P.id@
instance MimeRender MimeOctetStream BL.ByteString where mimeRender _ = P.id
-- | @BL.fromStrict . T.encodeUtf8@
instance MimeRender MimeOctetStream T.Text where mimeRender _ = BL.fromStrict . T.encodeUtf8
-- | @BCL.pack@
instance MimeRender MimeOctetStream String where mimeRender _ = BCL.pack

-- | @P.id@
instance MimeRender MimeMultipartFormData BL.ByteString where mimeRender _ = P.id
-- | @BL.fromStrict . T.encodeUtf8@
instance MimeRender MimeMultipartFormData T.Text where mimeRender _ = BL.fromStrict . T.encodeUtf8
-- | @BCL.pack@
instance MimeRender MimeMultipartFormData String where mimeRender _ = BCL.pack

-- | @P.Right . P.const NoContent@
instance MimeRender MimeNoContent NoContent where mimeRender _ = P.const BCL.empty

-- instance MimeRender MimeOctetStream Double where mimeRender _ = BB.toLazyByteString . BB.doubleDec
-- instance MimeRender MimeOctetStream Float where mimeRender _ = BB.toLazyByteString . BB.floatDec
-- instance MimeRender MimeOctetStream Int where mimeRender _ = BB.toLazyByteString . BB.intDec
-- instance MimeRender MimeOctetStream Integer where mimeRender _ = BB.toLazyByteString . BB.integerDec


-- ** MimeUnrender Class

class MimeType mtype => MimeUnrender mtype o where
    mimeUnrender :: P.Proxy mtype -> BL.ByteString -> P.Either String o
    mimeUnrender' :: mtype -> BL.ByteString -> P.Either String o
    mimeUnrender' _ x = mimeUnrender (P.Proxy :: P.Proxy mtype) x

-- ** MimeUnrender Instances

-- | @A.eitherDecode@
instance A.FromJSON a => MimeUnrender MimeJSON a where mimeUnrender _ = A.eitherDecode
-- | @P.left T.unpack . WH.urlDecodeAsForm@
instance WH.FromForm a => MimeUnrender MimeFormUrlEncoded a where mimeUnrender _ = P.left T.unpack . WH.urlDecodeAsForm
-- | @P.Right . P.id@

instance MimeUnrender MimePlainText BL.ByteString where mimeUnrender _ = P.Right . P.id
-- | @P.left P.show . TL.decodeUtf8'@
instance MimeUnrender MimePlainText T.Text where mimeUnrender _ = P.left P.show . T.decodeUtf8' . BL.toStrict
-- | @P.Right . BCL.unpack@
instance MimeUnrender MimePlainText String where mimeUnrender _ = P.Right . BCL.unpack

-- | @P.Right . P.id@
instance MimeUnrender MimeOctetStream BL.ByteString where mimeUnrender _ = P.Right . P.id
-- | @P.left P.show . T.decodeUtf8' . BL.toStrict@
instance MimeUnrender MimeOctetStream T.Text where mimeUnrender _ = P.left P.show . T.decodeUtf8' . BL.toStrict
-- | @P.Right . BCL.unpack@
instance MimeUnrender MimeOctetStream String where mimeUnrender _ = P.Right . BCL.unpack

-- | @P.Right . P.const NoContent@
instance MimeUnrender MimeNoContent NoContent where mimeUnrender _ = P.Right . P.const NoContent


-- ** Request Consumes

class MimeType mtype => Consumes req mtype where

-- ** Request Produces

class MimeType mtype => Produces req mtype where
