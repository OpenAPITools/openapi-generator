{-|
Module : SwaggerPetstore.Client
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SwaggerPetstore.Client where

import SwaggerPetstore.Model
import SwaggerPetstore.API
import SwaggerPetstore.MimeTypes
import SwaggerPetstore.Logging

import qualified Control.Monad.IO.Class as P
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Proxy as P (Proxy(..))
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Exts (IsString(..))
import Web.FormUrlEncoded as WH
import Web.HttpApiData as WH
import Control.Monad.Catch (MonadThrow)

import qualified Data.Time as TI
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Printf as T

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.ByteString.Builder as BB
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Client.TLS as NH
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Types.Method as NH
import qualified Network.HTTP.Types as NH
import qualified Network.HTTP.Types.URI as NH

import qualified Control.Exception.Safe as E
-- * Config

-- | 
data SwaggerPetstoreConfig = SwaggerPetstoreConfig
  { configHost  :: BCL.ByteString -- ^ host supplied in the Request
  , configUserAgent :: Text -- ^ user-agent supplied in the Request
  , configLogExecWithContext :: LogExecWithContext -- ^ Run a block using a Logger instance
  , configLogContext :: LogContext -- ^ Configures the logger
  }

-- | display the config
instance Show SwaggerPetstoreConfig where
  show c =
    T.printf
      "{ configHost = %v, configUserAgent = %v, ..}"
      (show (configHost c))
      (show (configUserAgent c))

-- | constructs a default SwaggerPetstoreConfig
--
-- configHost:
--
-- @http://petstore.swagger.io/v2@
--
-- configUserAgent:
--
-- @"swagger-haskell-http-client/1.0.0"@
--
newConfig :: IO SwaggerPetstoreConfig
newConfig = do
    logCxt <- initLogContext
    return $ SwaggerPetstoreConfig
        { configHost = "http://petstore.swagger.io/v2"
        , configUserAgent = "swagger-haskell-http-client/1.0.0"
        , configLogExecWithContext = runDefaultLogExecWithContext
        , configLogContext = logCxt
        }  

withStdoutLogging :: SwaggerPetstoreConfig -> IO SwaggerPetstoreConfig
withStdoutLogging p = do
    logCxt <- stdoutLoggingContext (configLogContext p)
    return $ p { configLogExecWithContext = stdoutLoggingExec, configLogContext = logCxt }

withStderrLogging :: SwaggerPetstoreConfig -> IO SwaggerPetstoreConfig
withStderrLogging p = do
    logCxt <- stderrLoggingContext (configLogContext p)
    return $ p { configLogExecWithContext = stderrLoggingExec, configLogContext = logCxt }

-- | updates the config to disable logging
withNoLogging :: SwaggerPetstoreConfig -> SwaggerPetstoreConfig
withNoLogging p = p { configLogExecWithContext =  runNullLogExec}

-- * Dispatch

-- ** Lbs

-- | send a request returning the raw http response
dispatchLbs
  :: (Produces req accept, MimeType contentType)
  => NH.Manager -- ^ http-client Connection manager
  -> SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest req contentType res -- ^ request
  -> accept -- ^ "accept" 'MimeType'
  -> IO (NH.Response BCL.ByteString) -- ^ response
dispatchLbs manager config request accept = do
  initReq <- _toInitRequest config request accept 
  dispatchInitUnsafe manager config initReq

-- ** Mime

-- | pair of decoded http body and http response
data MimeResult res =
  MimeResult { mimeResult :: Either MimeError res -- ^ decoded http body
             , mimeResultResponse :: NH.Response BCL.ByteString -- ^ http response 
             }
  deriving (Show, Functor, Foldable, Traversable)

-- | pair of unrender/parser error and http response
data MimeError =
  MimeError {
    mimeError :: String -- ^ unrender/parser error
  , mimeErrorResponse :: NH.Response BCL.ByteString -- ^ http response 
  } deriving (Eq, Show)

-- | send a request returning the 'MimeResult'
dispatchMime
  :: (Produces req accept, MimeUnrender accept res, MimeType contentType)
  => NH.Manager -- ^ http-client Connection manager
  -> SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest req contentType res -- ^ request
  -> accept -- ^ "accept" 'MimeType'
  -> IO (MimeResult res) -- ^ response
dispatchMime manager config request accept = do
  httpResponse <- dispatchLbs manager config request accept
  parsedResult <-
    runConfigLogWithExceptions "Client" config $
    do case mimeUnrender' accept (NH.responseBody httpResponse) of
         Left s -> do
           _log "Client" levelError (T.pack s)
           pure (Left (MimeError s httpResponse))
         Right r -> pure (Right r)
  return (MimeResult parsedResult httpResponse)

-- | like 'dispatchMime', but only returns the decoded http body
dispatchMime'
  :: (Produces req accept, MimeUnrender accept res, MimeType contentType)
  => NH.Manager -- ^ http-client Connection manager
  -> SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest req contentType res -- ^ request
  -> accept -- ^ "accept" 'MimeType'
  -> IO (Either MimeError res) -- ^ response
dispatchMime' manager config request accept = do
    MimeResult parsedResult _ <- dispatchMime manager config request accept 
    return parsedResult

-- ** Unsafe

-- | like 'dispatchReqLbs', but does not validate the operation is a 'Producer' of the "accept" 'MimeType'.  (Useful if the server's response is undocumented)
dispatchLbsUnsafe
  :: (MimeType accept, MimeType contentType)
  => NH.Manager -- ^ http-client Connection manager
  -> SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest req contentType res -- ^ request
  -> accept -- ^ "accept" 'MimeType'
  -> IO (NH.Response BCL.ByteString) -- ^ response
dispatchLbsUnsafe manager config request accept = do
  initReq <- _toInitRequest config request accept
  dispatchInitUnsafe manager config initReq

-- | dispatch an InitRequest
dispatchInitUnsafe
  :: NH.Manager -- ^ http-client Connection manager
  -> SwaggerPetstoreConfig -- ^ config
  -> InitRequest req contentType res accept -- ^ init request
  -> IO (NH.Response BCL.ByteString) -- ^ response
dispatchInitUnsafe manager config (InitRequest req) = do
  runConfigLogWithExceptions src config $
    do _log src levelInfo requestLogMsg
       _log src levelDebug requestDbgLogMsg
       res <- P.liftIO $ NH.httpLbs req manager
       _log src levelInfo (responseLogMsg res)
       _log src levelDebug ((T.pack . show) res)
       return res
  where
    src = "Client"
    endpoint =
      T.pack $
      BC.unpack $
      NH.method req <> " " <> NH.host req <> NH.path req <> NH.queryString req
    requestLogMsg = "REQ:" <> endpoint
    requestDbgLogMsg =
      "Headers=" <> (T.pack . show) (NH.requestHeaders req) <> " Body=" <>
      (case NH.requestBody req of
         NH.RequestBodyLBS xs -> T.decodeUtf8 (BL.toStrict xs)
         _ -> "<RequestBody>")
    responseStatusCode = (T.pack . show) . NH.statusCode . NH.responseStatus
    responseLogMsg res =
      "RES:statusCode=" <> responseStatusCode res <> " (" <> endpoint <> ")"

-- * InitRequest

-- | wraps an http-client 'Request' with request/response type parameters
newtype InitRequest req contentType res accept = InitRequest
  { unInitRequest :: NH.Request
  } deriving (Show)

-- |  Build an http-client 'Request' record from the supplied config and request
_toInitRequest
  :: (MimeType accept, MimeType contentType)
  => SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest req contentType res -- ^ request
  -> accept -- ^ "accept" 'MimeType'
  -> IO (InitRequest req contentType res accept) -- ^ initialized request
_toInitRequest config req0 accept = do
  parsedReq <- NH.parseRequest $ BCL.unpack $ BCL.append (configHost config) (BCL.concat (rUrlPath req0))
  let req1 = _setAcceptHeader req0 accept & _setContentTypeHeader
      reqHeaders = ("User-Agent", WH.toHeader (configUserAgent config)) : paramsHeaders (rParams req1)
      reqQuery = NH.renderQuery True (paramsQuery (rParams req1))
      pReq = parsedReq { NH.method = (rMethod req1)
                       , NH.requestHeaders = reqHeaders
                       , NH.queryString = reqQuery
                       }
  outReq <- case paramsBody (rParams req1) of
    ParamBodyNone -> pure (pReq { NH.requestBody = mempty })
    ParamBodyB bs -> pure (pReq { NH.requestBody = NH.RequestBodyBS bs })
    ParamBodyBL bl -> pure (pReq { NH.requestBody = NH.RequestBodyLBS bl })
    ParamBodyFormUrlEncoded form -> pure (pReq { NH.requestBody = NH.RequestBodyLBS (WH.urlEncodeForm form) })
    ParamBodyMultipartFormData parts -> NH.formDataBody parts pReq

  pure (InitRequest outReq)

-- | modify the underlying Request
modifyInitRequest :: InitRequest req contentType res accept -> (NH.Request -> NH.Request) -> InitRequest req contentType res accept 
modifyInitRequest (InitRequest req) f = InitRequest (f req)

-- | modify the underlying Request (monadic)
modifyInitRequestM :: Monad m => InitRequest req contentType res accept -> (NH.Request -> m NH.Request) -> m (InitRequest req contentType res accept)
modifyInitRequestM (InitRequest req) f = fmap InitRequest (f req)

-- ** Logging 

-- | Run a block using the configured logger instance
runConfigLog
  :: P.MonadIO m
  => SwaggerPetstoreConfig -> LogExec m
runConfigLog config = configLogExecWithContext config (configLogContext config)

-- | Run a block using the configured logger instance (logs exceptions)
runConfigLogWithExceptions
  :: (E.MonadCatch m, P.MonadIO m)
  => T.Text -> SwaggerPetstoreConfig -> LogExec m
runConfigLogWithExceptions src config = runConfigLog config . logExceptions src
