{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Error where

import           ClassyPrelude.Yesod
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Encoding.Error as TEE


data ErrorResponseBody = ErrorResponseBody
    { title  :: T.Text
    , detail :: T.Text
    }

instance ToContent ErrorResponseBody where
    toContent = toContent . encode

instance ToTypedContent ErrorResponseBody where
    toTypedContent = TypedContent "application/json" . toContent

$(deriveJSON defaultOptions 'ErrorResponseBody)

defaultErrorHandlerJson :: Yesod site => ErrorResponse -> HandlerFor site TypedContent
-- 400
defaultErrorHandlerJson (InvalidArgs ia) = fmap toTypedContent $ returnJson $
    ErrorResponseBody "Bad Request" $ T.intercalate " " ia
-- 401
defaultErrorHandlerJson NotAuthenticated = fmap toTypedContent $ returnJson $
    ErrorResponseBody "Unauthorized" "authentication required"
-- 403
defaultErrorHandlerJson (PermissionDenied _) = fmap toTypedContent $ returnJson $
    ErrorResponseBody "Forbidden" "unauthorized"
-- 404
defaultErrorHandlerJson NotFound = fmap toTypedContent $ returnJson $
    ErrorResponseBody "Not Found" "resource not found"
-- 405
defaultErrorHandlerJson (BadMethod m) = fmap toTypedContent $ returnJson $
    ErrorResponseBody "Method Not Allowed" $ "method " <> TE.decodeUtf8With TEE.lenientDecode m <> " not supported"
-- 500
defaultErrorHandlerJson (InternalError e) = fmap toTypedContent $ returnJson $
    ErrorResponseBody "Internal Server Error" e

-- 501
notImplemented :: HandlerFor site res
notImplemented = sendResponseStatus notImplemented501 $
    ErrorResponseBody "Not Implemented" "operation not implemented"
