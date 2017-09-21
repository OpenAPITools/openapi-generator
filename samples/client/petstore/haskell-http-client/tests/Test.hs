{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import SwaggerPetstore.Model
import SwaggerPetstore.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $
  do describe "JSON instances" $
       do propMimeEq MimeJSON (Proxy :: Proxy ApiResponse)
          propMimeEq MimeJSON (Proxy :: Proxy Category)
          propMimeEq MimeJSON (Proxy :: Proxy Order)
          propMimeEq MimeJSON (Proxy :: Proxy Pet)
          propMimeEq MimeJSON (Proxy :: Proxy Tag)
          propMimeEq MimeJSON (Proxy :: Proxy User)
          
