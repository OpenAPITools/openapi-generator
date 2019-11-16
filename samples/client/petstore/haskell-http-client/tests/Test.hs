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

import OpenAPIPetstore.Model
import OpenAPIPetstore.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy AdditionalPropertiesAnyType)
      propMimeEq MimeJSON (Proxy :: Proxy AdditionalPropertiesArray)
      propMimeEq MimeJSON (Proxy :: Proxy AdditionalPropertiesBoolean)
      propMimeEq MimeJSON (Proxy :: Proxy AdditionalPropertiesClass)
      propMimeEq MimeJSON (Proxy :: Proxy AdditionalPropertiesInteger)
      propMimeEq MimeJSON (Proxy :: Proxy AdditionalPropertiesNumber)
      propMimeEq MimeJSON (Proxy :: Proxy AdditionalPropertiesObject)
      propMimeEq MimeJSON (Proxy :: Proxy AdditionalPropertiesString)
      propMimeEq MimeJSON (Proxy :: Proxy Animal)
      propMimeEq MimeJSON (Proxy :: Proxy ApiResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ArrayOfArrayOfNumberOnly)
      propMimeEq MimeJSON (Proxy :: Proxy ArrayOfNumberOnly)
      propMimeEq MimeJSON (Proxy :: Proxy ArrayTest)
      propMimeEq MimeJSON (Proxy :: Proxy BigCat)
      propMimeEq MimeJSON (Proxy :: Proxy BigCatAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy Capitalization)
      propMimeEq MimeJSON (Proxy :: Proxy Cat)
      propMimeEq MimeJSON (Proxy :: Proxy CatAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy Category)
      propMimeEq MimeJSON (Proxy :: Proxy ClassModel)
      propMimeEq MimeJSON (Proxy :: Proxy Client)
      propMimeEq MimeJSON (Proxy :: Proxy Dog)
      propMimeEq MimeJSON (Proxy :: Proxy DogAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy EnumArrays)
      propMimeEq MimeJSON (Proxy :: Proxy EnumClass)
      propMimeEq MimeJSON (Proxy :: Proxy EnumTest)
      propMimeEq MimeJSON (Proxy :: Proxy File)
      propMimeEq MimeJSON (Proxy :: Proxy FileSchemaTestClass)
      propMimeEq MimeJSON (Proxy :: Proxy FormatTest)
      propMimeEq MimeJSON (Proxy :: Proxy HasOnlyReadOnly)
      propMimeEq MimeJSON (Proxy :: Proxy MapTest)
      propMimeEq MimeJSON (Proxy :: Proxy MixedPropertiesAndAdditionalPropertiesClass)
      propMimeEq MimeJSON (Proxy :: Proxy Model200Response)
      propMimeEq MimeJSON (Proxy :: Proxy ModelList)
      propMimeEq MimeJSON (Proxy :: Proxy ModelReturn)
      propMimeEq MimeJSON (Proxy :: Proxy Name)
      propMimeEq MimeJSON (Proxy :: Proxy NumberOnly)
      propMimeEq MimeJSON (Proxy :: Proxy Order)
      propMimeEq MimeJSON (Proxy :: Proxy OuterComposite)
      propMimeEq MimeJSON (Proxy :: Proxy OuterEnum)
      propMimeEq MimeJSON (Proxy :: Proxy Pet)
      propMimeEq MimeJSON (Proxy :: Proxy ReadOnlyFirst)
      propMimeEq MimeJSON (Proxy :: Proxy SpecialModelName)
      propMimeEq MimeJSON (Proxy :: Proxy Tag)
      propMimeEq MimeJSON (Proxy :: Proxy TypeHolderDefault)
      propMimeEq MimeJSON (Proxy :: Proxy TypeHolderExample)
      propMimeEq MimeJSON (Proxy :: Proxy User)
      propMimeEq MimeJSON (Proxy :: Proxy XmlItem)
      
