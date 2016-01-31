{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Pet
    ( Pet (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Model.Category
import Model.Tag


data Pet = Pet
    { id_ :: Integer
    , category :: Category
    , name :: String
    , photoUrls :: [String]
    , tags :: [Tag]
    , status :: String
    } deriving (Show, Eq, Generic)

instance FromJSON Pet
instance ToJSON Pet
instance Arbitrary Pet where
    arbitrary = Pet <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
