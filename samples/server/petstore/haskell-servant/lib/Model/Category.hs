{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Category
    ( Category (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data Category = Category
    { id_ :: Integer
    , name :: String
    } deriving (Show, Eq, Generic)

instance FromJSON Category
instance ToJSON Category
instance Arbitrary Category where
    arbitrary = Category <$> arbitrary <*> arbitrary
