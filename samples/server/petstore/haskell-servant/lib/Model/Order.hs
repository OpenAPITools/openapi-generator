{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Order
    ( Order (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data Order = Order
    { id_ :: Integer
    , petId :: Integer
    , quantity :: Integer
    , shipDate :: Integer
    , status :: String
    , complete :: Bool
    } deriving (Show, Eq, Generic)

instance FromJSON Order
instance ToJSON Order
instance Arbitrary Order where
    arbitrary = Order <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
