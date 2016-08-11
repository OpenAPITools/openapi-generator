{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Tag
    ( Tag (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data Tag = Tag
    { id_ :: Integer
    , name :: String
    } deriving (Show, Eq, Generic)

instance FromJSON Tag
instance ToJSON Tag
instance Arbitrary Tag where
    arbitrary = Tag <$> arbitrary <*> arbitrary
