{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.User
    ( User (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data User = User
    { id_ :: Integer
    , username :: String
    , firstName :: String
    , lastName :: String
    , email :: String
    , password :: String
    , phone :: String
    , userStatus :: Integer
    } deriving (Show, Eq, Generic)

instance FromJSON User
instance ToJSON User
instance Arbitrary User where
    arbitrary = User <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
