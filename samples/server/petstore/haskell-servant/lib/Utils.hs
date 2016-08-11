{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module Utils where

import GHC.Generics
import Servant.API
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Text as T
import Test.QuickCheck

instance FromText [String] where
    fromText = Just . splitOn "," . T.unpack

instance ToText [String] where
    toText = T.pack . intercalate ","

lkp inputs l = case lookup l inputs of
        Nothing -> Left $ "label " ++ T.unpack l ++ " not found"
        Just v  -> Right $ read (T.unpack v)

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map.Map k v) where
    arbitrary = Map.fromList <$> arbitrary
