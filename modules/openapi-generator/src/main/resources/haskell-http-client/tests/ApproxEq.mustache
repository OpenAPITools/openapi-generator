{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ApproxEq where

import Data.Text (Text)
import Data.Time.Clock
import Test.QuickCheck
import GHC.Generics as G

(==~)
  :: (ApproxEq a, Show a)
  => a -> a -> Property
a ==~ b = counterexample (show a ++ " !=~ " ++ show b) (a =~ b)

class GApproxEq f  where
  gApproxEq :: f a -> f a -> Bool

instance GApproxEq U1 where
  gApproxEq U1 U1 = True

instance (GApproxEq a, GApproxEq b) =>
         GApproxEq (a :+: b) where
  gApproxEq (L1 a) (L1 b) = gApproxEq a b
  gApproxEq (R1 a) (R1 b) = gApproxEq a b
  gApproxEq _ _ = False

instance (GApproxEq a, GApproxEq b) =>
         GApproxEq (a :*: b) where
  gApproxEq (a1 :*: b1) (a2 :*: b2) = gApproxEq a1 a2 && gApproxEq b1 b2

instance (ApproxEq a) =>
         GApproxEq (K1 i a) where
  gApproxEq (K1 a) (K1 b) = a =~ b

instance (GApproxEq f) =>
         GApproxEq (M1 i t f) where
  gApproxEq (M1 a) (M1 b) = gApproxEq a b

class ApproxEq a  where
  (=~) :: a -> a -> Bool
  default (=~) :: (Generic a, GApproxEq (Rep a)) => a -> a -> Bool
  a =~ b = gApproxEq (G.from a) (G.from b)

instance ApproxEq Text where
  (=~) = (==)

instance ApproxEq Char where
  (=~) = (==)

instance ApproxEq Bool where
  (=~) = (==)

instance ApproxEq Int where
  (=~) = (==)

instance ApproxEq Double where
  (=~) = (==)

instance ApproxEq a =>
         ApproxEq (Maybe a)

instance ApproxEq UTCTime where
  (=~) = (==)

instance ApproxEq a =>
         ApproxEq [a] where
  as =~ bs = and (zipWith (=~) as bs)

instance (ApproxEq l, ApproxEq r) =>
         ApproxEq (Either l r) where
  Left a =~ Left b = a =~ b
  Right a =~ Right b = a =~ b
  _ =~ _ = False

instance (ApproxEq l, ApproxEq r) =>
         ApproxEq (l, r) where
  (=~) (l1, r1) (l2, r2) = l1 =~ l2 && r1 =~ r2
