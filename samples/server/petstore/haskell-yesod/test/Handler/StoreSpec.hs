{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.StoreSpec (spec) where

import           TestImport


spec :: Spec
spec = withApp $ do

    describe "deleteStoreOrderByTextR" $
        it "returns 501 Not Implemented" $ do
            performMethod "DELETE" $ StoreOrderByTextR "orderId_example"
            statusIs 501

    describe "getStoreInventoryR" $
        it "returns 501 Not Implemented" $ do
            get StoreInventoryR
            statusIs 501

    describe "getStoreOrderByInt64R" $
        it "returns 501 Not Implemented" $ do
            get $ StoreOrderByInt64R 789
            statusIs 501

    describe "postStoreOrderR" $
        it "returns 501 Not Implemented" $ do
            post StoreOrderR
            statusIs 501
