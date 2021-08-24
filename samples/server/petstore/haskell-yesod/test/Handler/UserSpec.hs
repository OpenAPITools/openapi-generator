{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.UserSpec (spec) where

import           TestImport


spec :: Spec
spec = withApp $ do

    describe "postUserR" $
        it "returns 501 Not Implemented" $ do
            post UserR
            statusIs 501

    describe "postUserCreateWithArrayR" $
        it "returns 501 Not Implemented" $ do
            post UserCreateWithArrayR
            statusIs 501

    describe "postUserCreateWithListR" $
        it "returns 501 Not Implemented" $ do
            post UserCreateWithListR
            statusIs 501

    describe "deleteUserByTextR" $
        it "returns 501 Not Implemented" $ do
            performMethod "DELETE" $ UserByTextR "username_example"
            statusIs 501

    describe "getUserByTextR" $
        it "returns 501 Not Implemented" $ do
            get $ UserByTextR "username_example"
            statusIs 501

    describe "getUserLoginR" $
        it "returns 501 Not Implemented" $ do
            get UserLoginR
            statusIs 501

    describe "getUserLogoutR" $
        it "returns 501 Not Implemented" $ do
            get UserLogoutR
            statusIs 501

    describe "putUserByTextR" $
        it "returns 501 Not Implemented" $ do
            performMethod "PUT" $ UserByTextR "username_example"
            statusIs 501
