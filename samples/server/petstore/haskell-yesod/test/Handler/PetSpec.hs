{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.PetSpec (spec) where

import           TestImport


spec :: Spec
spec = withApp $ do

    describe "postPetR" $
        it "returns 501 Not Implemented" $ do
            post PetR
            statusIs 501

    describe "deletePetByInt64R" $
        it "returns 501 Not Implemented" $ do
            performMethod "DELETE" $ PetByInt64R 789
            statusIs 501

    describe "getPetFindByStatusR" $
        it "returns 501 Not Implemented" $ do
            get PetFindByStatusR
            statusIs 501

    describe "getPetFindByTagsR" $
        it "returns 501 Not Implemented" $ do
            get PetFindByTagsR
            statusIs 501

    describe "getPetByInt64R" $
        it "returns 501 Not Implemented" $ do
            get $ PetByInt64R 789
            statusIs 501

    describe "putPetR" $
        it "returns 501 Not Implemented" $ do
            performMethod "PUT" PetR
            statusIs 501

    describe "postPetByInt64R" $
        it "returns 501 Not Implemented" $ do
            post $ PetByInt64R 789
            statusIs 501

    describe "postPetByInt64UploadImageR" $
        it "returns 501 Not Implemented" $ do
            post $ PetByInt64UploadImageR 789
            statusIs 501
