{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.Pet where

import           Import


-- | Add a new pet to the store
--
-- 
-- operationId: addPet
postPetR :: Handler Value
postPetR = notImplemented

-- | Deletes a pet
--
-- 
-- operationId: deletePet
deletePetByInt64R :: Int64 -- ^ Pet id to delete
                  -> Handler Value
deletePetByInt64R petId = notImplemented

-- | Finds Pets by status
--
-- Multiple status values can be provided with comma separated strings
-- operationId: findPetsByStatus
getPetFindByStatusR :: Handler Value
getPetFindByStatusR = notImplemented

-- | Finds Pets by tags
--
-- Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
-- operationId: findPetsByTags
getPetFindByTagsR :: Handler Value
getPetFindByTagsR = notImplemented

-- | Find pet by ID
--
-- Returns a single pet
-- operationId: getPetById
getPetByInt64R :: Int64 -- ^ ID of pet to return
               -> Handler Value
getPetByInt64R petId = notImplemented

-- | Update an existing pet
--
-- 
-- operationId: updatePet
putPetR :: Handler Value
putPetR = notImplemented

-- | Updates a pet in the store with form data
--
-- 
-- operationId: updatePetWithForm
postPetByInt64R :: Int64 -- ^ ID of pet that needs to be updated
                -> Handler Value
postPetByInt64R petId = notImplemented

-- | uploads an image
--
-- 
-- operationId: uploadFile
postPetByInt64UploadImageR :: Int64 -- ^ ID of pet to update
                           -> Handler Value
postPetByInt64UploadImageR petId = notImplemented
