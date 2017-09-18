{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-orphans #-}

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Lens.Micro as L
import qualified Network.HTTP.Client as NH

import qualified SwaggerPetstore as S

import Data.Monoid ((<>))

import System.Environment (getEnvironment)

-- * MAIN

main :: IO ()
main = do

  env <- getEnvironment

  mgr <- NH.newManager NH.defaultManagerSettings

  config0 <- S.withStdoutLogging =<< S.newConfig 

  let config = case lookup "HOST" env of
        Just h -> config0 { S.configHost = BCL.pack h }
        _ -> config0

  putStrLn "******** CONFIG ********"
  putStrLn (show config)

  putStrLn "******** Pet operations ********"
  runPet mgr config

  putStrLn "******** Store operations ********"
  runStore mgr config

  putStrLn "******** User operations ********"
  runUser mgr config

  putStrLn "******** END ********"

  return ()


-- * PET

runPet :: NH.Manager -> S.SwaggerPetstoreConfig -> IO ()
runPet mgr config = do

  -- create the request for addPet, encoded with content-type application/json
  let addPetRequest = S.addPet S.MimeJSON (S.mkPet "name" ["url1", "url2"])

  -- send the rquest with accept header application/json
  -- dispatchLbs simply returns the raw Network.HTTP.Client.Response ByteString
  addPetResponse <- S.dispatchLbs mgr config addPetRequest S.MimeJSON

  -- the Consumes & Produces typeclasses control which 'content-type'
  -- and 'accept' encodings are allowed for each operation
  -- -- No instance for (S.Produces S.AddPet S.MimePlainText)
  -- addPetResponse <- S.dispatchLbs mgr config addPetRequest S.MimePlainText

  -- inspect the AddPet type to see typeclasses indicating wihch
  -- content-type and accept types (mimeTypes) are valid

  -- :i S.AddPet
    -- data S.AddPet 	-- Defined in ‘SwaggerPetstore.API’
    -- instance S.Produces S.AddPet S.MimeXML
    --   -- Defined in ‘SwaggerPetstore.API’
    -- instance S.Produces S.AddPet S.MimeJSON
    --   -- Defined in ‘SwaggerPetstore.API’
    -- instance S.Consumes S.AddPet S.MimeXML
    --   -- Defined in ‘SwaggerPetstore.API’
    -- instance S.Consumes S.AddPet S.MimeJSON
    --   -- Defined in ‘SwaggerPetstore.API’
    -- instance S.HasBodyParam S.AddPet S.Pet
    --   -- Defined in ‘SwaggerPetstore.API’

  
  -- since this swagger definition has no response schema defined for
  -- the 'addPet' response, we decode the response bytestring manually
  case A.eitherDecode (NH.responseBody addPetResponse) of
    Right pet@S.Pet { S.petId = Just pid } -> do

        -- create the request for getPetById
        let getPetByIdRequest = S.getPetById pid

        -- dispatchMime returns MimeResult, which includes the
        -- expected decoded model object 'Pet', or a parse failure
        getPetByIdRequestResult <- S.dispatchMime mgr config getPetByIdRequest S.MimeJSON
        case S.mimeResult getPetByIdRequestResult of
          Left (S.MimeError _ _) -> return () -- parse error, already displayed in the log
          Right r -> putStrLn $ "getPetById: found pet: " <> show r  -- display 'Pet' model object, r 

        -- findPetsByStatus
        let findPetsByStatusRequest = S.findPetsByStatus ["available","pending","sold"]
        findPetsByStatusResult <- S.dispatchMime mgr config findPetsByStatusRequest S.MimeJSON
        mapM_ (\r -> putStrLn $ "findPetsByStatus: found " <> (show . length) r <> " pets") findPetsByStatusResult 

        -- findPetsByTags
        let findPetsByTagsRequest = S.findPetsByTags ["name","tag1"]
        findPetsByTagsResult <- S.dispatchMime mgr config findPetsByTagsRequest S.MimeJSON
        mapM_ (\r -> putStrLn $ "findPetsByTags: found " <> (show . length) r <> " pets") findPetsByTagsResult 

        -- updatePet
        let updatePetRequest = S.updatePet S.MimeJSON $ pet
                { S.petStatus   = Just "available"
                , S.petCategory = Just (S.Category (Just 3) (Just "catname"))
                }
        _ <- S.dispatchLbs mgr config updatePetRequest S.MimeXML

        -- requred parameters are included as function arguments, optional parameters are included with applyOptionalParam
        -- inspect the UpdatePetWithForm type to see typeclasses indicating optional paramteters (:i S.UpdatePetWithForm)
        -- instance S.HasOptionalParam S.UpdatePetWithForm S.Name
        --   -- Defined in ‘SwaggerPetstore.API’
        -- instance S.HasOptionalParam S.UpdatePetWithForm S.Status
        --   -- Defined in ‘SwaggerPetstore.API’
        let updatePetWithFormRequest = S.updatePetWithForm S.MimeFormUrlEncoded pid
                `S.applyOptionalParam` S.Name "petName"
                `S.applyOptionalParam` S.Status "pending"
        _ <- S.dispatchLbs mgr config updatePetWithFormRequest S.MimeJSON

        -- multipart/form-data file uploads are just a different content-type
        let uploadFileRequest = S.uploadFile S.MimeMultipartFormData pid
                `S.applyOptionalParam` S.File "package.yaml" -- the file contents of the path are read when dispatched
                `S.applyOptionalParam` S.AdditionalMetadata "a package.yaml file"
        uploadFileRequestResult <- S.dispatchMime mgr config uploadFileRequest S.MimeJSON
        mapM_ (\r -> putStrLn $ "uploadFile: " <> show r) uploadFileRequestResult 

        -- deletePet
        let deletePetRequest = S.deletePet pid
                `S.applyOptionalParam` S.ApiUnderscorekey "api key"
        _ <- S.dispatchLbs mgr config deletePetRequest S.MimeJSON

        return ()

    Left e -> putStrLn e
    _ -> putStrLn "no Pet id returned"

  return ()



-- * STORE
  
-- declare that 'placeOrder' can recieve a JSON content-type request
instance S.Consumes S.PlaceOrder S.MimeJSON 

runStore :: NH.Manager -> S.SwaggerPetstoreConfig -> IO ()
runStore mgr config = do

  -- we can set arbitrary headers with setHeader
  let getInventoryRequest = S.getInventory
        `S.setHeader` [("api_key","special-key")] 
  getInventoryRequestRequestResult <- S.dispatchMime mgr config getInventoryRequest S.MimeJSON
  mapM_ (\r -> putStrLn $ "getInventoryRequest: found " <> (show . length) r <> " results") getInventoryRequestRequestResult

  -- placeOrder
  now <- TI.getCurrentTime
  let placeOrderRequest = S.placeOrder S.MimeJSON (S.mkOrder { S.orderId = Just 21, S.orderQuantity = Just 210, S.orderShipDate = Just now})
  placeOrderResult <- S.dispatchMime mgr config placeOrderRequest S.MimeJSON
  mapM_ (\r -> putStrLn $ "placeOrderResult: " <> show r) placeOrderResult

  let orderId = maybe 10 id $ either (const Nothing) (S.orderId) (S.mimeResult placeOrderResult)

  -- getOrderByid
  let getOrderByIdRequest = S.getOrderById orderId
  getOrderByIdRequestResult <- S.dispatchMime mgr config getOrderByIdRequest S.MimeJSON
  mapM_ (\r -> putStrLn $ "getOrderById: found order: " <> show r) getOrderByIdRequestResult 

  -- deleteOrder
  let deleteOrderRequest = S.deleteOrder "21"
  _ <- S.dispatchLbs mgr config deleteOrderRequest S.MimeJSON

  return ()



-- * USER

-- this swagger definition doesn't declare what content-type the
-- server actually expects for these operations, so delcare it here
instance S.Consumes S.CreateUser S.MimeJSON
instance S.Consumes S.UpdateUser S.MimeJSON
instance S.Consumes S.CreateUsersWithArrayInput S.MimeJSON
instance S.Consumes S.CreateUsersWithListInput S.MimeJSON

-- similarly we declare these operations are allowed to omit the
-- accept header despite what the swagger definition says
instance S.Produces S.CreateUsersWithArrayInput S.MimeNoContent
instance S.Produces S.CreateUsersWithListInput S.MimeNoContent

runUser :: NH.Manager -> S.SwaggerPetstoreConfig -> IO ()
runUser mgr config = do

  let username = "hsusername"
  -- createUser
  let user = S.mkUser { S.userId = Just 21, S.userUsername = Just username } 
  let createUserRequest = S.createUser S.MimeJSON user
  _ <- S.dispatchLbs mgr config createUserRequest S.MimeJSON

  -- can use lenses (model record names are appended L) to view or modify records
  let users = take 8 $ drop 1 $ iterate (L.over S.userUsernameL (fmap (<> "*")) . L.over S.userIdL (fmap (+ 1))) user
  let createUsersWithArrayInputRequest = S.createUsersWithArrayInput S.MimeJSON users
  _ <- S.dispatchLbs mgr config createUsersWithArrayInputRequest S.MimeNoContent

  -- createUsersWithArrayInput
  let createUsersWithListInputRequest = S.createUsersWithListInput S.MimeJSON users
  _ <- S.dispatchLbs mgr config createUsersWithListInputRequest S.MimeNoContent

  -- getUserByName
  let getUserByNameRequest = S.getUserByName username
  getUserByNameResult <- S.dispatchMime mgr config getUserByNameRequest S.MimeJSON
  mapM_ (\r -> putStrLn $ "getUserByName: found user: " <> show r) getUserByNameResult 

  -- loginUser
  let loginUserRequest = S.loginUser username "password1"
  loginUserResult <- S.dispatchLbs mgr config loginUserRequest S.MimeJSON
  BCL.putStrLn $ "loginUser: " <> (NH.responseBody loginUserResult)

  -- updateUser
  let updateUserRequest = S.updateUser S.MimeJSON username (user { S.userEmail = Just "xyz@example.com" })
  _ <- S.dispatchLbs mgr config updateUserRequest S.MimeJSON

  -- logoutUser
  _ <- S.dispatchLbs mgr config S.logoutUser S.MimeJSON
  
  -- deleteUser
  let deleteUserRequest = S.deleteUser username
  _ <- S.dispatchLbs mgr config deleteUserRequest S.MimeJSON

  return ()
