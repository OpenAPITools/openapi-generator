{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-orphans #-}

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Lens.Micro as L
import qualified Network.HTTP.Client as NH

import qualified OpenAPIPetstore as S

import Data.Monoid ((<>))

import System.Environment (getEnvironment)

-- * MAIN

main :: IO ()
main = do

  env <- getEnvironment

  mgr <- NH.newManager NH.defaultManagerSettings

  config0 <- S.withStdoutLogging =<< S.newConfig 

  let config =
        -- configure host
        case lookup "HOST" env of
            Just h -> config0 { S.configHost = BCL.pack h }
            _ -> config0
        -- each configured auth method is only applied to requests that specify them
        `S.addAuthMethod` S.AuthBasicHttpBasicTest "username" "password"
        `S.addAuthMethod` S.AuthApiKeyApiKey "secret-key"
        `S.addAuthMethod` S.AuthApiKeyApiKeyQuery "secret-key"
        `S.addAuthMethod` S.AuthOAuthPetstoreAuth "secret-key"

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

runPet :: NH.Manager -> S.OpenAPIPetstoreConfig -> IO ()
runPet mgr config = do

  -- create the request for addPet, encoded with content-type application/json, with accept header application/json
  let addPetRequest = S.addPet (S.ContentType S.MimeJSON) (S.mkPet "name" ["url1", "url2"])

  -- dispatchLbs simply returns the raw Network.HTTP.Client.Response ByteString
  addPetResponse <- S.dispatchLbs mgr config addPetRequest 

  -- the Consumes & Produces typeclasses control which 'content-type'
  -- and 'accept' encodings are allowed for each operation
  -- -- No instance for (S.Produces S.AddPet S.MimePlainText)
  -- addPetResponse <- S.dispatchLbs mgr config addPetRequest

  -- inspect the AddPet type to see typeclasses indicating wihch
  -- content-type and accept types (mimeTypes) are valid

  -- :i S.AddPet
    -- data S.AddPet 	-- Defined in ‘OpenAPIPetstore.API’
    -- instance S.Produces S.AddPet S.MimeXML
    --   -- Defined in ‘OpenAPIPetstore.API’
    -- instance S.Produces S.AddPet S.MimeJSON
    --   -- Defined in ‘OpenAPIPetstore.API’
    -- instance S.Consumes S.AddPet S.MimeXML
    --   -- Defined in ‘OpenAPIPetstore.API’
    -- instance S.Consumes S.AddPet S.MimeJSON
    --   -- Defined in ‘OpenAPIPetstore.API’
    -- instance S.HasBodyParam S.AddPet S.Pet
    --   -- Defined in ‘OpenAPIPetstore.API’

  
  -- since this openapi definition has no response schema defined for
  -- the 'addPet' response, we decode the response bytestring manually
  case A.eitherDecode (NH.responseBody addPetResponse) of
    Right pet@S.Pet { S.petId = Just pid } -> do

        -- create the request for getPetById
        let getPetByIdRequest = S.getPetById (S.Accept S.MimeJSON) (S.PetId pid)

        -- dispatchMime returns MimeResult, which includes the
        -- expected decoded model object 'Pet', or a parse failure
        getPetByIdRequestResult <- S.dispatchMime mgr config getPetByIdRequest
        case S.mimeResult getPetByIdRequestResult of
          Left (S.MimeError _ _) -> return () -- parse error, already displayed in the log
          Right r -> putStrLn $ "getPetById: found pet: " <> show r  -- display 'Pet' model object, r 

        -- findPetsByStatus
        let findPetsByStatusRequest = S.findPetsByStatus (S.Accept S.MimeJSON)
                                                         (S.Status [ S.E'Status2'Available
                                                                   , S.E'Status2'Pending
                                                                   , S.E'Status2'Sold])
        findPetsByStatusResult <- S.dispatchMime mgr config findPetsByStatusRequest
        mapM_ (\r -> putStrLn $ "findPetsByStatus: found " <> (show . length) r <> " pets") findPetsByStatusResult 

        -- findPetsByTags
        let findPetsByTagsRequest = S.findPetsByTags (S.Accept S.MimeJSON) (S.Tags ["name","tag1"])
        findPetsByTagsResult <- S.dispatchMime mgr config findPetsByTagsRequest
        mapM_ (\r -> putStrLn $ "findPetsByTags: found " <> (show . length) r <> " pets") findPetsByTagsResult 

        -- updatePet
        let updatePetRequest = S.updatePet (S.ContentType S.MimeJSON) $ pet
                { S.petStatus   = Just S.E'Status2'Available
                , S.petCategory = Just (S.Category (Just 3) "catname")
                }
        _ <- S.dispatchLbs mgr config updatePetRequest

        -- requred parameters are included as function arguments, optional parameters are included with applyOptionalParam
        -- inspect the UpdatePetWithForm type to see typeclasses indicating optional paramteters (:i S.UpdatePetWithForm)
        -- instance S.HasOptionalParam S.UpdatePetWithForm S.Name
        --   -- Defined in ‘OpenAPIPetstore.API’
        -- instance S.HasOptionalParam S.UpdatePetWithForm S.Status
        --   -- Defined in ‘OpenAPIPetstore.API’
        let updatePetWithFormRequest = S.updatePetWithForm (S.PetId pid)
                `S.applyOptionalParam` S.Name2 "petName"
                `S.applyOptionalParam` S.StatusText "pending"
        _ <- S.dispatchLbs mgr config updatePetWithFormRequest 

        -- multipart/form-data file uploads are just a different content-type
        let uploadFileRequest = S.uploadFile (S.PetId pid)
                `S.applyOptionalParam` S.File2 "package.yaml" -- the file contents of the path are read when dispatched
                `S.applyOptionalParam` S.AdditionalMetadata "a package.yaml file"
        uploadFileRequestResult <- S.dispatchMime mgr config uploadFileRequest 
        mapM_ (\r -> putStrLn $ "uploadFile: " <> show r) uploadFileRequestResult 

        -- deletePet
        let deletePetRequest = S.deletePet (S.PetId pid)
                `S.applyOptionalParam` S.ApiKey "api key"
        _ <- S.dispatchLbs mgr config deletePetRequest 

        return ()

    Left e -> putStrLn e
    _ -> putStrLn "no Pet id returned"

  return ()



-- * STORE

runStore :: NH.Manager -> S.OpenAPIPetstoreConfig -> IO ()
runStore mgr config = do

  -- we can set arbitrary headers with setHeader
  let getInventoryRequest = S.getInventory
        `S.setHeader` [("random-header","random-value")] 
  getInventoryRequestRequestResult <- S.dispatchMime mgr config getInventoryRequest
  mapM_ (\r -> putStrLn $ "getInventoryRequest: found " <> (show . length) r <> " results") getInventoryRequestRequestResult

  -- placeOrder
  now <- TI.getCurrentTime
  let placeOrderRequest = S.placeOrder (S.ContentType S.MimeJSON) (S.Accept S.MimeJSON) (S.mkOrder { S.orderId = Just 21, S.orderQuantity = Just 210, S.orderShipDate = Just (S.DateTime now)})
  placeOrderResult <- S.dispatchMime mgr config placeOrderRequest
  mapM_ (\r -> putStrLn $ "placeOrderResult: " <> show r) placeOrderResult

  let orderId = maybe 10 id $ either (const Nothing) (S.orderId) (S.mimeResult placeOrderResult)

  -- getOrderByid
  let getOrderByIdRequest = S.getOrderById (S.Accept S.MimeJSON) (S.OrderId orderId)
  getOrderByIdRequestResult <- S.dispatchMime mgr config getOrderByIdRequest
  mapM_ (\r -> putStrLn $ "getOrderById: found order: " <> show r) getOrderByIdRequestResult 

  -- deleteOrder
  let deleteOrderRequest = S.deleteOrder (S.OrderIdText "21")
  _ <- S.dispatchLbs mgr config deleteOrderRequest 

  return ()



-- * USER

runUser :: NH.Manager -> S.OpenAPIPetstoreConfig -> IO ()
runUser mgr config = do

  let username = "hsusername"
  -- createUser
  let user = S.mkUser { S.userId = Just 21, S.userUsername = Just username } 
  let createUserRequest = S.createUser (S.ContentType S.MimeJSON) user
  _ <- S.dispatchLbs mgr config createUserRequest

  -- can use lenses (model record names are appended L) to view or modify records
  let users = take 8 $ drop 1 $ iterate (L.over S.userUsernameL (fmap (<> "*")) . L.over S.userIdL (fmap (+ 1))) user
  let createUsersWithArrayInputRequest = S.createUsersWithArrayInput (S.ContentType S.MimeJSON) (S.Body users)
  _ <- S.dispatchLbs mgr config createUsersWithArrayInputRequest 

  -- createUsersWithArrayInput
  let createUsersWithListInputRequest = S.createUsersWithListInput (S.ContentType S.MimeJSON) (S.Body users)
  _ <- S.dispatchLbs mgr config createUsersWithListInputRequest

  -- getUserByName
  let getUserByNameRequest = S.getUserByName (S.Accept S.MimeJSON) (S.Username username)
  getUserByNameResult <- S.dispatchMime mgr config getUserByNameRequest
  mapM_ (\r -> putStrLn $ "getUserByName: found user: " <> show r) getUserByNameResult 

  -- loginUser
  let loginUserRequest = S.loginUser (S.Accept S.MimeJSON) (S.Username username) (S.Password "password1")
  loginUserResult <- S.dispatchLbs mgr config loginUserRequest
  BCL.putStrLn $ "loginUser: " <> (NH.responseBody loginUserResult)

  -- updateUser
  let updateUserRequest = S.updateUser (S.ContentType S.MimeJSON) (user { S.userEmail = Just "xyz@example.com" }) (S.Username username) 
  _ <- S.dispatchLbs mgr config updateUserRequest

  -- logoutUser
  _ <- S.dispatchLbs mgr config (S.logoutUser)
  
  -- deleteUser
  let deleteUserRequest = S.deleteUser (S.Username username)
  _ <- S.dispatchLbs mgr config deleteUserRequest 

  return ()
