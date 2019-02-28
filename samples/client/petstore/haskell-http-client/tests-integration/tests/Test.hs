{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-deprecations -fno-warn-unused-matches -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-orphans #-}

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Lens.Micro as L
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Types.Status as NH

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.HUnit.Lang

import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.Environment (getEnvironment)

import qualified OpenAPIPetstore as S

import Data.Monoid ((<>))

-- * UTILS

assertSuccess :: Expectation
assertSuccess = Success `shouldBe` Success

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
            _ -> config0 { S.configHost = "http://0.0.0.0/v2" }
        -- each configured auth method is only applied to requests that specify them
        `S.addAuthMethod` S.AuthBasicHttpBasicTest "username" "password"
        `S.addAuthMethod` S.AuthApiKeyApiKey "secret-key"
        `S.addAuthMethod` S.AuthApiKeyApiKeyQuery "secret-key"
        `S.addAuthMethod` S.AuthOAuthPetstoreAuth "secret-key"

  putStrLn "\n******** CONFIG ********"
  putStrLn (show config)


  hspec $ do
    testPetOps mgr config
    testStoreOps mgr config
    testUserOps mgr config

-- * PET TESTS

testPetOps :: NH.Manager -> S.OpenAPIPetstoreConfig -> Spec
testPetOps mgr config = 

  describe "** pet operations" $ do

    _pet <- runIO $ newIORef (Nothing :: Maybe S.Pet)

    it "addPet" $ do
      let addPetRequest =
             S.addPet (S.ContentType S.MimeJSON) (S.mkPet "name" ["url1", "url2"])
      addPetResponse <- S.dispatchLbs mgr config addPetRequest
      NH.responseStatus addPetResponse `shouldBe` NH.status200
      case A.eitherDecode (NH.responseBody addPetResponse) of
        Right pet -> do
          _pet `writeIORef` Just pet 
          assertSuccess
        Left e -> assertFailure e

    around (\go ->
      readIORef _pet >>= \case
        Just pet@S.Pet {S.petId = Just petId} -> go (petId, pet)
        _ -> pendingWith "no petId") $
      it "getPetById" $ \(petId, pet) -> do
        let getPetByIdRequest = S.getPetById (S.Accept S.MimeJSON) (S.PetId petId)
        getPetByIdRequestResult <- S.dispatchMime mgr config getPetByIdRequest 
        NH.responseStatus (S.mimeResultResponse getPetByIdRequestResult) `shouldBe` NH.status200
        case S.mimeResult getPetByIdRequestResult of
          Right p -> p `shouldBe` pet
          Left (S.MimeError e _) -> assertFailure e

    it "findPetsByStatus" $ do
      let findPetsByStatusRequest = S.findPetsByStatus (S.Accept S.MimeJSON)
                                        (S.Status [ S.E'Status2'Available
                                                  , S.E'Status2'Pending
                                                  , S.E'Status2'Sold])
      findPetsByStatusResult <- S.dispatchMime mgr config findPetsByStatusRequest
      NH.responseStatus (S.mimeResultResponse findPetsByStatusResult) `shouldBe` NH.status200
      case S.mimeResult findPetsByStatusResult of
        Right r -> length r `shouldSatisfy` (> 0)
        Left (S.MimeError e _) -> assertFailure e
      
    it "findPetsByTags" $ do
      let findPetsByTagsRequest = S.findPetsByTags (S.Accept S.MimeJSON) (S.Tags ["name","tag1"])
      findPetsByTagsResult <- S.dispatchMime mgr config findPetsByTagsRequest
      NH.responseStatus (S.mimeResultResponse findPetsByTagsResult) `shouldBe` NH.status200
      case S.mimeResult findPetsByTagsResult of
        Right r -> length r `shouldSatisfy` (> 0)
        Left (S.MimeError e _) -> assertFailure e

    around (\go ->
      readIORef _pet >>= \case
        Just pet -> go pet
        _ -> pendingWith "no pet") $
      it "updatePet" $ \pet -> do
        let updatePetRequest = S.updatePet (S.ContentType S.MimeJSON)
              (pet
                 { S.petStatus   = Just S.E'Status2'Available
                 , S.petCategory = Just (S.Category (Just 3) "catname")
                 })
        updatePetResponse <- S.dispatchLbs mgr config updatePetRequest 
        NH.responseStatus updatePetResponse `shouldBe` NH.status200

    it "updatePetWithFormRequest" $ do
      readIORef _pet >>= \case
        Just S.Pet {S.petId = Just petId} -> do
          let updatePetWithFormRequest = S.updatePetWithForm
                (S.PetId petId)
                `S.applyOptionalParam` S.Name2 "petName"
                `S.applyOptionalParam` S.StatusText "pending"
          updatePetWithFormResponse <- S.dispatchLbs mgr config updatePetWithFormRequest 
          NH.responseStatus updatePetWithFormResponse `shouldBe` NH.status200
        _ -> pendingWith "no pet"

    around (\go ->
      readIORef _pet >>= \case
        Just pet@S.Pet {S.petId = Just petId} -> go petId
        _ -> pendingWith "no petId") $
      it "uploadFile" $ \petId -> do
          let uploadFileRequest = S.uploadFile (S.PetId petId)
                  `S.applyOptionalParam` S.File2 "package.yaml"
                  `S.applyOptionalParam` S.AdditionalMetadata "a package.yaml file"
          uploadFileRequestResult <- S.dispatchMime mgr config uploadFileRequest 
          NH.responseStatus (S.mimeResultResponse uploadFileRequestResult) `shouldBe` NH.status200
          case S.mimeResult uploadFileRequestResult of
              Right _ -> assertSuccess
              Left (S.MimeError e _) -> assertFailure e

    around (\go ->
      readIORef _pet >>= \case
        Just pet@S.Pet {S.petId = Just petId} -> go petId
        _ -> pendingWith "no petId") $
      it "deletePet" $ \petId -> do
        let deletePetRequest = S.deletePet (S.PetId petId)
                  `S.applyOptionalParam` S.ApiKey "api key"
        deletePetResponse <- S.dispatchLbs mgr config deletePetRequest
        NH.responseStatus deletePetResponse `shouldBe` NH.status200

-- * STORE TESTS
  
testStoreOps :: NH.Manager -> S.OpenAPIPetstoreConfig -> Spec
testStoreOps mgr config = do

  describe "** store operations" $ do

    _order <- runIO $ newIORef (Nothing :: Maybe S.Order)

    it "getInventory" $ do
      let getInventoryRequest = S.getInventory
            `S.setHeader` [("api_key","special-key")] 
      getInventoryRequestRequestResult <- S.dispatchMime mgr config getInventoryRequest 
      NH.responseStatus (S.mimeResultResponse getInventoryRequestRequestResult) `shouldBe` NH.status200
      case S.mimeResult getInventoryRequestRequestResult of
          Right r -> length r `shouldSatisfy` (> 0)
          Left (S.MimeError e _) -> assertFailure e

    it "placeOrder" $ do
      now <- TI.getCurrentTime
      let placeOrderRequest = S.placeOrder (S.ContentType S.MimeJSON) (S.Accept S.MimeJSON)
            (S.mkOrder
             { S.orderId = Just 21
             , S.orderQuantity = Just 210
             , S.orderShipDate = Just (S.DateTime now)
             })
      placeOrderResult <- S.dispatchMime mgr config placeOrderRequest 
      NH.responseStatus (S.mimeResultResponse placeOrderResult) `shouldBe` NH.status200
      case S.mimeResult placeOrderResult of
          Right order -> do
            _order `writeIORef` Just order 
            assertSuccess
          Left (S.MimeError e _) -> assertFailure e

    around (\go ->
      readIORef _order >>= \case
        Just order@S.Order {S.orderId = Just orderId} -> go (orderId, order)
        _ -> pendingWith "no orderId") $
      it "getOrderById" $ \(orderId, order) -> do
        let getOrderByIdRequest = S.getOrderById  (S.Accept S.MimeJSON) (S.OrderId orderId)
        getOrderByIdRequestResult <- S.dispatchMime mgr config getOrderByIdRequest
        NH.responseStatus (S.mimeResultResponse getOrderByIdRequestResult) `shouldBe` NH.status200
        case S.mimeResult getOrderByIdRequestResult of
            Right o -> o `shouldBe` order
            Left (S.MimeError e _) -> assertFailure e

    around (\go ->
      readIORef _order >>= \case
        Just S.Order {S.orderId = Just orderId} -> go (T.pack (show orderId))
        _ -> pendingWith "no orderId") $
      it "deleteOrder" $ \orderId -> do
        let deleteOrderRequest = S.deleteOrder (S.OrderIdText orderId)
        deleteOrderResult <- S.dispatchLbs mgr config deleteOrderRequest 
        NH.responseStatus deleteOrderResult `shouldBe` NH.status200


-- * USER TESTS

testUserOps :: NH.Manager -> S.OpenAPIPetstoreConfig -> Spec
testUserOps mgr config = do

  describe "** user operations" $ do

    let _username = "hsusername"
        _password = "password1"
        _user =
          S.mkUser
          { S.userId = Just 21
          , S.userUsername = Just _username
          , S.userEmail = Just "xyz@example.com"
          , S.userUserStatus = Just 0
          }
        _users =
          take 8 $
          drop 1 $
          iterate
            (L.over (S.userUsernameL . L._Just) (<> "*") .
             L.over (S.userIdL . L._Just) (+ 1))
            _user

    before (pure _user) $
      it "createUser" $ \user -> do
        let createUserRequest = S.createUser (S.ContentType S.MimeJSON) user
        createUserResult <- S.dispatchLbs mgr config createUserRequest 
        NH.responseStatus createUserResult `shouldBe` NH.status200

    before (pure _users) $
      it "createUsersWithArrayInput" $ \users -> do
        let createUsersWithArrayInputRequest = S.createUsersWithArrayInput (S.ContentType S.MimeJSON) (S.Body users)
        createUsersWithArrayInputResult <- S.dispatchLbs mgr config createUsersWithArrayInputRequest
        NH.responseStatus createUsersWithArrayInputResult `shouldBe` NH.status200

    before (pure _users) $
      it "createUsersWithListInput" $ \users -> do
        let createUsersWithListInputRequest = S.createUsersWithListInput (S.ContentType S.MimeJSON) (S.Body users)
        createUsersWithListInputResult <- S.dispatchLbs mgr config createUsersWithListInputRequest 
        NH.responseStatus createUsersWithListInputResult `shouldBe` NH.status200

    before (pure (_username, _user)) $
      it "getUserByName" $ \(username, user) -> do
        let getUserByNameRequest = S.getUserByName (S.Accept S.MimeJSON) (S.Username username)
        getUserByNameResult <- S.dispatchMime mgr config getUserByNameRequest 
        NH.responseStatus (S.mimeResultResponse getUserByNameResult) `shouldBe` NH.status200
        case S.mimeResult getUserByNameResult of
          Right u -> u `shouldBe` user
          Left (S.MimeError e _) -> assertFailure e

    before (pure (_username, _password)) $
      it "loginUser" $ \(username, password) -> do
        let loginUserRequest = S.loginUser (S.Accept S.MimeJSON) (S.Username username) (S.Password password)
        loginUserResult <- S.dispatchLbs mgr config loginUserRequest
        NH.responseStatus loginUserResult `shouldBe` NH.status200

    before (pure (_username, _user)) $
      it "updateUser" $ \(username, user) -> do
        let updateUserRequest = S.updateUser (S.ContentType S.MimeJSON) user (S.Username username) 
        updateUserResult <- S.dispatchLbs mgr config updateUserRequest
        NH.responseStatus updateUserResult `shouldBe` NH.status200

    it "logoutuser" $ do
        logoutUserResult <- S.dispatchLbs mgr config S.logoutUser
        NH.responseStatus logoutUserResult `shouldBe` NH.status200

    before (pure _username) $
      it "deleteUser" $ \username -> do
        let deleteUserRequest = S.deleteUser (S.Username username)
        deleteUserResult <- S.dispatchLbs mgr config deleteUserRequest 
        NH.responseStatus deleteUserResult `shouldBe` NH.status200
