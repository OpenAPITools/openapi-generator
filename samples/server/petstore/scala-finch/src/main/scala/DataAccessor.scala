package org.openapitools

// TODO: properly handle custom imports
import java.io._
import java.util.UUID
import java.time._
import com.twitter.finagle.http.exp.Multipart.{FileUpload, InMemoryFileUpload, OnDiskFileUpload}

import org.openapitools.models._

trait DataAccessor {
    // TODO: apiInfo -> apis -> operations = TODO error
    private object TODO extends CommonError("Not implemented") {
        def message = "Not implemented"
    }

        /**
        * 
        * @return A Pet
        */
        def Pet_addPet(pet: Pet): Either[CommonError,Pet] = Left(TODO)

        /**
        * 
        * @return A Unit
        */
        def Pet_deletePet(petId: Long, apiKey: Option[String]): Either[CommonError,Unit] = Left(TODO)

        /**
        * 
        * @return A Seq[Pet]
        */
        def Pet_findPetsByStatus(status: Seq[String]): Either[CommonError,Seq[Pet]] = Left(TODO)

        /**
        * 
        * @return A Seq[Pet]
        */
        def Pet_findPetsByTags(tags: Seq[String]): Either[CommonError,Seq[Pet]] = Left(TODO)

        /**
        * 
        * @return A Pet
        */
        def Pet_getPetById(petId: Long, authParamapi_key: String): Either[CommonError,Pet] = Left(TODO)

        /**
        * 
        * @return A Pet
        */
        def Pet_updatePet(pet: Pet): Either[CommonError,Pet] = Left(TODO)

        /**
        * 
        * @return A Unit
        */
        def Pet_updatePetWithForm(petId: Long, name: Option[String], status: Option[String]): Either[CommonError,Unit] = Left(TODO)

        /**
        * 
        * @return A ApiResponse
        */
        def Pet_uploadFile(petId: Long, additionalMetadata: Option[String], file: FileUpload): Either[CommonError,ApiResponse] = Left(TODO)

        /**
        * 
        * @return A Unit
        */
        def Store_deleteOrder(orderId: String): Either[CommonError,Unit] = Left(TODO)

        /**
        * 
        * @return A Map[String, Int]
        */
        def Store_getInventory(authParamapi_key: String): Either[CommonError,Map[String, Int]] = Left(TODO)

        /**
        * 
        * @return A Order
        */
        def Store_getOrderById(orderId: Long): Either[CommonError,Order] = Left(TODO)

        /**
        * 
        * @return A Order
        */
        def Store_placeOrder(order: Order): Either[CommonError,Order] = Left(TODO)

        /**
        * 
        * @return A Unit
        */
        def User_createUser(user: User, authParamapi_key: String): Either[CommonError,Unit] = Left(TODO)

        /**
        * 
        * @return A Unit
        */
        def User_createUsersWithArrayInput(user: Seq[User], authParamapi_key: String): Either[CommonError,Unit] = Left(TODO)

        /**
        * 
        * @return A Unit
        */
        def User_createUsersWithListInput(user: Seq[User], authParamapi_key: String): Either[CommonError,Unit] = Left(TODO)

        /**
        * 
        * @return A Unit
        */
        def User_deleteUser(username: String, authParamapi_key: String): Either[CommonError,Unit] = Left(TODO)

        /**
        * 
        * @return A User
        */
        def User_getUserByName(username: String): Either[CommonError,User] = Left(TODO)

        /**
        * 
        * @return A String
        */
        def User_loginUser(username: String, password: String): Either[CommonError,String] = Left(TODO)

        /**
        * 
        * @return A Unit
        */
        def User_logoutUser(authParamapi_key: String): Either[CommonError,Unit] = Left(TODO)

        /**
        * 
        * @return A Unit
        */
        def User_updateUser(username: String, user: User, authParamapi_key: String): Either[CommonError,Unit] = Left(TODO)

}