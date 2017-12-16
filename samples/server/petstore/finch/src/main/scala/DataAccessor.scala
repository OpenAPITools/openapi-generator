package io.swagger

// TODO: properly handle custom imports
import java.io._
import java.util.UUID
import java.time._


import io.swagger.models._

trait DataAccessor {
    // TODO: apiInfo -> apis -> operations = ???
    // NOTE: ??? throws a not implemented exception

        /**
        * 
        * @return A Unit
        */
        def Pet_addPet(body: Pet): Either[CommonError,Unit] = ???

        /**
        * 
        * @return A Unit
        */
        def Pet_deletePet(petId: Long, apiKey: Option[String]): Either[CommonError,Unit] = ???

        /**
        * 
        * @return A Seq[Pet]
        */
        def Pet_findPetsByStatus(status: Seq[String]): Either[CommonError,Seq[Pet]] = ???

        /**
        * 
        * @return A Seq[Pet]
        */
        def Pet_findPetsByTags(tags: Seq[String]): Either[CommonError,Seq[Pet]] = ???

        /**
        * 
        * @return A Pet
        */
        def Pet_getPetById(petId: Long, authParamapi_key: String): Either[CommonError,Pet] = ???

        /**
        * 
        * @return A Unit
        */
        def Pet_updatePet(body: Pet): Either[CommonError,Unit] = ???

        /**
        * 
        * @return A Unit
        */
        def Pet_updatePetWithForm(petId: Long, name: Option[String], status: Option[String]): Either[CommonError,Unit] = ???

        /**
        * 
        * @return A ApiResponse
        */
        def Pet_uploadFile(petId: Long, additionalMetadata: Option[String], file: FileUpload): Either[CommonError,ApiResponse] = ???

        /**
        * 
        * @return A Unit
        */
        def Store_deleteOrder(orderId: String): Either[CommonError,Unit] = ???

        /**
        * 
        * @return A Map[String, Int]
        */
        def Store_getInventory(authParamapi_key: String): Either[CommonError,Map[String, Int]] = ???

        /**
        * 
        * @return A Order
        */
        def Store_getOrderById(orderId: Long): Either[CommonError,Order] = ???

        /**
        * 
        * @return A Order
        */
        def Store_placeOrder(body: Order): Either[CommonError,Order] = ???

        /**
        * 
        * @return A Unit
        */
        def User_createUser(body: User): Either[CommonError,Unit] = ???

        /**
        * 
        * @return A Unit
        */
        def User_createUsersWithArrayInput(body: Seq[User]): Either[CommonError,Unit] = ???

        /**
        * 
        * @return A Unit
        */
        def User_createUsersWithListInput(body: Seq[User]): Either[CommonError,Unit] = ???

        /**
        * 
        * @return A Unit
        */
        def User_deleteUser(username: String): Either[CommonError,Unit] = ???

        /**
        * 
        * @return A User
        */
        def User_getUserByName(username: String): Either[CommonError,User] = ???

        /**
        * 
        * @return A String
        */
        def User_loginUser(username: String, password: String): Either[CommonError,String] = ???

        /**
        * 
        * @return A Unit
        */
        def User_logoutUser(): Either[CommonError,Unit] = ???

        /**
        * 
        * @return A Unit
        */
        def User_updateUser(username: String, body: User): Either[CommonError,Unit] = ???

}