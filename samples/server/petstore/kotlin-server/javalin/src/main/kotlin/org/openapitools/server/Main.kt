package org.openapitools.server

import io.javalin.Javalin
import io.javalin.community.routing.dsl.routing

import org.openapitools.server.apis.PetApi
import org.openapitools.server.apis.PetApiServiceImpl
import org.openapitools.server.apis.StoreApi
import org.openapitools.server.apis.StoreApiServiceImpl
import org.openapitools.server.apis.UserApi
import org.openapitools.server.apis.UserApiServiceImpl

fun main() {
    val PetApi = PetApi(PetApiServiceImpl())
    val StoreApi = StoreApi(StoreApiServiceImpl())
    val UserApi = UserApi(UserApiServiceImpl())

    val app = Javalin
        .create { config ->
            config.routing {
                post("/pet", PetApi::addPet)
                delete("/pet/{petId}", PetApi::deletePet)
                get("/pet/findByStatus", PetApi::findPetsByStatus)
                get("/pet/findByTags", PetApi::findPetsByTags)
                get("/pet/{petId}", PetApi::getPetById)
                put("/pet", PetApi::updatePet)
                post("/pet/{petId}", PetApi::updatePetWithForm)
                post("/pet/{petId}/uploadImage", PetApi::uploadFile)

                delete("/store/order/{orderId}", StoreApi::deleteOrder)
                get("/store/inventory", StoreApi::getInventory)
                get("/store/order/{orderId}", StoreApi::getOrderById)
                post("/store/order", StoreApi::placeOrder)

                post("/user", UserApi::createUser)
                post("/user/createWithArray", UserApi::createUsersWithArrayInput)
                post("/user/createWithList", UserApi::createUsersWithListInput)
                delete("/user/{username}", UserApi::deleteUser)
                get("/user/{username}", UserApi::getUserByName)
                get("/user/login", UserApi::loginUser)
                get("/user/logout", UserApi::logoutUser)
                put("/user/{username}", UserApi::updateUser)

            }
        }

    app.start()
}
