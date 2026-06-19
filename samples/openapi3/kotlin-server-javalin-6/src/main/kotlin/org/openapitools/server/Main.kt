package org.openapitools.server

import io.javalin.Javalin
import io.javalin.apibuilder.ApiBuilder.*

import org.openapitools.server.apis.PetApi
import org.openapitools.server.apis.PetApiServiceImpl
import org.openapitools.server.apis.StoreApi
import org.openapitools.server.apis.StoreApiServiceImpl
import org.openapitools.server.apis.UserApi
import org.openapitools.server.apis.UserApiServiceImpl

fun main() {
    val petApi = PetApi(PetApiServiceImpl())
    val storeApi = StoreApi(StoreApiServiceImpl())
    val userApi = UserApi(UserApiServiceImpl())

    val app = Javalin
        .create { config ->
            config.router.apiBuilder {
                path("/pet") { post(petApi::addPet) }
                path("/pet/{petId}") { delete(petApi::deletePet) }
                path("/pet/findByStatus") { get(petApi::findPetsByStatus) }
                path("/pet/findByTags") { get(petApi::findPetsByTags) }
                path("/pet/{petId}") { get(petApi::getPetById) }
                path("/pet") { put(petApi::updatePet) }
                path("/pet/{petId}") { post(petApi::updatePetWithForm) }
                path("/pet/{petId}/uploadImage") { post(petApi::uploadFile) }

                path("/store/order/{orderId}") { delete(storeApi::deleteOrder) }
                path("/store/inventory") { get(storeApi::getInventory) }
                path("/store/order/{orderId}") { get(storeApi::getOrderById) }
                path("/store/order") { post(storeApi::placeOrder) }

                path("/user") { post(userApi::createUser) }
                path("/user/createWithArray") { post(userApi::createUsersWithArrayInput) }
                path("/user/createWithList") { post(userApi::createUsersWithListInput) }
                path("/user/{username}") { delete(userApi::deleteUser) }
                path("/user/{username}") { get(userApi::getUserByName) }
                path("/user/login") { get(userApi::loginUser) }
                path("/user/logout") { get(userApi::logoutUser) }
                path("/user/{username}") { put(userApi::updateUser) }

            }
        }

    app.start()
}
