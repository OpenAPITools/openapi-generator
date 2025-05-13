package org.openapitools.server.api.api

import jakarta.inject.Inject
import jakarta.inject.Singleton
import jakarta.validation.Valid
import jakarta.validation.constraints.DecimalMax
import jakarta.validation.constraints.DecimalMin
import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Max
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotNull
import jakarta.validation.constraints.Pattern
import jakarta.validation.constraints.Size
import misk.web.Delete
import misk.web.Description
import misk.web.Get
import misk.web.HttpCall
import misk.web.Patch
import misk.web.PathParam
import misk.web.Post
import misk.web.Put
import misk.web.QueryParam
import misk.web.RequestBody
import misk.web.RequestContentType
import misk.web.RequestHeader
import misk.web.ResponseContentType
import misk.web.mediatype.MediaTypes
import misk.web.actions.WebAction
import org.openapitools.server.api.model.User

/**
* @TODO("Fill out implementation")
*/
@Singleton
class UserApiAction @Inject constructor(
) : WebAction {

    @Post("/user")
    @Description("Create user")
    @RequestContentType(MediaTypes.APPLICATION_JSON)
    
    fun createUser(
        @Valid @RequestBody user: User) {
        TODO()
    }

    @Post("/user/createWithArray")
    @Description("Creates list of users with given input array")
    @RequestContentType(MediaTypes.APPLICATION_JSON)
    
    fun createUsersWithArrayInput(
        @Valid @RequestBody user: kotlin.Array<User>) {
        TODO()
    }

    @Post("/user/createWithList")
    @Description("Creates list of users with given input array")
    @RequestContentType(MediaTypes.APPLICATION_JSON)
    
    fun createUsersWithListInput(
        @Valid @RequestBody user: kotlin.Array<User>) {
        TODO()
    }

    @Delete("/user/{username}")
    @Description("Delete user")
    
    fun deleteUser(
        @PathParam("username") username: kotlin.String) {
        TODO()
    }

    @Get("/user/{username}")
    @Description("Get user by user name")
    @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
    
    fun getUserByName(
        @PathParam("username") username: kotlin.String): User {
        TODO()
    }

    @Get("/user/login")
    @Description("Logs user into the system")
    @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
    
    fun loginUser(
         @QueryParam(value = "username") username: kotlin.String, 
         @QueryParam(value = "password") password: kotlin.String): kotlin.String {
        TODO()
    }

    @Get("/user/logout")
    @Description("Logs out current logged in user session")
    
    fun logoutUser() {
        TODO()
    }

    @Put("/user/{username}")
    @Description("Updated user")
    @RequestContentType(MediaTypes.APPLICATION_JSON)
    
    fun updateUser(
        @PathParam("username") username: kotlin.String, 
        @Valid @RequestBody user: User) {
        TODO()
    }
}
