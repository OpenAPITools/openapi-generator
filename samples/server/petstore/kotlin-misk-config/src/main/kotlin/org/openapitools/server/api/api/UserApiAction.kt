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
import misk.web.actions.WebAction
import misk.web.interceptors.LogRequestResponse
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
import misk.web.Response
import misk.web.ResponseContentType
import misk.web.mediatype.MediaTypes
import org.openapitools.server.api.model.User

/**
* @TODO("Fill out implementation")
*/
@Singleton
class UserApiAction @Inject constructor(
) : WebAction {

    @Post("samplePrefix/user")
    @Description("Create user")
    @RequestContentType(MediaTypes.APPLICATION_JSON)
    @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 2.0)
    @Suppress("unused")
    fun createUser(
        @Valid @RequestBody user: User
    ): Response<Unit> {
        TODO()
    }

    @Post("samplePrefix/user/createWithArray")
    @Description("Creates list of users with given input array")
    @RequestContentType(MediaTypes.APPLICATION_JSON)
    @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 2.0)
    @Suppress("unused")
    fun createUsersWithArrayInput(
        @Valid @RequestBody user: kotlin.collections.List<User>
    ): Response<Unit> {
        TODO()
    }

    @Post("samplePrefix/user/createWithList")
    @Description("Creates list of users with given input array")
    @RequestContentType(MediaTypes.APPLICATION_JSON)
    @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 2.0)
    @Suppress("unused")
    fun createUsersWithListInput(
        @Valid @RequestBody user: kotlin.collections.List<User>
    ): Response<Unit> {
        TODO()
    }

    @Delete("samplePrefix/user/{username}")
    @Description("Delete user")
    @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 2.0)
    @Suppress("unused")
    fun deleteUser(
        @PathParam("username") username: kotlin.String
    ): Response<Unit> {
        TODO()
    }

    @Get("samplePrefix/user/{username}")
    @Description("Get user by user name")
    @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
    @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 2.0)
    @Suppress("unused")
    fun getUserByName(
        @PathParam("username") username: kotlin.String
    ): User {
        TODO()
    }

    @Get("samplePrefix/user/login")
    @Description("Logs user into the system")
    @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
    @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 2.0)
    @Suppress("unused")
    fun loginUser(
         @QueryParam(value = "username") username: kotlin.String, 
         @QueryParam(value = "password") password: kotlin.String
    ): kotlin.String {
        TODO()
    }

    @Get("samplePrefix/user/logout")
    @Description("Logs out current logged in user session")
    @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 2.0)
    @Suppress("unused")
    fun logoutUser(
    ): Response<Unit> {
        TODO()
    }

    @Put("samplePrefix/user/{username}")
    @Description("Updated user")
    @RequestContentType(MediaTypes.APPLICATION_JSON)
    @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 2.0)
    @Suppress("unused")
    fun updateUser(
        @PathParam("username") username: kotlin.String, 
        @Valid @RequestBody user: User
    ): Response<Unit> {
        TODO()
    }
}
