package org.openapitools.server.api.api

import org.openapitools.server.api.model.User

    import javax.validation.Valid
    import javax.validation.constraints.DecimalMax
    import javax.validation.constraints.DecimalMin
    import javax.validation.constraints.Email
    import javax.validation.constraints.Max
    import javax.validation.constraints.Min
    import javax.validation.constraints.NotNull
    import javax.validation.constraints.Pattern
    import javax.validation.constraints.Size

import jakarta.inject.Inject
import jakarta.inject.Singleton

// TODO("Only import what we need")
import misk.web.Delete
import misk.web.Description
import misk.web.Get
import misk.web.HttpCall
import misk.web.PathParam
import misk.web.Post
import misk.web.Put
import misk.web.QueryParam
import misk.web.RequestBody
import misk.web.RequestContentType
import misk.web.RequestHeaders
import misk.web.ResponseContentType
import misk.web.actions.WebAction
import misk.web.interceptors.LogRequestResponse
import misk.web.mediatype.MediaTypes
import okhttp3.Headers

    @Validated
    @Singleton
    class UserApiController @Inject constructor(
        private val userApi: UserApi
    ) : UserApi, WebAction {

        @Post("/user")
        @Description("Create user")
        @RequestContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        override fun createUser(@RequestBody user: User) =
            userApi.createUser(user)

        @Post("/user/createWithArray")
        @Description("Creates list of users with given input array")
        @RequestContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        override fun createUsersWithArrayInput(@RequestBody user: kotlin.Array<User>) =
            userApi.createUsersWithArrayInput(user)

        @Post("/user/createWithList")
        @Description("Creates list of users with given input array")
        @RequestContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        override fun createUsersWithListInput(@RequestBody user: kotlin.Array<User>) =
            userApi.createUsersWithListInput(user)

        @Delete("/user/{username}")
        @Description("Delete user")
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        override fun deleteUser(@PathParam("username") username: kotlin.String) =
            userApi.deleteUser(username)

        @Get("/user/{username}")
        @Description("Get user by user name")
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        override fun getUserByName(@PathParam("username") username: kotlin.String) =
            userApi.getUserByName(username)

        @Get("/user/login")
        @Description("Logs user into the system")
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        override fun loginUser(@QueryParam username: kotlin.String,@QueryParam password: kotlin.String) =
            userApi.loginUser(username, password)

        @Get("/user/logout")
        @Description("Logs out current logged in user session")
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        override fun logoutUser() =
            userApi.logoutUser()

        @Put("/user/{username}")
        @Description("Updated user")
        @RequestContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        override fun updateUser(@PathParam("username") username: kotlin.String,@RequestBody user: User) =
            userApi.updateUser(username, user)
    }
