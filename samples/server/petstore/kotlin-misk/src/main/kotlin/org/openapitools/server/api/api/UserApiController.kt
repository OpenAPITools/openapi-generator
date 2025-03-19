package org.openapitools.server.api.api

import org.openapitools.server.api.model.User


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

    @Singleton
    class UserApiController @Inject constructor(
        // @TODO("camelCase this")
        private val UserApi : UserApi
    ) : UserApi, WebAction {

        @Post("/user")
        @Description("Create user")
        @RequestContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun createUser(@RequestBody user: User) =
            UserApi.createUser(user)

        @Post("/user/createWithArray")
        @Description("Creates list of users with given input array")
        @RequestContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun createUsersWithArrayInput(@RequestBody user: kotlin.Array<User>) =
            UserApi.createUsersWithArrayInput(user)

        @Post("/user/createWithList")
        @Description("Creates list of users with given input array")
        @RequestContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun createUsersWithListInput(@RequestBody user: kotlin.Array<User>) =
            UserApi.createUsersWithListInput(user)

        @Delete("/user/{username}")
        @Description("Delete user")
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun deleteUser(@PathParam("username") username: kotlin.String) =
            UserApi.deleteUser(username)

        @Get("/user/{username}")
        @Description("Get user by user name")
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun getUserByName(@PathParam("username") username: kotlin.String) =
            UserApi.getUserByName(username)

        @Get("/user/login")
        @Description("Logs user into the system")
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun loginUser(@QueryParam username: kotlin.String,@QueryParam password: kotlin.String) =
            UserApi.loginUser(username, password)

        @Get("/user/logout")
        @Description("Logs out current logged in user session")
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun logoutUser() =
            UserApi.logoutUser()

        @Put("/user/{username}")
        @Description("Updated user")
        @RequestContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun updateUser(@PathParam("username") username: kotlin.String,@RequestBody user: User) =
            UserApi.updateUser(username, user)
    }
